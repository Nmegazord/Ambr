#' Ambr Map
#'
#' Gives a map of the ambr with culture stations, vessels ids and vessel numbers according to standards
#'
#' @return A dataframe with the map
#'
#' @importFrom rlang .data
ambr_map <- function() {
  map <- dplyr::data_frame(culture_station = c(rep("CS1", 12), rep("CS2", 12), rep("CS3", 12), rep("CS4", 12)),
                    vessel_number = rep(1:12, 4),
                    vessel_id = 1:48)
  map <- map |>
    dplyr::mutate(
      vessel_number = as.factor(.data$vessel_number),
      culture_station = as.factor(.data$culture_station),
      vessel_id = as.factor(.data$vessel_id)
    )
  return(map)
}

#' Append map
#'
#' Appends missing mapping values to the dataframe
#'
#' @param data the data to append the map to
#' @param append_by the .data$parameter, generally c("culture_station", "vessel_number")
#'
#' @return A stitch of the dataframe with the map of the positions
#'
append_map <- function(data, append_by) {
  x <- data |>
    dplyr::left_join(ambr_map(), by = append_by)
  return(x)
}

# Import ambr data
#'
#' Main function of the package, imports all ambr data in a frame
#'
#' @param path the path of the AuditData folder
#' @param schema a schema in csv format with clone and generation as additional parameters
#'
#' @return A list of dataframe with all the parameters for the ambr
#' @export
#'
import_ambr_audit_data <- function(path = "AuditData", schema = F) {
  # Generates a list of files within the AuditData folder that end in "cv.csv"
  path = "AuditData"
  file_list <- list.files(
    path = path,
    pattern = "*cv.csv",
    full.names = T,
    recursive = T
  )

  # Imports the files
  df <- dplyr::bind_rows(purrr::map2(
    purrr::map(
      file_list,
      data.table::fread,
      sep = ",",
      header = TRUE,
      drop = 1
    ),
    file_list,
    cbind
  ))

  # Dropping the vessels that were not inoculated
  inoculated_vessels <- df |>
    dplyr::filter(.data$Name == "INOCULATED") |>
    dplyr::select(.data$V2) |>
    dplyr::pull()

  df <- df |>
    dplyr::filter(.data$V2 %in% inoculated_vessels)

  # gets rid of all date_something rows (redundant)
  df <- df |>
    dplyr::filter(!grepl("DATE", .data$Name))

  # Extracting the culture station and vessel number from the filename
  df <- df |>
    dplyr::mutate(culture_station = as.factor(
      stringr::str_extract(.data$V2, pattern = "(?<=^AuditData/CS[1234]/)CS[1234](?=_[[:digit:]]{1,2}_cv.csv$)")
    ),
    vessel_number = as.factor(as.numeric(
      stringr::str_extract(.data$V2, pattern = "(?<=^AuditData/CS[1234]/CS[1234]_)[[:digit:]]{1,2}(?=_cv.csv$)")
    ))) |>
    dplyr::select(-.data$V2)

  # Clean dplyr::rename and factor data
  df <- df |>
    janitor::clean_names() |>
    dplyr::rename(parameter = .data$name) |>
    dplyr::mutate(
      parameter = tolower(.data$parameter),
      parameter = as.factor(.data$parameter),
      date_time = lubridate::dmy_hms(.data$date_time)
    )

  # Imports map and stitches it to the dataframe
  df <- append_map(df, append_by = c("culture_station", "vessel_number"))

  # Calculates time_from_inoculated
  when_inoculated <- df |>
    dplyr::filter(.data$parameter == "inoculated") |>
    dplyr::select(.data$value, .data$vessel_id) |>
    dplyr::rename(time_of_inoculum = .data$value)

  df <- df |>
    dplyr::left_join(when_inoculated, by = c("vessel_id")) |>
    dplyr::mutate(
      time_from_inoculation = difftime(.data$date_time, lubridate::dmy_hms(.data$time_of_inoculum), units="hours"),
      time_from_inoculation = as.numeric(.data$time_from_inoculation)) |>
    dplyr::select(-.data$time_of_inoculum)

  # adds schema if available
  if (schema != F) {
    schema <- data.table::fread(schema) |>
      dplyr::mutate(
        vessel_number = as.numeric(.data$vessel_number),
        vessel_number = as.factor(.data$vessel_number),
        culture_station = as.factor(.data$culture_station)
      )

    df <- df |>
      dplyr::left_join(schema, by = c("culture_station", "vessel_number")
      ) |>
      dplyr::mutate(clone = as.factor(.data$clone),
             generation = as.factor(.data$generation),
             clone_generation_id = as.factor(.data$clone_generation_id))
  }

  # split the data frame by '.data$parameter' and attempt to guess each value format
  df_split <- split(df, df$.data$parameter) |>
    lapply(\(x) x |> dplyr::mutate(value = readr::parse_guess(.data$value)))

  # the date data have a non-standard format and need special handling
  df_split$inoculated <- df_split$inoculated |>
    dplyr::mutate(value = lubridate::dmy_hms(.data$value))

  return(df_split)
}

#  Import transfection time
#'
#' Function to import transfection time from process audit
#'
#' @param path the location of ProcessAudit.csv
#'
#' @return a dataframe with transfection timepoints in standard format
#' @export
#'
import_transfection_time <- function(path = "AuditData/ProcessAudit.csv") {
  process_audit <-
    readr::read_csv(file =path,
             col_names = F,
             skip = 1,
             show_col_types = FALSE)

  # Data cleaning
  process_audit <- process_audit |>
    dplyr::mutate(X1 = gsub(';', ',', process_audit$X1),
           X1 = stringr::str_sub(.data$X1, 2, -1)) |>
    tidyr::separate(
      col = .data$X1,
      into = c(
        "date_time",
        "action",
        "value",
        "source_location",
        "source_id",
        "source_well",
        "target_location",
        "target_id",
        "target_well",
        "extra_info"
      ),
      sep = ","
    ) |>
    dplyr::rename(vessel_number = .data$target_well,
           culture_station = .data$target_id) |>
    dplyr::filter(.data$culture_station %in% c("CS1", "CS2", "CS3", "CS4")) |>
    dplyr::mutate(
      vessel_number = as.numeric(.data$vessel_number),
      culture_station = as.factor(.data$culture_station),
      vessel_number = as.factor(.data$vessel_number),
      date_time = lubridate::dmy_hms(.data$date_time)
    )

  # Imports map and stitches it to the dataframe
  process_audit <- append_map(process_audit, append_by = c("culture_station", "vessel_number"))

  # Extract transfection time from process_audit and save it in a dataframe
  transfection_time <- process_audit |>
    dplyr::filter(
      .data$source_id %in% c(
        "Transfection Plate 1 CS1",
        "Transfection Plate 2 CS2",
        "Transfection Plate 3 CS3",
        "Transfection Plate 4 CS4"
      )
    ) |>
    dplyr::select(.data$date_time, .data$culture_station, .data$vessel_number, .data$vessel_id)
  return(transfection_time)

}
