packages <- c("readr", "dplyr", "httr")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(readr)
library(dplyr)
library(httr)

output_dir <- path.expand("'/Users/kenulfernando/Desktop/Uwin/Second_Sem/Data Acquisition /Ontario_Extreme_Weather_Electricity/data/raw/weather'")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

years <- 2019:2025
stations <- data.frame(
  station = c("London A", "Ottawa Intl A", "Sudbury A", "Thunder Bay A",
              "Toronto Intl A", "Windsor A"),
  prefix = c("614", "610", "606", "604", "615", "613"),
  stringsAsFactors = FALSE
)

# Get the official station inventory. The first three lines are metadata.
inventory_url <- paste0(
  "https://collaboration.cmc.ec.gc.ca/cmc/climate/",
  "Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv"
)
inventory_file <- file.path(output_dir, "Station_Inventory_EN.csv")
if (!file.exists(inventory_file) || file.size(inventory_file) == 0) {
  download.file(inventory_url, inventory_file, mode = "wb")
}
inventory <- read_csv(inventory_file, skip = 3, show_col_types = FALSE,
                      locale = locale(encoding = "UTF-8"))

required_cols <- c("Name", "Climate ID", "Station ID", "DLY First Year", "DLY Last Year")
if (!all(required_cols %in% names(inventory))) {
  stop("Unexpected inventory columns. Found: ", paste(names(inventory), collapse = ", "),
       ". Check the official inventory CSV format.")
}

# Find matching station records for each year (IDs sometimes change over time).
inventory <- inventory %>%
  mutate(
    station_name_upper = toupper(trimws(as.character(.data[["Name"]]))),
    climate_id_text = as.character(.data[["Climate ID"]]),
    daily_first = suppressWarnings(as.integer(.data[["DLY First Year"]])),
    daily_last = suppressWarnings(as.integer(.data[["DLY Last Year"]])),
    station_id_num = suppressWarnings(as.integer(.data[["Station ID"]]))
  )

log_rows <- list()
selection_rows <- list()
log_i <- 0L
selection_i <- 0L

for (i in seq_len(nrow(stations))) {
  station_name <- stations$station[i]
  prefix <- stations$prefix[i]
  folder <- file.path(output_dir, station_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  for (year in years) {
    matches <- inventory %>%
      filter(station_name_upper == toupper(station_name),
             startsWith(climate_id_text, prefix),
             !is.na(station_id_num), !is.na(daily_first), !is.na(daily_last),
             daily_first <= year, daily_last >= year) %>%
      arrange(desc(daily_last), daily_first)

    if (nrow(matches) == 0) {
      message("NO MATCH: ", station_name, " ", year,
              " (check station name or availability in inventory)")
      log_i <- log_i + 1L
      log_rows[[log_i]] <- data.frame(Station = station_name, Year = year,
                                      Station_ID = NA_integer_, Climate_ID = NA_character_,
                                      Status = "No matching daily station record")
      next
    }

    selected <- matches[1, ]
    sid <- selected$station_id_num
    climate_id <- selected$climate_id_text
    selection_i <- selection_i + 1L
    selection_rows[[selection_i]] <- data.frame(Station = station_name,
      Year = year, Station_ID = sid, Climate_ID = climate_id)

    # API returns one CSV containing a whole year of daily observations.
    filename <- sprintf("en_climate_daily_ON_%s_%d_P1D.csv", climate_id, year)
    destination <- file.path(folder, filename)

    if (file.exists(destination) && file.size(destination) > 100) {
      status <- "Skipped (already downloaded)"
    } else {
      status <- tryCatch({
        response <- GET("https://climate.weather.gc.ca/climate_data/bulk_data_e.html",
                        query = list(format = "csv", stationID = sid,
                                     Year = year, Month = 1, Day = 1,
                                     timeframe = 2, submit = "Download Data"),
                        timeout(90), user_agent("Ontario academic weather analysis"))
        stop_for_status(response)
        temp <- tempfile(fileext = ".csv")
        writeBin(content(response, as = "raw"), temp)
        first_line <- readLines(temp, n = 1, warn = FALSE, encoding = "UTF-8")
        if (!length(first_line) || !grepl("Date/Time", first_line, fixed = TRUE)) {
          unlink(temp)
          stop("Response does not look like an ECCC daily CSV")
        }
        if (!file.copy(temp, destination, overwrite = TRUE)) stop("Could not save file")
        unlink(temp)
        "Downloaded"
      }, error = function(e) paste("FAILED:", conditionMessage(e)))
      Sys.sleep(1)
    }

    message(station_name, " | ", year, " | ID ", sid, " | ", status)
    log_i <- log_i + 1L
    log_rows[[log_i]] <- data.frame(Station = station_name, Year = year,
      Station_ID = sid, Climate_ID = climate_id, Status = status)
  }
}

download_log <- bind_rows(log_rows)
station_year_ids <- bind_rows(selection_rows)
write_csv(download_log, file.path(output_dir, "download_log.csv"))
write_csv(station_year_ids, file.path(output_dir, "station_ids_by_year.csv"))

print(download_log, n = Inf)
cat("\nFiles saved in: ", output_dir, "\n", sep = "")
cat("Download log: ", file.path(output_dir, "download_log.csv"), "\n", sep = "")
cat("Station IDs: ", file.path(output_dir, "station_ids_by_year.csv"), "\n", sep = "")
