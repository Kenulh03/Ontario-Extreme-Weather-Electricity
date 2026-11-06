library(dplyr)
library(readr)


electricity_daily <- read_csv(
  "data/cleaned/electricity_daily.csv",
  show_col_types = FALSE
)

electricity_hourly <- read_csv(
  "data/cleaned/electricity_hourly_clean.csv",
  show_col_types = FALSE
)

weather_daily <- read_csv(
  "data/cleaned/weather_daily.csv",
  show_col_types = FALSE
)

weather_station <- read_csv(
  "data/cleaned/weather_station_daily_clean.csv",
  show_col_types = FALSE
)


# DATA ACQUISITION

cat("\n=== DATASETS IMPORTED SUCCESSFULLY ===\n")

datasets <- list(
  Electricity_Hourly = electricity_hourly,
  Electricity_Daily = electricity_daily,
  Weather_Station = weather_station,
  Weather_Daily = weather_daily
)

for (name in names(datasets)) {
  cat(name, ":",
      nrow(datasets[[name]]), "rows,",
      ncol(datasets[[name]]), "columns\n")
}


#ELECTRICITY DEMAND

cat("\n=== CLEANED ELECTRICITY DATA ===\n")

print(head(electricity_daily, 8))
print(names(electricity_daily))

cat("\nMissing values:\n")
print(colSums(is.na(electricity_daily)))


# WEATHER DATA


cat("\n=== CLEANED WEATHER DATA ===\n")

print(head(weather_daily, 8))
print(names(weather_daily))

cat("\nMissing values:\n")
print(colSums(is.na(weather_daily)))

if ("Stations_Available" %in% names(weather_daily)) {
  cat("\nStation availability:\n")
  print(table(weather_daily$Stations_Available))

  cat("Days with fewer than 4 stations:",
      sum(weather_daily$Stations_Available < 4,
          na.rm = TRUE), "\n")
}


# DATA INTEGRATION


cat("\n=== INTEGRATED DATASET ===\n")

# Confirm the date columns exist
stopifnot(
  "Date" %in% names(electricity_daily),
  "Date" %in% names(weather_daily)
)

electricity_daily$Date <- as.Date(electricity_daily$Date)
weather_daily$Date <- as.Date(weather_daily$Date)

# Match electricity and weather records by date
integrated_daily <- inner_join(
  electricity_daily,
  weather_daily,
  by = "Date"
)

print(head(integrated_daily, 8))

cat("\nIntegrated rows:",
    nrow(integrated_daily), "\n")

cat("Integrated columns:",
    ncol(integrated_daily), "\n")

cat("Duplicate dates:",
    sum(duplicated(integrated_daily$Date)), "\n")

# Export integrated dataset
write_csv(integrated_daily, "integrated_daily.csv")

cat("\nIntegrated dataset saved successfully!\n")
