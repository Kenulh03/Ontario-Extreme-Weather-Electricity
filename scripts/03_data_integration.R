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


electricity_daily$Date <- as.Date(electricity_daily$Date)
electricity_hourly$Date <- as.Date(electricity_hourly$Date)
weather_daily$Date <- as.Date(weather_daily$Date)

#Validate datasets

cat("\n=== DATA VALIDATION ===\n")

cat("Electricity rows:", nrow(electricity_daily), "\n")
cat("Weather rows:", nrow(weather_daily), "\n")

cat("Electricity duplicate dates:",
    sum(duplicated(electricity_daily$Date)), "\n")

cat("Weather duplicate dates:",
    sum(duplicated(weather_daily$Date)), "\n")

stopifnot(
  !anyNA(electricity_daily$Date),
  !anyNA(weather_daily$Date),
  !anyDuplicated(electricity_daily$Date),
  !anyDuplicated(weather_daily$Date)
)

# 4. Identify unmatched dates

missing_weather_dates <- electricity_daily %>%
  anti_join(weather_daily, by = "Date")

missing_electricity_dates <- weather_daily %>%
  anti_join(electricity_daily, by = "Date")

cat("\n=== UNMATCHED DATES ===\n")

print(missing_weather_dates$Date)
print(missing_electricity_dates$Date)

# 5. Check daily hourly-data completeness

hourly_coverage <- electricity_hourly %>%
  group_by(Date) %>%
  summarise(
    Hourly_Records = n(),
    Unique_Hours = n_distinct(Hour),
    .groups = "drop"
  )

incomplete_days <- hourly_coverage %>%
  filter(Hourly_Records != 24 | Unique_Hours != 24)

cat("\n=== INCOMPLETE HOURLY DAYS ===\n")
print(incomplete_days)

# 6. Merge electricity and weather datasets

integrated_daily <- electricity_daily %>%
  inner_join(
    weather_daily,
    by = "Date",
    relationship = "one-to-one"
  ) %>%
  filter(
    Date >= as.Date("2019-01-01"),
    Date <= as.Date("2025-12-31")
  ) %>%
  arrange(Date)

# Weather station coverage already filtered
# to >= 4 in 02_weather_acquisition.R

# 7. Validate integrated dataset

cat("\n=== INTEGRATION RESULTS ===\n")

cat("Total rows:", nrow(integrated_daily), "\n")
cat("Total columns:", ncol(integrated_daily), "\n")
cat("Duplicate dates:",
    sum(duplicated(integrated_daily$Date)), "\n")

cat("Missing values:\n")
print(colSums(is.na(integrated_daily)))

print(head(integrated_daily))
print(summary(integrated_daily))

#Export integrated dataset and audit files

write_csv(
  integrated_daily,
  "data/final/integrated_daily.csv"
)

write_csv(
  missing_weather_dates,
  "data/final/excluded_weather_dates.csv"
)

write_csv(
  missing_electricity_dates,
  "data/final/excluded_electricity_dates.csv"
)

write_csv(
  incomplete_days,
  "data/final/incomplete_hourly_days.csv"
)

cat("\nDATA INTEGRATION COMPLETED SUCCESSFULLY!\n")
