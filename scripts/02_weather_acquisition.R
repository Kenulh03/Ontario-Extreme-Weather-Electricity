library(tidyverse)
library(lubridate)

weather_files <- list.files(
  "data/raw/weather",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

print(length(weather_files))
print(weather_files)

weather_sample <- read_csv(
  weather_files[1],
  show_col_types = FALSE
)

print(names(weather_sample))
print(head(weather_sample))
print(str(weather_sample))

weather_raw <- weather_files |>
  map_dfr(
    ~ read_csv(
      .x,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
  )

print(dim(weather_raw))
print(head(weather_raw))

weather_clean <- weather_raw |>
  select(
    `Longitude (x)`,
    `Latitude (y)`,
    `Station Name`,
    `Climate ID`,
    `Date/Time`,
    `Max Temp (°C)`,
    `Min Temp (°C)`,
    `Mean Temp (°C)`,
    `Heat Deg Days (°C)`,
    `Cool Deg Days (°C)`,
    `Total Precip (mm)`
  ) |>
  rename(
    Longitude = `Longitude (x)`,
    Latitude = `Latitude (y)`,
    Station = `Station Name`,
    Climate_ID = `Climate ID`,
    Date = `Date/Time`,
    Max_Temp_C = `Max Temp (°C)`,
    Min_Temp_C = `Min Temp (°C)`,
    Mean_Temp_C = `Mean Temp (°C)`,
    Heating_Degree_Days = `Heat Deg Days (°C)`,
    Cooling_Degree_Days = `Cool Deg Days (°C)`,
    Total_Precip_mm = `Total Precip (mm)`
  ) |>
  mutate(
    Date = as.Date(Date),
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude),
    Max_Temp_C = as.numeric(Max_Temp_C),
    Min_Temp_C = as.numeric(Min_Temp_C),
    Mean_Temp_C = as.numeric(Mean_Temp_C),
    Heating_Degree_Days = as.numeric(Heating_Degree_Days),
    Cooling_Degree_Days = as.numeric(Cooling_Degree_Days),
    Total_Precip_mm = as.numeric(Total_Precip_mm)
  ) |>
  distinct()

print(dim(weather_clean))
print(head(weather_clean))
str(weather_clean)

weather_clean |>
  count(Station) |>
  print()

weather_clean |>
  group_by(Station) |>
  summarise(
    Missing_Mean = sum(is.na(Mean_Temp_C)),
    Missing_Max = sum(is.na(Max_Temp_C)),
    Missing_Min = sum(is.na(Min_Temp_C)),
    .groups = "drop"
  ) |>
  print()

  weather_clean |>
  mutate(Year = year(Date)) |>
  group_by(Station, Year) |>
  summarise(
    Total_Days = n(),
    Missing_Mean = sum(is.na(Mean_Temp_C)),
    Missing_Max = sum(is.na(Max_Temp_C)),
    Missing_Min = sum(is.na(Min_Temp_C)),
    .groups = "drop"
  ) |>
  print(n = Inf)

daily_station_coverage <- weather_clean |>
  group_by(Date) |>
  summarise(
    Stations_Available = sum(!is.na(Mean_Temp_C)),
    .groups = "drop"
)

daily_station_coverage |>
  count(Stations_Available) |>
  arrange(Stations_Available) |>
  print()

safe_mean <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

safe_min <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    min(x, na.rm = TRUE)
  }
}

weather_daily <- weather_clean |>
  group_by(Date) |>
  summarise(
    Avg_Temp_C = safe_mean(Mean_Temp_C),
    Avg_Max_Temp_C = safe_mean(Max_Temp_C),
    Avg_Min_Temp_C = safe_mean(Min_Temp_C),
    Province_Max_Temp_C = safe_max(Max_Temp_C),
    Province_Min_Temp_C = safe_min(Min_Temp_C),
    Avg_Heating_Degree_Days = safe_mean(Heating_Degree_Days),
    Avg_Cooling_Degree_Days = safe_mean(Cooling_Degree_Days),
    Stations_Available = sum(!is.na(Mean_Temp_C)),
    .groups = "drop"
  ) |>
  filter(Stations_Available >= 4)

print(dim(weather_daily))
print(head(weather_daily))
print(colSums(is.na(weather_daily)))

weather_daily <- weather_daily |>
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    )
  )

write_csv(
  weather_daily,
  "data/cleaned/weather_daily.csv"
)

write_csv(
  weather_clean,
  "data/cleaned/weather_station_daily_clean.csv"
)