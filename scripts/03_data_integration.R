library(tidyverse)
library(lubridate)

# 1. Read cleaned electricity data
electricity_daily <- read_csv(
  "data/cleaned/electricity_daily.csv",
  show_col_types = FALSE
)


# 2. Read cleaned weather data
weather_daily <- read_csv(
  "data/cleaned/weather_daily.csv",
  show_col_types = FALSE
)

print(head(electricity_daily))
print(head(weather_daily))

print(names(electricity_daily))
print(names(weather_daily))

print(dim(electricity_daily))
print(dim(weather_daily))

# 3. Make sure Date is stored as Date in both datasets
electricity_daily <- electricity_daily |>
  mutate(Date = as.Date(Date))

weather_daily <- weather_daily |>
  mutate(Date = as.Date(Date))

print(class(electricity_daily$Date))
print(class(weather_daily$Date))

print(range(electricity_daily$Date))
print(range(weather_daily$Date))

missing_weather_dates <- electricity_daily |>
    anti_join(weather_daily,
      by = "Date"
    )

print(missing_weather_dates)