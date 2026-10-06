library(tidyverse)
library(lubridate)

# Find only the 7 main yearly files
electricity_files <- list.files(
  "data/raw/electricity",
  pattern = "^PUB_Demand_20(19|20|21|22|23|24|25)\\.csv$",
  full.names = TRUE
)

print(electricity_files)

electricity_raw <- electricity_files |>
  map_dfr(
    ~ read_csv(
      .x,
      skip = 3,
      show_col_types = FALSE
    )
  )

print(dim(electricity_raw))
print(head(electricity_raw))
print(tail(electricity_raw))

electricity_clean <- electricity_raw |>
  rename(
    Market_Demand_MW = `Market Demand`,
    Ontario_Demand_MW = `Ontario Demand`
  ) |>
  mutate(
    Date = as.Date(Date)
  ) |>
  distinct()

print(colSums(is.na(electricity_clean)))

print(sum(duplicated(electricity_clean)))

print(range(electricity_clean$Date))

print(colSums(is.na(electricity_clean)))

print(sum(duplicated(electricity_clean)))

print(range(electricity_clean$Date))

electricity_clean |>
  mutate(Year = year(Date)) |>
  count(Year) |>
  print()

electricity_daily <- electricity_clean |>
  group_by(Date) |>
  summarise(
    Peak_Demand_MW = max(Ontario_Demand_MW, na.rm = TRUE),
    Avg_Demand_MW = mean(Ontario_Demand_MW, na.rm = TRUE),
    Min_Demand_MW = min(Ontario_Demand_MW, na.rm = TRUE),
    .groups = "drop"
  )

print(head(electricity_daily))
print(dim(electricity_daily))

write_csv(
  electricity_clean,
  "data/cleaned/electricity_hourly_clean.csv"
)

write_csv(
  electricity_daily,
  "data/cleaned/electricity_daily.csv"
)