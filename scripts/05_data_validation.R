
library(readr)

cat("\n===== DATA VALIDATION STARTED =====\n")

#DEFINE PATHS

data_path <- "data/final/integrated_daily.csv"
results_path <- "results"

if (!file.exists(data_path)) {
  stop("Integrated dataset not found: ", data_path)
}

#LOAD INTEGRATED DATA

data <- read_csv(
  data_path,
  show_col_types = FALSE
)

cat("Total records:", nrow(data), "\n")
cat("Total columns:", ncol(data), "\n")

#VALIDATE REQUIRED COLUMNS

required_columns <- c(
  "Date",
  "Peak_Demand_MW"
)

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

cat("Required columns: PASS\n")

#VALIDATE DATE FORMAT

data$Date <- as.Date(data$Date)

if (anyNA(data$Date)) {
  stop("Invalid or missing dates found.")
}

cat("Date validation: PASS\n")

#VALIDATE DATE RANGE

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2025-12-31")

if (any(data$Date < start_date |
        data$Date > end_date)) {
  stop("Dates outside the project period found.")
}

cat("Date range: PASS\n")

#CHECK DUPLICATE DATES

duplicate_count <- sum(duplicated(data$Date))

cat("Duplicate dates:", duplicate_count, "\n")

if (duplicate_count > 0) {
  stop("Duplicate dates detected.")
}

cat("Duplicate validation: PASS\n")

#VALIDATE ELECTRICITY DEMAND

peak_demand <- suppressWarnings(
  as.numeric(data$Peak_Demand_MW)
)

if (any(!is.finite(peak_demand))) {
  stop("Missing or invalid peak demand values found.")
}

if (any(peak_demand <= 0)) {
  stop("Non-positive peak demand values found.")
}

cat("Electricity demand validation: PASS\n")

#CHECK DATASET SIZE

if (nrow(data) == 0) {
  stop("Integrated dataset is empty.")
}

cat("Dataset size: PASS\n")

#CHECK ANALYSIS OUTPUT FILES

required_outputs <- c(
  "results/tables/overall_summary.csv",
  "results/tables/annual_summary.csv",
  "results/tables/monthly_summary.csv",
  "results/tables/seasonal_summary.csv",
  "results/tables/high_demand_days.csv",
  "results/tables/high_demand_summary.csv",
  "results/tables/correlation_results.csv",
  "results/plots/daily_demand_trend.png",
  "results/plots/annual_demand.png",
  "results/plots/monthly_demand.png",
  "results/plots/seasonal_demand.png",
  "results/plots/demand_distribution.png"
)

missing_outputs <- required_outputs[
  !file.exists(required_outputs)
]

if (length(missing_outputs) > 0) {
  stop(
    "Missing analysis outputs: ",
    paste(missing_outputs, collapse = ", ")
  )
}

cat("Analysis output validation: PASS\n")

#CHECK WEATHER VARIABLES

weather_columns <- intersect(
  c("Avg_Temp_C", "HDD", "CDD"),
  names(data)
)

for (column in weather_columns) {
  values <- suppressWarnings(
    as.numeric(data[[column]])
  )

  invalid_count <- sum(!is.finite(values))

  cat(
    column,
    "- missing or invalid values:",
    invalid_count,
    "\n"
  )
}

#FINAL VALIDATION SUMMARY

cat("\n====================================\n")
cat("FINAL VALIDATION COMPLETED\n")
cat("====================================\n")

cat("Records validated:", nrow(data), "\n")
cat("Date range:",
    as.character(min(data$Date)),
    "to",
    as.character(max(data$Date)),
    "\n")

cat("\nALL REQUIRED VALIDATION CHECKS PASSED\n")
cat("Integrated dataset ready for SQL import checks.\n")