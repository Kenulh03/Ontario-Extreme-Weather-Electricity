
#LOAD REQUIRED LIBRARIES
library(dplyr)
library(readr)

#LOAD INTEGRATED DATA

# Run this script from the project root directory.
file_path <- "data/final/integrated_daily.csv"

if (!file.exists(file_path)) {
  file_path <- "integrated_daily.csv"
}

if (!file.exists(file_path)) {
  stop("Cannot find integrated_daily.csv. Check the file path.")
}

data <- read_csv(file_path, show_col_types = FALSE)

cat("\n===== DATASET INFORMATION =====\n")
cat("Total observations:", nrow(data), "\n")
cat("Total variables:", ncol(data), "\n")

print(names(data))
print(head(data))
print(summary(data))

#IDENTIFY REQUIRED VARIABLES

find_column <- function(candidates, columns) {
  match_index <- match(
    tolower(candidates),
    tolower(columns)
  )

  match_index <- match_index[!is.na(match_index)]

  if (length(match_index) == 0) {
    return(NA_character_)
  }

  columns[match_index[1]]
}

date_col <- find_column(
  c("Date", "Demand_Date", "date"),
  names(data)
)

peak_col <- find_column(
  c("Peak_Demand_MW", "Peak_MW", "Daily_Peak_MW"),
  names(data)
)

avg_col <- find_column(
  c("Avg_Demand_MW", "Average_Demand_MW", "Mean_Demand_MW"),
  names(data)
)

temp_col <- find_column(
  c("Avg_Temp_C", "Average_Temp_C", "Mean_Temp_C",
    "Avg_Temp", "Mean_Temp"),
  names(data)
)

hdd_col <- find_column(
  c("HDD", "Heating_Degree_Days", "Avg_HDD"),
  names(data)
)

cdd_col <- find_column(
  c("CDD", "Cooling_Degree_Days", "Avg_CDD"),
  names(data)
)

if (is.na(date_col) || is.na(peak_col)) {
  stop(
    "Required Date or Peak Demand column not found. ",
    "Check the column names printed above."
  )
}

#Standardize the columns used in analysis.
data$Date <- as.Date(data[[date_col]])
data$Peak_Demand_MW <- as.numeric(data[[peak_col]])

if (!is.na(avg_col)) {
  data$Avg_Demand_MW <- as.numeric(data[[avg_col]])
}

if (!is.na(temp_col)) {
  data$Avg_Temp_C <- as.numeric(data[[temp_col]])
}

if (!is.na(hdd_col)) {
  data$HDD <- as.numeric(data[[hdd_col]])
}

if (!is.na(cdd_col)) {
  data$CDD <- as.numeric(data[[cdd_col]])
}

#PREPARE DATA FOR ANALYSIS

data <- data %>%
  filter(
    !is.na(Date),
    !is.na(Peak_Demand_MW)
  ) %>%
  mutate(
    Year = as.integer(format(Date, "%Y")),
    Month = as.integer(format(Date, "%m")),
    Month_Name = factor(
      month.abb[Month],
      levels = month.abb
    ),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    )
  ) %>%
  arrange(Date)

cat("\n===== DATA PREPARATION =====\n")
cat("Total valid observations:", nrow(data), "\n")
cat("Missing peak demand:", sum(is.na(data$Peak_Demand_MW)), "\n")
cat("Duplicate dates:", sum(duplicated(data$Date)), "\n")

if (nrow(data) == 0) {
  stop("No valid records available for analysis.")
}

#OVERALL ELECTRICITY DEMAND ANALYSIS

cat("\n===== OVERALL ELECTRICITY DEMAND =====\n")

overall_summary <- data %>%
  summarise(
    Total_Days = n(),
    Average_Peak_MW = mean(Peak_Demand_MW, na.rm = TRUE),
    Median_Peak_MW = median(Peak_Demand_MW, na.rm = TRUE),
    Minimum_Peak_MW = min(Peak_Demand_MW, na.rm = TRUE),
    Maximum_Peak_MW = max(Peak_Demand_MW, na.rm = TRUE),
    SD_Peak_MW = sd(Peak_Demand_MW, na.rm = TRUE)
  )

print(overall_summary)

#Daily electricity demand trend
plot(
  data$Date,
  data$Peak_Demand_MW,
  type = "l",
  col = "steelblue",
  lwd = 1,
  xlab = "Date",
  ylab = "Peak Demand (MW)",
  main = "Daily Peak Electricity Demand in Ontario"
)

grid()

#ANNUAL ELECTRICITY DEMAND ANALYSIS

cat("\n===== ANNUAL ELECTRICITY DEMAND =====\n")

annual_summary <- data %>%
  group_by(Year) %>%
  summarise(
    Total_Days = n(),
    Average_Peak_MW = mean(Peak_Demand_MW, na.rm = TRUE),
    Maximum_Peak_MW = max(Peak_Demand_MW, na.rm = TRUE),
    Minimum_Peak_MW = min(Peak_Demand_MW, na.rm = TRUE),
    .groups = "drop"
  )

print(annual_summary)

plot(
  annual_summary$Year,
  annual_summary$Average_Peak_MW,
  type = "b",
  col = "darkblue",
  pch = 19,
  lwd = 2,
  xlab = "Year",
  ylab = "Average Daily Peak Demand (MW)",
  main = "Annual Electricity Demand in Ontario"
)

grid()

#MONTHLY ELECTRICITY DEMAND ANALYSIS

cat("\n===== MONTHLY ELECTRICITY DEMAND =====\n")

monthly_summary <- data %>%
  group_by(Month, Month_Name) %>%
  summarise(
    Total_Days = n(),
    Average_Peak_MW = mean(Peak_Demand_MW, na.rm = TRUE),
    Maximum_Peak_MW = max(Peak_Demand_MW, na.rm = TRUE),
    Minimum_Peak_MW = min(Peak_Demand_MW, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Month)

print(monthly_summary)

barplot(
  monthly_summary$Average_Peak_MW,
  names.arg = as.character(monthly_summary$Month_Name),
  col = "steelblue",
  border = NA,
  xlab = "Month",
  ylab = "Average Peak Demand (MW)",
  main = "Average Monthly Peak Electricity Demand"
)

#SEASONAL ELECTRICITY DEMAND ANALYSIS

cat("\n===== SEASONAL ELECTRICITY DEMAND =====\n")

seasonal_summary <- data %>%
  group_by(Season) %>%
  summarise(
    Total_Days = n(),
    Average_Peak_MW = mean(Peak_Demand_MW, na.rm = TRUE),
    Maximum_Peak_MW = max(Peak_Demand_MW, na.rm = TRUE),
    .groups = "drop"
  )

print(seasonal_summary)

boxplot(
  Peak_Demand_MW ~ Season,
  data = data,
  col = "lightblue",
  xlab = "Season",
  ylab = "Peak Demand (MW)",
  main = "Seasonal Distribution of Electricity Demand"
)

#DISTRIBUTION OF ELECTRICITY DEMAND

cat("\n===== ELECTRICITY DEMAND DISTRIBUTION =====\n")

hist(
  data$Peak_Demand_MW,
  breaks = 30,
  col = "lightblue",
  border = "white",
  xlab = "Peak Demand (MW)",
  main = "Distribution of Daily Peak Electricity Demand"
)

#EXTREME ELECTRICITY DEMAND DAYS

cat("\n===== EXTREME DEMAND DAYS =====\n")

# Define high-demand days using the 90th percentile.
high_demand_threshold <- quantile(
  data$Peak_Demand_MW,
  probs = 0.90,
  na.rm = TRUE
)

data <- data %>%
  mutate(
    High_Demand = if_else(
      Peak_Demand_MW >= high_demand_threshold,
      "High",
      "Normal"
    )
  )

cat("90th percentile threshold (MW):",
    high_demand_threshold, "\n")

high_demand_summary <- data %>%
  count(Year, High_Demand)

print(high_demand_summary)

high_demand_days <- data %>%
  filter(High_Demand == "High") %>%
  arrange(desc(Peak_Demand_MW))

cat("\nTop 10 electricity demand days:\n")

print(
  high_demand_days %>%
    select(Date, Peak_Demand_MW, Season) %>%
    head(10)
)

#TEMPERATURE AND ELECTRICITY DEMAND

if ("Avg_Temp_C" %in% names(data)) {

  cat("\n===== TEMPERATURE VS DEMAND =====\n")

  temperature_data <- data %>%
    filter(
      is.finite(Avg_Temp_C),
      is.finite(Peak_Demand_MW)
    )

  if (nrow(temperature_data) >= 3) {

    temperature_correlation <- cor(
      temperature_data$Avg_Temp_C,
      temperature_data$Peak_Demand_MW
    )

    cat(
      "Temperature-demand correlation:",
      temperature_correlation,
      "\n"
    )

    plot(
      temperature_data$Avg_Temp_C,
      temperature_data$Peak_Demand_MW,
      pch = 19,
      col = rgb(0, 0, 1, 0.3),
      xlab = "Average Temperature (C)",
      ylab = "Peak Demand (MW)",
      main = "Temperature vs Electricity Demand"
    )

    #A quadratic curve can represent both heating
    #and cooling-related electricity demand.
    temp_model <- lm(
      Peak_Demand_MW ~ Avg_Temp_C + I(Avg_Temp_C^2),
      data = temperature_data
    )

    temperature_grid <- seq(
      min(temperature_data$Avg_Temp_C),
      max(temperature_data$Avg_Temp_C),
      length.out = 200
    )

    predicted_demand <- predict(
      temp_model,
      newdata = data.frame(
        Avg_Temp_C = temperature_grid
      )
    )

    lines(
      temperature_grid,
      predicted_demand,
      col = "red",
      lwd = 2
    )

    print(summary(temp_model))
  }
}

#HEATING DEGREE DAYS ANALYSIS

if ("HDD" %in% names(data)) {

  cat("\n===== HEATING DEGREE DAYS =====\n")

  hdd_data <- data %>%
    filter(
      is.finite(HDD),
      is.finite(Peak_Demand_MW)
    )

  if (nrow(hdd_data) >= 3 &&
      sd(hdd_data$HDD) > 0) {

    hdd_correlation <- cor(
      hdd_data$HDD,
      hdd_data$Peak_Demand_MW
    )

    cat("HDD correlation:", hdd_correlation, "\n")

    plot(
      hdd_data$HDD,
      hdd_data$Peak_Demand_MW,
      pch = 19,
      col = "darkorange",
      xlab = "Heating Degree Days",
      ylab = "Peak Demand (MW)",
      main = "Heating Degree Days vs Electricity Demand"
    )

    hdd_model <- lm(
      Peak_Demand_MW ~ HDD,
      data = hdd_data
    )

    abline(
      hdd_model,
      col = "red",
      lwd = 2
    )

    print(summary(hdd_model))
  }
}

#COOLING DEGREE DAYS ANALYSIS

if ("CDD" %in% names(data)) {

  cat("\n===== COOLING DEGREE DAYS =====\n")

  cdd_data <- data %>%
    filter(
      is.finite(CDD),
      is.finite(Peak_Demand_MW)
    )

  if (nrow(cdd_data) >= 3 &&
      sd(cdd_data$CDD) > 0) {

    cdd_correlation <- cor(
      cdd_data$CDD,
      cdd_data$Peak_Demand_MW
    )

    cat("CDD correlation:", cdd_correlation, "\n")

    plot(
      cdd_data$CDD,
      cdd_data$Peak_Demand_MW,
      pch = 19,
      col = "firebrick",
      xlab = "Cooling Degree Days",
      ylab = "Peak Demand (MW)",
      main = "Cooling Degree Days vs Electricity Demand"
    )

    cdd_model <- lm(
      Peak_Demand_MW ~ CDD,
      data = cdd_data
    )

    abline(
      cdd_model,
      col = "blue",
      lwd = 2
    )

    print(summary(cdd_model))
  }
}

#COMBINED HDD AND CDD REGRESSION

if (all(c("HDD", "CDD") %in% names(data))) {

  cat("\n===== COMBINED WEATHER ANALYSIS =====\n")

  regression_data <- data %>%
    filter(
      is.finite(HDD),
      is.finite(CDD),
      is.finite(Peak_Demand_MW)
    )

  if (nrow(regression_data) >= 5 &&
      sd(regression_data$HDD) > 0 &&
      sd(regression_data$CDD) > 0) {

    weather_model <- lm(
      Peak_Demand_MW ~ HDD + CDD,
      data = regression_data
    )

    print(summary(weather_model))

    predicted <- predict(weather_model)

    plot(
      predicted,
      regression_data$Peak_Demand_MW,
      pch = 19,
      col = "steelblue",
      xlab = "Fitted Peak Demand (MW)",
      ylab = "Actual Peak Demand (MW)",
      main = "Actual vs Fitted Electricity Demand"
    )

    abline(
      0, 1,
      col = "red",
      lwd = 2
    )
  }
}

#FINAL ANALYSIS SUMMARY


cat("EXPLORATORY DATA ANALYSIS COMPLETED\n")


cat("Records analyzed:", nrow(data), "\n")
cat("First date:", as.character(min(data$Date)), "\n")
cat("Last date:", as.character(max(data$Date)), "\n")

cat(
  "Average peak demand:",
  round(mean(data$Peak_Demand_MW), 2),
  "MW\n"
)

cat(
  "Maximum peak demand:",
  round(max(data$Peak_Demand_MW), 2),
  "MW\n"
)

cat(
  "High-demand days:",
  sum(data$High_Demand == "High"),
  "\n"
)

cat("\nAll available analyses completed successfully.\n")



#SAVE EXPLORATORY ANALYSIS RESULTS
#Ontario Extreme Weather & Electricity Demand


# Create output folders
dir.create("results/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/models", recursive = TRUE, showWarnings = FALSE)


#SAVE SUMMARY TABLES


tables <- list(
  overall_summary = "overall_summary",
  annual_summary = "annual_summary",
  monthly_summary = "monthly_summary",
  seasonal_summary = "seasonal_summary",
  high_demand_days = "high_demand_days",
  high_demand_summary = "high_demand_summary"
)

for (filename in names(tables)) {
  object_name <- tables[[filename]]

  if (exists(object_name, inherits = FALSE)) {
    write.csv(
      get(object_name),
      file.path("results/tables", paste0(filename, ".csv")),
      row.names = FALSE
    )
  } else {
    warning("Missing analysis object: ", object_name)
  }
}

#SAVE CORRELATION RESULTS

correlations <- c(
  Average_Temperature = if (exists("temperature_correlation")) {
    temperature_correlation
  } else {
    NA_real_
  },
  Heating_Degree_Days = if (exists("hdd_correlation")) {
    hdd_correlation
  } else {
    NA_real_
  },
  Cooling_Degree_Days = if (exists("cdd_correlation")) {
    cdd_correlation
  } else {
    NA_real_
  }
)

correlation_results <- data.frame(
  Variable = names(correlations),
  Correlation = as.numeric(correlations)
)

write.csv(
  correlation_results,
  "results/tables/correlation_results.csv",
  row.names = FALSE
)


#FUNCTION TO SAVE PLOTS


save_plot <- function(filename, draw_plot) {
  png(
    filename = file.path("results/plots", filename),
    width = 1200,
    height = 800,
    res = 150
  )

  tryCatch(
    draw_plot(),
    finally = dev.off()
  )
}

#DAILY DEMAND TREND


save_plot("daily_demand_trend.png", function() {
  plot(
    data$Date,
    data$Peak_Demand_MW,
    type = "l",
    col = "steelblue",
    xlab = "Date",
    ylab = "Peak Demand (MW)",
    main = "Daily Peak Electricity Demand"
  )
  grid()
})


#ANNUAL DEMAND


save_plot("annual_demand.png", function() {
  plot(
    annual_summary$Year,
    annual_summary$Average_Peak_MW,
    type = "b",
    pch = 19,
    col = "darkblue",
    lwd = 2,
    xlab = "Year",
    ylab = "Average Peak Demand (MW)",
    main = "Annual Electricity Demand"
  )
  grid()
})


#MONTHLY DEMAND


save_plot("monthly_demand.png", function() {
  barplot(
    monthly_summary$Average_Peak_MW,
    names.arg = as.character(monthly_summary$Month_Name),
    col = "steelblue",
    border = NA,
    xlab = "Month",
    ylab = "Average Peak Demand (MW)",
    main = "Monthly Electricity Demand"
  )
})


#SEASONAL DEMAND


save_plot("seasonal_demand.png", function() {
  boxplot(
    Peak_Demand_MW ~ Season,
    data = data,
    col = "lightblue",
    xlab = "Season",
    ylab = "Peak Demand (MW)",
    main = "Seasonal Electricity Demand"
  )
})


#DEMAND DISTRIBUTION


save_plot("demand_distribution.png", function() {
  hist(
    data$Peak_Demand_MW,
    breaks = 30,
    col = "lightblue",
    border = "white",
    xlab = "Peak Demand (MW)",
    main = "Distribution of Daily Peak Demand"
  )
})


#TEMPERATURE VS DEMAND


if (exists("temperature_data") &&
    nrow(temperature_data) >= 3) {

  save_plot("temperature_vs_demand.png", function() {
    plot(
      temperature_data$Avg_Temp_C,
      temperature_data$Peak_Demand_MW,
      pch = 19,
      col = "steelblue",
      xlab = "Average Temperature (C)",
      ylab = "Peak Demand (MW)",
      main = "Temperature vs Electricity Demand"
    )

    if (exists("temperature_grid") &&
        exists("predicted_demand")) {
      lines(
        temperature_grid,
        predicted_demand,
        col = "red",
        lwd = 2
      )
    }
  })
}


#HDD VS DEMAND


if (exists("hdd_data") && nrow(hdd_data) >= 3) {
  save_plot("hdd_vs_demand.png", function() {
    plot(
      hdd_data$HDD,
      hdd_data$Peak_Demand_MW,
      pch = 19,
      col = "darkorange",
      xlab = "Heating Degree Days",
      ylab = "Peak Demand (MW)",
      main = "HDD vs Electricity Demand"
    )

    if (exists("hdd_model")) {
      abline(hdd_model, col = "red", lwd = 2)
    }
  })
}


#CDD VS DEMAND


if (exists("cdd_data") && nrow(cdd_data) >= 3) {
  save_plot("cdd_vs_demand.png", function() {
    plot(
      cdd_data$CDD,
      cdd_data$Peak_Demand_MW,
      pch = 19,
      col = "firebrick",
      xlab = "Cooling Degree Days",
      ylab = "Peak Demand (MW)",
      main = "CDD vs Electricity Demand"
    )

    if (exists("cdd_model")) {
      abline(cdd_model, col = "blue", lwd = 2)
    }
  })
}


#ACTUAL VS FITTED DEMAND


if (exists("weather_model") &&
    exists("regression_data")) {

  save_plot("actual_vs_fitted_demand.png", function() {
    plot(
      fitted(weather_model),
      regression_data$Peak_Demand_MW,
      pch = 19,
      col = "steelblue",
      xlab = "Fitted Demand (MW)",
      ylab = "Actual Demand (MW)",
      main = "Actual vs Fitted Electricity Demand"
    )

    abline(0, 1, col = "red", lwd = 2)
  })
}


#SAVE REGRESSION SUMMARIES


models <- c(
  temp_model = "temperature_regression.txt",
  hdd_model = "hdd_regression.txt",
  cdd_model = "cdd_regression.txt",
  weather_model = "combined_weather_regression.txt"
)

for (model_name in names(models)) {
  if (exists(model_name)) {
    capture.output(
      summary(get(model_name)),
      file = file.path("results/models", models[[model_name]])
    )
  }
}

#CONFIRM OUTPUTS


cat("\n========================================\n")
cat("EXPLORATORY ANALYSIS EXPORT COMPLETED\n")
cat("========================================\n")

cat("\nSaved tables:\n")
print(list.files("results/tables"))

cat("\nSaved plots:\n")
print(list.files("results/plots"))

cat("\nSaved regression reports:\n")
print(list.files("results/models"))

cat("\nResults saved in:", normalizePath("results"), "\n")
