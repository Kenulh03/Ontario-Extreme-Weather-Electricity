
USE ontario_electricity;

-- VIEW 1: Daily electricity and weather data
CREATE OR REPLACE VIEW vw_daily_analysis AS
SELECT
    `Date`,
    `Year`,
    `Month`,
    Season,
    Peak_Demand_MW,
    Avg_Demand_MW,
    Min_Demand_MW,
    Avg_Temp_C,
    Avg_Max_Temp_C,
    Avg_Min_Temp_C,
    Province_Max_Temp_C,
    Province_Min_Temp_C,
    Avg_Heating_Degree_Days,
    Avg_Cooling_Degree_Days,
    Stations_Available
FROM integrated_daily;

-- VIEW 2: Annual electricity demand
CREATE OR REPLACE VIEW vw_annual_demand AS
SELECT
    `Year`,
    COUNT(*) AS Total_Days,
    ROUND(AVG(Peak_Demand_MW), 2) AS Avg_Peak_Demand_MW,
    MAX(Peak_Demand_MW) AS Max_Demand_MW,
    ROUND(AVG(Avg_Temp_C), 2) AS Avg_Temperature_C
FROM integrated_daily
GROUP BY `Year`;

-- VIEW 3: Monthly electricity demand
CREATE OR REPLACE VIEW vw_monthly_demand AS
SELECT
    `Month`,
    ROUND(AVG(Peak_Demand_MW), 2) AS Avg_Peak_Demand_MW,
    MAX(Peak_Demand_MW) AS Max_Demand_MW
FROM integrated_daily
GROUP BY `Month`;

-- VIEW 4: Seasonal electricity demand
CREATE OR REPLACE VIEW vw_seasonal_demand AS
SELECT
    Season,
    COUNT(*) AS Total_Days,
    ROUND(AVG(Peak_Demand_MW), 2) AS Avg_Peak_Demand_MW,
    ROUND(AVG(Avg_Temp_C), 2) AS Avg_Temperature_C
FROM integrated_daily
GROUP BY Season;

-- VIEW 5: Extreme temperature analysis
CREATE OR REPLACE VIEW vw_extreme_weather AS
SELECT
    `Date`,
    Peak_Demand_MW,
    Avg_Temp_C,
    Avg_Heating_Degree_Days,
    Avg_Cooling_Degree_Days,
    Season,
    CASE
        WHEN Avg_Temp_C <= -10 THEN 'Very Cold'
        WHEN Avg_Temp_C >= 25 THEN 'Very Hot'
        ELSE 'Other'
    END AS Temperature_Category
FROM integrated_daily;
