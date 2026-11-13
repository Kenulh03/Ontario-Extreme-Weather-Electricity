USE ontario_electricity;

-- 1. Verify total records
SELECT COUNT(*) AS Total_Records
FROM integrated_daily;

-- 2. Verify date range
SELECT
    MIN(`Date`) AS Start_Date,
    MAX(`Date`) AS End_Date
FROM integrated_daily;

-- 3. Check duplicate dates
SELECT
    `Date`,
    COUNT(*) AS Occurrences
FROM integrated_daily
GROUP BY `Date`
HAVING COUNT(*) > 1;

-- 4. Check missing values
SELECT
    SUM(Peak_Demand_MW IS NULL) AS Missing_Demand,
    SUM(Avg_Temp_C IS NULL) AS Missing_Temperature,
    SUM(Avg_Heating_Degree_Days IS NULL) AS Missing_HDD,
    SUM(Avg_Cooling_Degree_Days IS NULL) AS Missing_CDD
FROM integrated_daily;

-- 5. Validate weather station coverage
SELECT
    MIN(Stations_Available) AS Minimum_Stations,
    MAX(Stations_Available) AS Maximum_Stations
FROM integrated_daily;

-- 6. Verify yearly record counts
SELECT
    `Year`,
    COUNT(*) AS Total_Days
FROM integrated_daily
GROUP BY `Year`
ORDER BY `Year`;
