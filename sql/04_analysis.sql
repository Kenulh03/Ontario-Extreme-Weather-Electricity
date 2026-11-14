USE ontario_electricity;

-- 1. Annual electricity demand analysis
SELECT
    `Year`,
    COUNT(*) AS Total_Days,
    ROUND(AVG(Peak_Demand_MW), 2) AS Avg_Peak_Demand_MW,
    MAX(Peak_Demand_MW) AS Maximum_Demand_MW,
    ROUND(AVG(Avg_Temp_C), 2) AS Avg_Temperature_C
FROM integrated_daily
GROUP BY `Year`
ORDER BY `Year`;

-- 2. Monthly demand analysis
SELECT
    `Month`,
    ROUND(AVG(Peak_Demand_MW), 2) AS Avg_Peak_Demand_MW,
    MAX(Peak_Demand_MW) AS Maximum_Demand_MW
FROM integrated_daily
GROUP BY `Month`
ORDER BY `Month`;

-- 3. Seasonal demand analysis
SELECT
    Season,
    COUNT(*) AS Total_Days,
    ROUND(AVG(Peak_Demand_MW), 2) AS Avg_Peak_Demand_MW,
    ROUND(AVG(Avg_Temp_C), 2) AS Avg_Temperature_C
FROM integrated_daily
GROUP BY Season
ORDER BY FIELD(
    Season,
    'Winter', 'Spring', 'Summer', 'Fall'
);

-- 4. Top 10 highest electricity demand days
SELECT
    `Date`,
    Peak_Demand_MW,
    ROUND(Avg_Temp_C, 2) AS Avg_Temperature_C,
    Season
FROM integrated_daily
ORDER BY Peak_Demand_MW DESC
LIMIT 10;
