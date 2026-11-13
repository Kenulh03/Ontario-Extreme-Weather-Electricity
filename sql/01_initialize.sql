CREATE DATABASE IF NOT EXISTS ontario_electricity;

USE ontario_electricity;


CREATE TABLE IF NOT EXISTS integrated_daily (
    `Date` DATE PRIMARY KEY,

    Peak_Demand_MW INT,
    Avg_Demand_MW DOUBLE,
    Min_Demand_MW INT,

    Avg_Temp_C DOUBLE,
    Avg_Max_Temp_C DOUBLE,
    Avg_Min_Temp_C DOUBLE,

    Province_Max_Temp_C DOUBLE,
    Province_Min_Temp_C DOUBLE,

    Avg_Heating_Degree_Days DOUBLE,
    Avg_Cooling_Degree_Days DOUBLE,

    Stations_Available INT,

    `Year` INT,
    `Month` INT,
    Season VARCHAR(10)
);

