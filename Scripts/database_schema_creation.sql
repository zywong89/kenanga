DROP DATABASE IF EXISTS kenanga;
CREATE DATABASE kenanga WITH OWNER postgres;

\connect kenanga

DROP SCHEMA IF EXISTS data;
CREATE SCHEMA data AUTHORIZATION postgres;

DROP SCHEMA IF EXISTS result;
CREATE SCHEMA result AUTHORIZATION postgres;

-- Daily Data
-- DROP TABLE IF EXISTS data.daily_data;
-- CREATE TABLE data.daily_data (
  -- dt date,
  -- usdmyr numeric(6, 4),
  -- klpln numeric(8, 1),
  -- fbmklci numeric(8, 1),
  -- pal2maly numeric(8, 2),
  -- bo1 numeric(4, 1),
  -- qs1 numeric(6, 2),
  -- co1 numeric(6, 2),
  -- PRIMARY KEY (dt)
-- );


-- Monthly Data
-- DROP TABLE IF EXISTS data.monthly_data;
-- CREATE TABLE data.monthly_data (
  -- dt date,
  -- bo1 numeric(4, 1),
  -- co1 numeric(6, 2),
  -- qs1 numeric(6, 2),
  -- pal2maly numeric(8, 2),
  -- usdmyr numeric(6, 4),
  -- noaa numeric(3, 1),
  -- topi numeric(8, 3),
  -- dxy numeric(8, 3),
  -- PRIMARY KEY (dt)
-- );


-- Analytical Base Table
-- DROP TABLE IF EXISTS data.abt;
-- CREATE TABLE data.abt (
  -- dt date,
  -- pal2maly numeric(8, 2),
  -- topi numeric(8, 3),
  -- noaa numeric(3, 1),
  -- usdmyr numeric(6, 4),
  -- co1 numeric(6, 2),
  -- qs1 numeric(6, 2),
  -- trend int,
  -- PRIMARY KEY (dt)
-- );


-- Monthly Forecasts
-- DROP TABLE IF EXISTS result.forecasts;
-- CREATE TABLE result.forecasts (
  -- dt date,
  -- t1_pal2maly numeric(8, 2),
  -- t2_pal2maly numeric(8, 2),
  -- t3_pal2maly numeric(8, 2),
  -- PRIMARY KEY (dt)  
-- );
