# ---- libraries
library(tidyverse)
library(lubridate)

# ---- where_we_are
path <- "./data/core"

# ---- read_m_cases_timeseries
m_cases_timeseries_path <- sprintf("%s/Municipality_cases_time_series.csv", path)
m_cases_timeseries <- readr::read_csv2(m_cases_timeseries_path)
m_cases_timeseries

# ---- cleanup_m_cases_timeseries