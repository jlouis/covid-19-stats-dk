# ---- libraries
library(tidyverse)
library(lubridate)

# ---- cutoffs
low_cutoff <- "2021-09-01"

# ---- where_we_are
path <- "./data/core"

# ---- read_m_cases_timeseries
m_cases_timeseries_path <- sprintf("%s/Municipality_cases_time_series.csv", path)
m_cases_timeseries <- readr::read_csv2(m_cases_timeseries_path)
m_cases_timeseries

# ---- cleanup_m_cases_timeseries
m_cases_timeseries <- m_cases_timeseries %>%
  pivot_longer(cols = -c(`SampleDate`), names_to="Municipality", values_to="Cases")
m_cases_timeseries

# ---- m_plot_cases
p <- ggplot(m_cases_timeseries %>% filter(SampleDate > low_cutoff), aes(x=SampleDate, y=Municipality, fill=Cases, height=0.8))
p + geom_tile() +
    scale_fill_continuous(type = "viridis") +
    labs(title = "Cases split by Municipality", subtitle = "", caption = "Source: ssi.dk", x="Date", y = "Municipality")

