# ---- libraries
library(tidyverse)
library(lubridate)

# ---- cutoffs
low_cutoff <- "2021-12-01"
# ---- dates
rt_date <- "2022_02_01"

# ---- where_we_are
path <- "./data/core"

# ---- read_rt
rt_cases_path <- sprintf("%s/Rt_cases_%s.csv", path, rt_date)
rt_cases <- readr::read_csv2(rt_cases_path)
rt_cases

# ---- read_m_cases_timeseries
m_cases_timeseries_path <- sprintf("%s/Municipality_cases_time_series.csv", path)
m_cases_timeseries <- readr::read_csv2(m_cases_timeseries_path)
m_cases_timeseries

# ---- cleanup_m_cases_timeseries
m_cases_timeseries <- m_cases_timeseries %>%
  pivot_longer(cols = -c(`SampleDate`), names_to="Municipality", values_to="Cases") %>%
  rename(date = `SampleDate`)

m_cases_timeseries

# ---- read_m_tested_timeseries
m_tested_timeseries_path <- sprintf("%s/Municipality_tested_persons_time_series.csv", path)
m_tested_timeseries <- readr::read_csv2(m_tested_timeseries_path)
m_tested_timeseries <- m_tested_timeseries %>%
  pivot_longer(cols = -c(`PrDate_adjusted`), names_to="Municipality", values_to="Tested") %>%
  rename(date = `PrDate_adjusted`)

# ---- join_m_timeseries
municipality <- m_cases_timeseries %>% left_join(m_tested_timeseries, by = c("date", "Municipality"))
municipality <- municipality %>% mutate(Municipality = factor(Municipality))
municipality

# ---- m_high_cutoff
high_cutoff <- max(municipality$date) - days(2)

# ---- m_split_off_islands
islands <- c("Christiansø", "Fanø", "Læsø", "Samsø")
municipality_islands <- municipality %>% filter(Municipality %in% islands)
municipality <- municipality %>% filter(!Municipality %in% islands)

# ---- m_plot_rate
p <- ggplot(municipality %>% filter(date > low_cutoff, date <= high_cutoff, Municipality != 'NA'),
            aes(date, forcats::fct_rev(Municipality), fill=Cases/Tested))
p + geom_tile() +
    scale_fill_continuous(type = "viridis") +
    labs(title = "Positive Rate split by Municipality (Excluding Islands)",
         subtitle = "Christmas contributes to positive rate",
         caption = "Source: ssi.dk",
         x="Date",
         y = "Municipality")

# ---- m_plot_rate_islands
p <- ggplot(municipality_islands %>% filter(date > low_cutoff, date <= high_cutoff, Municipality != 'NA'),
            aes(date, forcats::fct_rev(Municipality), fill=Cases/Tested))
p + geom_tile() +
    scale_fill_continuous(type = "viridis") +
    labs(title = "Positive Rate split by Municipality (Islands only)",
         subtitle = "Christmas contributes to positive rate",
         caption = "Source: ssi.dk",
         x="Date",
         y = "Municipality")
