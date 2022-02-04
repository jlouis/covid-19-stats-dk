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
municipality_ml <- municipality %>% filter(!Municipality %in% islands)

# ---- m_plot_rate
p <- ggplot(municipality_ml %>% filter(date > low_cutoff, date <= high_cutoff, Municipality != 'NA'),
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

# ---- m_summarize
m_summary <- municipality %>% group_by(date) %>% summarize(Cases = sum(Cases), Tested = sum(Tested))
p <- ggplot(m_summary %>% filter(date > low_cutoff, date <= high_cutoff), aes(x=date, y=Cases/Tested))
p + geom_point(alpha=.5) +
    geom_smooth(method = 'loess', span = .3) +
    scale_colour_brewer(palette="Set2") +
    labs(title = "Trendline for Covid-19 positive rate in Denmark",
         caption = "Source: ssi.dk")

m_summary <- m_summary %>% pivot_longer(-date, names_to="type", values_to="value")

p <- ggplot(m_summary %>% filter(date <= high_cutoff), aes(x=date, y=value, color=type))
p + geom_point(alpha=.5) +
    geom_smooth(method = 'loess', span = .3) +
    scale_colour_brewer(palette="Set2") +
    labs(title = "Trendline for Covid-19 cases in Denmark",
         caption = "Source: ssi.dk")


# ---- regional_area_vectors
nordjylland <- c("Brønderslev",
                 "Frederikshavn",
                 "Hjørring",
                 "Jammerbugt",
                 "Læsø",
                 "Mariagerfjord",
                 "Morsø",
                 "Rebild",
                 "Thisted",
                 "Vesthimmerlands",
                 "Aalborg")

midtjylland <- c("Favrskov",
                 "Hedensted",
                 "Herning",
                 "Holstebro",
                 "Horsens",
                 "Ikast-Brande",
                 "Lemvig",
                 "Norddjurs",
                 "Odder",
                 "Randers",
                 "Ringkøbing-Skjern",
                 "Samsø",
                 "Silkeborg",
                 "Skanderborg",
                 "Skive",
                 "Struer",
                 "Syddjurs",
                 "Viborg",
                 "Aarhus",
                 "Århus")

syddanmark <- c("Assens",
                "Billund",
                "Esbjerg",
                "Fanø",
                "Fredericia",
                "Faaborg-Midtfyn",
                "Haderslev",
                "Kerteminde",
                "Kolding",
                "Langeland",
                "Middelfart",
                "Nordfyns",
                "Nyborg",
                "Odense",
                "Svendborg",
                "Sønderborg",
                "Tønder",
                "Varde",
                "Vejen",
                "Vejle",
                "Ærø",
                "Aabenraa")

sjælland <- c("Faxe",
              "Greve",
              "Guldborgsund",
              "Holbæk",
              "Kalundborg",
              "Køge",
              "Lejre",
              "Lolland",
              "Næstved",
              "Odsherred",
              "Ringsted",
              "Roskilde",
              "Slagelse",
              "Solrød",
              "Sorø",
              "Stevns",
              "Vordingborg")

hovedstaden <- c("Copenhagen",
                 "Albertslund",
                 "Allerød",
                 "Ballerup",
                 "Bornholm",
                 "Brøndby",
                 "Dragør",
                 "Egedal",
                 "Fredensborg",
                 "Frederiksberg",
                 "Frederikssund",
                 "Furesø",
                 "Gentofte",
                 "Gladsaxe",
                 "Glostrup",
                 "Gribskov",
                 "Halsnæs",
                 "Helsingør",
                 "Herlev",
                 "Hillerød",
                 "Hvidovre",
                 "Høje-Taastrup",
                 "Hørsholm",
                 "Ishøj",
                 "København",
                 "Lyngby-Taarbæk",
                 "Rudersdal",
                 "Rødovre",
                 "Tårnby",
                 "Vallensbæk")

# ---- region_mapping
regions <- union(
  tibble(Municipality = nordjylland, Region = "Nordjylland"),
  tibble(Municipality = midtjylland, Region = "Midtjylland"))
regions <- union(regions, tibble(Municipality = syddanmark,  Region = "Syddanmark"))
regions <- union(regions, tibble(Municipality = sjælland,    Region = "Sjælland"))
regions <- union(regions, tibble(Municipality = hovedstaden, Region = "Hovedstaden"))
regions

# ---- region_split
municipality_ml <- municipality_ml %>% left_join(regions, by = c("Municipality"))
p <- ggplot(municipality_ml %>% filter(date > low_cutoff, date <= high_cutoff, Municipality != 'NA'),
            aes(date, forcats::fct_rev(Municipality), fill=Cases/Tested))
p + geom_tile() +
    scale_fill_continuous(type = "viridis") +
    facet_wrap(. ~ Region, scales="free") +
    labs(title = "Positive Rate split by Region/Municipality (Excluding Islands)",
         subtitle = "Christmas contributes to positive rate",
         caption = "Source: ssi.dk",
         x="Date",
         y = "Municipality")
