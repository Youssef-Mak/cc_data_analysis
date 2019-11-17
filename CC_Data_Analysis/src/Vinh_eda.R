# Load libraries
library(ggthemes)
library(fable)
library(janitor)
library(lubridate)
library(plotly)
library(skimr)
library(tidyverse)

# Load and clean value per activity data from data.oecd
subject_mappings <- tribble(
  ~subject_code, ~subject_mapping,
  "AGRFORTFISH", "Agriculture and Fishery",
  "CONSTR", "Construction",
  "FINANCEINS", "Finance and Insurance",
  "INDUSENRG", "Industry and Energy",
  "INFCOMM", "Information Communication",
  "MFG", "Manufacturing",
  "OTHSERVACT", "Other Service Activities",
  "PROSCISUPP", "Profesional and Scientific Support Services",
  "PUBADMINEDUSOC", "Public",
  "REALEST", "Real Estate",
  "SERV", "Services",
  "TOT", "Total Growth",
  "WHLEHTELTRANSP", "Wholesale, Retail, Trade, Transport, Accomodation"
)

value_per_activity <- 
  readr::read_csv('data/raw/value_added_per_act.csv') %>% 
  clean_names() %>%
  select(-indicator) %>% 
  dplyr::rename(year = time, country = location) %>% 
  mutate(subject = mapvalues(subject, from=subject_mappings$subject_code, to=subject_mappings$subject_mapping)) %>%
  pivot_wider(names_from = measure, values_from = value) 


value_per_activity_tsbl <-
  value_per_activity %>% 
  as_tsibble(key = c(country, subject), index = year)

 
 
 
# Nest time series data per country and subject
# value_per_activity <-
#   value_per_activity %>% 
#   group_by(country, subject) %>% 
#   nest()
# 
# value_per_activity <-
#   value_per_activity %>% 
#   unnest(cols = c(data))


# Taken from data.oecd
year_range <- c(1970, 2018)

p <- value_per_activity %>% 
  filter(country == 'AUS') %>% 
  ggplot(aes(year, AGRWTH, colour=subject)) +
  geom_line(alpha = 1/3)

ggplotly(p)


# Load green house emissions data








