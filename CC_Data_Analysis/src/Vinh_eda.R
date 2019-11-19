# Load libraries
library(countrycode)
library(fable)
library(ggrepel)
library(ggthemes)
library(janitor)
library(lubridate)
library(plotly)
library(plyr)
library(skimr)
library(tidyverse)

# Load and clean value per activity data from data.oecd
subject_mappings <- tribble(
  ~code, ~name,
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
  read_csv('data/raw/value_added_per_act.csv') %>% 
  clean_names() %>%
  select(-indicator) %>% 
  dplyr::rename(year = time, country = location) %>% 
  mutate(country = countrycode(country, 'iso3c', 'country.name.en', nomatch=NULL)) %>%
  mutate(country = replace(country, country == 'EU', 'European Union')) %>% 
  mutate(subject = mapvalues(subject, from=subject_mappings$code, to=subject_mappings$name)) %>%
  pivot_wider(names_from = measure, values_from = value) 


value_per_activity_tsbl <-
  value_per_activity %>%
  as_tsibble(key = c(country, subject), index = year)

 
# Nest time series data per country and subject
value_per_activity <-
  value_per_activity %>%
  group_by(country, subject) %>%
  nest()

value_per_activity <-
  value_per_activity %>%
  unnest(cols = c(data))


# Taken from data.oecd
year_range <- c(1970, 2018)




# Load green house emissions data
country_ghg_emissions <- 
  read_csv('data/raw/CAIT-Country-GHG-Emissions.csv', skip=2) %>% 
  clean_names()

p <- country_ghg_emissions %>% 
  filter(country == 'India') %>% 
  pivot_longer(cols = -one_of('country', 'year'), names_to = 'subject', values_to = 'value') %>% 
  ggplot(aes(x = year, y = value, colour = subject)) +
  geom_line()

ggplotly(p)


country_ghg_emissions %>% 
  full_join(value_per_activity, by = c('country', 'year')) %>% 
  filter(subject == 'Industry and Energy', country == 'Canada') %>% 
  ggplot(aes(x=industrial_processes_mt_co2e, y=PC_VA, label=year)) +
  geom_smooth() + 
  geom_point() +
  geom_text_repel()








