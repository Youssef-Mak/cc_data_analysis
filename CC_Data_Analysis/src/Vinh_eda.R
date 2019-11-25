# Load libraries
library(countrycode)
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
  select(-indicator, -flag_codes, -frequency) %>% 
  dplyr::rename(year = time, country = location) %>% 
  mutate(country = countrycode(country, 'iso3c', 'country.name.en', nomatch=NULL)) %>%   # Replace 3 letter code with full name
  mutate(country = replace(country, country == 'EU', 'European Union')) %>%     # European union is not covered by the country code
  mutate(subject = mapvalues(subject, from=subject_mappings$code, to=subject_mappings$name)) %>%  # Replace subject code with name
  pivot_wider(names_from = measure, values_from = value) 


# value_per_activity_tsbl <-
#   value_per_activity %>%
#   as_tsibble(key = c(country, subject), index = year) %>% 
#   fill_gaps(.na=True)

 
# # Nest time series data per country and subject
# value_per_activity <-
#   value_per_activity %>%
#   group_by(country, subject) %>%
#   nest()
# 
# value_per_activity <-
#   value_per_activity %>%
#   unnest(cols = c(data))


# Load green house emissions data
country_ghg_emissions <- 
  read_csv('data/raw/CAIT-Country-GHG-Emissions.csv', skip=2) %>% 
  clean_names()




# Who are biggest producers of GHG ----------------------------------------

# Avg emissions of all types per country
country_avg_emissions <- country_ghg_emissions %>% 
  select(-year) %>% 
  filter(country != 'World', !str_detect(country, 'European Union')) %>% 
  group_by(country) %>% 
  summarise_each(list(mean = mean))


# Barplot of top 20 countries avg ghg emission
country_avg_emissions %>% 
  rename(avg_ghg_emissions = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e_mean) %>% 
  top_n(20, wt=avg_ghg_emissions) %>% 
  ggplot(aes(reorder(country, avg_ghg_emissions), avg_ghg_emissions)) +
  geom_bar(stat='identity') +
  coord_flip() + 
  labs(
    title = 'Top 20 countries based on their average GHG emissions',
    x = 'Country',
    y = 'Average GHG emission per year (MtCO2e)'
  ) +
  theme_hc()

"US, China, and India have very high average emission in comparison to the other countries so we will include them in our analysis
As well, Canada will of course be included
Could also include Brazil, Indonesia, Japan"

# TODO: We could also focus on CO2 emissions only, however it's part of GHG already
# TODO: Possibly the rest of the analysis will be purely based on GHG emission as it covers several different types of waste


countries_of_interest <-
  tribble(
    ~country,
    'United States',
    'China',
    'India',
    'Japan',
    'Brazil',
    'Canada'
  )

# GHG emissions for top 20 countries over time
p <- country_avg_emissions %>% 
  rename(avg_ghg_emissions = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e_mean) %>% 
  top_n(20, wt=avg_ghg_emissions) %>% 
  select(country) %>% 
  inner_join(country_ghg_emissions,by='country') %>% 
  ggplot(aes(x=year, y=total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e, colour=country)) +
  geom_line() +
  labs(
    title = 'GHG emission per country',
    y= 'GHG emission (MtCO2e)',
    x = 'Year',
    colour = 'Country'
  )
ggplotly(p)


# GHG emissions for countries of interest only
p <- countries_of_interest %>% 
  inner_join(country_ghg_emissions,by='country') %>% 
  ggplot(aes(x=year, y=total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e, colour=country)) +
  geom_line() +
  labs(
    title = 'GHG emission per country',
    y = 'GHG emission (MtCO2e)',
    x = 'Year',
    colour = 'Country'
  )
ggplotly(p)



# Effects of Regulations --------------------------------------------------

"Let's go through each country of India, China, US, Canada. Individually
Assumptions, the categories and sectored described by the different files refer to the same set of activities"


regulations <- read_csv('data/raw/law_search/data.csv') %>% 
  clean_names() %>% 
  rename(year = year_passed) %>% 
  mutate(categories = str_split(categories, ';'))
  
category_types <- regulations %>% 
  select(categories) %>% 
  unlist() %>% 
  str_trim() %>% 
  unique()


# Common categories among regulations and ghg emissions per country
common_categories <-
  tribble(
    ~regulation_categories, ~country_ghg_categories,
    'REDD+ and LULUCF', 'land use change and forestry mt co2e',
    'Energy Demand and Energy Supply', 'energy_mt_co2e',
    'Industry', 'industrical_processes_mt_co2e',
    'Transportation', 'transportation_mt_co2 (also part of energy)',
  )


## Canada

'Within the remit of this law, the Government announced the introduction of the clean fuel standard regulatory framework to 
achieve 30 megatonnes of annual reductions in GHG emissions by 2030, contributing to Canada’s effort to achieve its overall
GHG mitigation target of 30% emission reduction below 2005 levels by 2030.

We can check to see if Canada did even meet its goals'


regulations %>% 
  filter(country == 'Canada') %>% 
  inner_join(country_ghg_emissions, by='country') %>% 
  ggplot(aes(x=))
  



















