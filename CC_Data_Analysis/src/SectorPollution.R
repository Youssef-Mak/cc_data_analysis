
library(plyr)
library(dplyr)
library(countrycode)
library(tidyr)
library(readxl)
library(janitor)
library(ggplot2)
library(skimr)
library(stringr)
library(ggrepel)

install.packages("countrycode")
library(countrycode)


country_emissions_filename <- "data/raw/CAIT-Country-CO2-Emissions-Energy-Sub-Sector.csv"

activity_value_filename <- "data/raw/value_added_per_act.csv"


processCsvData <- function(csv_filename) {
  csv_df <- read.csv(csv_filename) %>% 
    clean_names() %>% 
    na.omit()
  return(csv_df)
}

# Country Emission DF
ce_df <- processCsvData(country_emissions_filename)

# Per Country Activity Value Added DF
coun_ava_df <- processCsvData(activity_value_filename) %>% 
  dplyr::rename(activity = subject) %>% 
  dplyr::rename(year = time) %>% 
  dplyr::mutate(location = countrycode::countrycode(location, 'iso3c', 'country.name.en'))

coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("AGRFORTFISH" = "Agriculture and Fishery"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("CONSTR" = "Construction"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("FINANCEINS" = "Finance and Insurance"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("INDUSENRG" = "Industry and Energy"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("INFCOMM" = "Information Communication"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("MFG" = "Manufacturing"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("OTHSERVACT" = "Other Service Activities"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("PROSCISUPP" = "Profesional and Scientific Support Services"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("PUBADMINEDUSOC" = "Public")) # Public Administration, Defense, Education, Health, Social Work
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("REALEST" = "Real Estate"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("SERV" = "Services"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("TOT" = "Total Growth"))
coun_ava_df$activity<- plyr::revalue(coun_ava_df$activity, c("WHLEHTELTRANSP" = "Wholesale, Retail, Trade, Transport, Accomodation"))

countries_ce <- unique(ce_df$country)
countries_ce
countries_df <- unique(coun_ava_df$location)
countries_df

merged_df <- dplyr::left_join(ce_df, coun_ava_df, by = c("country" = "location", "year" = "year")) 
  



# Per Activity Value added DF
int_ava_df <- coun_ava_df %>% 
  dplyr::group_by(activity, year) %>% 
  dplyr::summarise(average_val = mean(value))


int_ava_df %>%
  ggplot(
    aes(
      x = (year),
      y = (average_val),
      color = activity
    )
  ) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = lm) +
  facet_grid(rows = vars(activity)) +
  labs(
    x = "Year",
    y = "Value Added",
    colour = 'Activity Category',
    title = "Economical Value Added vs Year"
  )
  








