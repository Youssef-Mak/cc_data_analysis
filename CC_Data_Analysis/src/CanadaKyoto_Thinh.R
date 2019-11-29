rm(list = ls())
library(countrycode)
library(ggrepel)
library(fable)
library(ggthemes)
library(janitor)
library(lubridate)
library(plotly)
library(plyr)
library(skimr)
library(tidyverse)
library(tsibble)
library(forecast)


country_ghg_emissions <- 
  read_csv('data/raw/CAIT-Country-GHG-Emissions.csv', skip=2) %>% 
  clean_names()

kyoto_protocol_effective_date <- c(start = ymd("2005-02-16"), end=ymd("2012-12-31"))

canada_ghg_emissions <- country_ghg_emissions %>% 
  filter(country == "Canada") %>%
  mutate(total = rowSums(.[2:21])) %>%
  select(country,year,total)

canada_ghg_emissions %>% 
  ggplot(aes(x = year, y =total)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")


# Set training data from 1992 to 2007
# Plot some forecasts

meanf(canada_ghg_emissions , 7)

naive(canada_ghg_emissions , 7)

autoplot(canada_ghg_emissions ) +
  autolayer(meanf(canada_ghg_emissions,h=7),
            series="Mean", PI=FALSE) +
  autolayer(rwf(canada_ghg_emissions , h=7),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(canada_ghg_emissions , drift=TRUE, h=7),
            series="Drift", PI=FALSE) +
  ggtitle("Forecast of Canada Total GHG from 2005-2012") +
  xlab("Year") + ylab("Total GHG") +
  guides(colour=guide_legend(title="Forecast"))
