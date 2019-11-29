rm(list = ls())

#Load Libaries
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


# Load dataset
country_ghg_emissions <- 
  read_csv('data/raw/CAIT-Country-GHG-Emissions.csv', skip=2) %>% 
  clean_names()

# Give the effective date of kyoto protocal
kyoto_protocol_effective_date <- c(start = ymd("2005-02-16"), end=ymd("2012-12-31"))

# Cleaning data and getting total data
canada_ghg_emissions <- country_ghg_emissions %>% 
  filter(country == "Canada") %>%
  mutate(total = rowSums(.[2:21])) %>%
  select(year,total)

# Data before the intervention perioid
canada_before_kyoto <- canada_ghg_emissions %>%
  filter(year < 2006)

# Plot of canada total greenhouse from 1990-2012
canada_ghg_emissions %>% 
  ggplot(aes(x = year, y =total)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

# Plot of canada total greenhouse from 1990-20-5
canada_before_kyoto %>% 
  ggplot(aes(x = year, y =total)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

# Calculate arima constants

# gives overall
acf(canada_before_kyoto)

#Calculates p
pacf(diff(log(canada_before_kyoto$total)))

#Caulculates q
acf(diff(log(canada_before_kyoto$total)))

#Caludlates 
plot(diff(log(canada_before_kyoto$total)))

fit <- arima(log(canada_before_kyoto$total), c(0,1,1))

pred <- predict(fit, n.ahead = 7)

pred1 <- 2.718^(pred$pred)

time_series <- ts(canada_before_kyoto, start= 1990, frequency=1)

autoplot(time_series)

fit <- lobster_tsbl %>%
  model(
    arima = ARIMA(value),
    naive = NAIVE(value)
  )


meanf(canada_ghg_emissions , 7)

naive(canada_ghg_emissions , 7)

autoplot(canada_before_kyoto ) +
  autolayer(meanf(canada_before_kyoto,h=7),
            series="Mean", PI=FALSE) +
  autolayer(rwf(canada_before_kyoto , h=7),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(canada_before_kyoto , drift=TRUE, h=7),
            series="Drift", PI=FALSE) +
  ggtitle("Forecast of Canada Total GHG from 2005-2012") +
  xlab("Year") + ylab("Total GHG") +
  guides(colour=guide_legend(title="Forecast"))
