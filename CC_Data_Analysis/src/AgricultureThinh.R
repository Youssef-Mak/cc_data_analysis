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

#Load Monthly and annual temperature data from 1997-2018
temp <- read_csv('data/raw/NationalWeatherService.csv') %>% 
                  clean_names() %>%
                  select(year,total) %>%
                  rename(Temperature = "total") %>%
                  filter (year >1996)%>%
                  filter (year <2019)%>%
                  arrange(year)

#Load Gross Output for All Us subsectors from 1997-2018
argi <- read_csv('data/raw/GrossOutput.csv',skip = 4) %>%
                   clean_names()  %>%
                   select(-x1) %>%
                   filter(x2 == "Agriculture, forestry, fishing, and hunting") %>%
                   pivot_longer(-x2,
                                names_to = 'year',
                                values_to = 'value') 

#Putting dataset together
dataset <- temp %>%
  mutate(GrossProduct = argi$value)

#Retrieving sd for annual Temperature
tempsd <-sd(dataset$Temperature)

##Retrieving sd for annual GrossProduct
grosssd <- sd(dataset$GrossProduct)

#Getting mean for Temerature
meantemp <- mean(dataset$Temperature)

#Getting mean forGrossProduct
meangross <- mean(dataset$GrossProduct)

shapiro.test(dataset$Temperature)
shapiro.test(dataset$GrossProduct)

# f test
fvalue <- tempsd/Grosssd

#Comparting variance using f test
mydata1 <- rnorm(23, mean=meantemp, sd=tempsd)
mydata2 <- rnorm(23, mean=meangross, sd=grosssd)
var.test(mydata1, mydata2,
         alternative = "two.sided")


