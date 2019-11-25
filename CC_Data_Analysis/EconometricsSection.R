library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(tidyr)
library(stringr)
library(knitr)
library(tibble)
library(purrr)
library(broom)
library(caret)
library(keras)
library(mlbench)
Econometrics <- read_excel("data/raw/WDIEXCEL.xlsx",sheet = "Data")

Econometrics <- Econometrics %>% clean_names() %>% select(-indicator_code) %>%
  pivot_longer(cols = starts_with("x"),names_to = "year",values_to = "value") %>%
  pivot_wider(names_from = indicator_name, values_from = "value") %>%
  mutate(year = as.numeric(str_remove(year,"x"))) %>% clean_names()

Emissions <- Econometrics %>% select(adjusted_savings_particulate_emission_damage_percent_of_gni,
                                     total_greenhouse_gas_emissions_kt_of_co2_equivalent,
                                     year,
                                     country_code)

Econometrics <- Econometrics %>% select(-contains("emissions"))

CountryNames <- read_excel("data/raw/WDIEXCEL.xlsx",sheet = "Country") %>% 
  clean_names() %>% 
  select(country_code,region) %>%
  na.omit()

Econometrics <- Econometrics %>% inner_join(CountryNames,by = c("country_code"="country_code")) %>%
  inner_join(Emissions,by = c("country_code"="country_code","year"="year")) %>% 
  mutate(year = paste0(year,"/01","/01")) %>%
  mutate(year = as.Date(year,"%Y/%m/%d"))  %>% 
  BBmisc::normalize(method = "standardize", 
                    margin = 2)
                                                   

set.seed(0)
control <- trainControl(method = "repeatedcv",number = 25,repeats = 3)
model <- train(total_greenhouse_gas_emissions_kt_of_co2_equivalent~.)


