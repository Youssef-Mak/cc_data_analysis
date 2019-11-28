library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(tidyr)
library(stringr)
library(knitr)
library(tsibble)
library(feasts)
library(fable)
library(tibble)
library(purrr)
library(broom)
library(keras)

Econometrics <- read_excel("data/raw/WDIEXCEL.xlsx",sheet = "Data")

Econometrics <- Econometrics %>% clean_names() %>% select(-indicator_code) %>%
  pivot_longer(cols = starts_with("x"),names_to = "year",values_to = "value") %>%
  pivot_wider(names_from = indicator_name, values_from = "value") %>%
  mutate(year = as.numeric(str_remove(year,"x"))) %>% clean_names()

DamageEmissions <- Econometrics %>% select(adjusted_savings_particulate_emission_damage_percent_of_gni,
                                     total_greenhouse_gas_emissions_kt_of_co2_equivalent,
                                     year,
                                     country_code)
Emissions <- Econometrics  %>% select(contains("emissions"),year,country_code)


CountryNames <- read_excel("data/raw/WDIEXCEL.xlsx",sheet = "Country") %>% 
  clean_names() %>% 
  select(country_code,region,short_name) %>%
  na.omit()

Econometrics <- Econometrics %>% inner_join(CountryNames,by = c("country_code"="country_code")) %>%
  inner_join(Emissions,by = c("country_code"="country_code","year"="year")) %>% 
  mutate(year = paste0(year,"/01","/01")) %>%
  mutate(year = as.Date(year,"%Y/%m/%d"))  
                                                   

set.seed(0)

DamageEmssionsOutput <- DamageEmissions %>% select(year,adjusted_savings_particulate_emission_damage_percent_of_gni,country_code)
GreenhouseGas <- DamageEmissions %>% select(year,total_greenhouse_gas_emissions_kt_of_co2_equivalent,country_code)

WorldDamageEmissions <- DamageEmssionsOutput %>% inner_join(GreenhouseGas,by = c("country_code"="country_code","year"="year"))%>%
  na.omit() %>%
  as_tsibble(key = country_code,index = year)
  
WorldCCF <-   WorldDamageEmissions %>% group_split(country_code)

#EmissionDamageModels <- WorldDamageEmissions %>% model( arimaEmission = ARIMA(adjusted_savings_particulate_emission_damage_percent_of_gni))
#GreenhouseEmissionModels <- WorldDamageEmissions %>% model( arimaGreenhouse = ARIMA(total_greenhouse_gas_emissions_kt_of_co2_equivalent))
#fc <- EmissionDamageModels %>% select(arima) %>% coef()
#small <- EmissionDamageModels %>% 
 # inner_join(GreenhouseEmissionModels,by = c("country_code" = "country_code"))

GenerateCCF <- function(CountryTibble,emptyCCFList){
  country <- CountryTibble$country_code[[1]]
  
  significanceLevel <- 2 / sqrt(nrow(CountryTibble))
  tempCCF <- ccf(CountryTibble$adjusted_savings_particulate_emission_damage_percent_of_gni,
                 CountryTibble$total_greenhouse_gas_emissions_kt_of_co2_equivalent,
                 plot = FALSE)
  tempCCFLevels <- tibble(lag = as.vector(tempCCF$lag),
                          acf = as.vector(tempCCF$acf)) %>% filter(abs(acf) >= significanceLevel) 
  SigRelationship <- nrow(tempCCFLevels) > 0
  PosSigRelationship <- tempCCFLevels %>% filter(acf >= significanceLevel) %>% nrow()
  PosSigRelationship <- PosSigRelationship > 0
  NegSigRelationship <- tempCCFLevels %>% filter(acf <= -significanceLevel) %>% nrow()
  NegSigRelationship <- NegSigRelationship > 0
  CCFRelationships <- tibble(country_code = country,
                             SigRelation = SigRelationship,
                             NegSigRelation = NegSigRelationship,
                             PosSigRelation = PosSigRelationship
                             )
  emptyCCFList <- list()
  tempCCF$snames <-paste0("Particulate damage and greenhouse gas emissions cross correlation for country:",
                                   country)
  emptyCCFList[[length(emptyCCFList) + 1]]<-(list(tempCCF,country))
  return(list(CCFRelationships,emptyCCFList))
}

GetCCFRelationshipsFromWorld <- function(worldData){return(worldData[[1]])}
GetCCFObjectsFromWorld <- function(worldData){return(worldData[[2]])}
WorldResults <-  lapply(WorldCCF,FUN = GenerateCCF)
Relationships<- lapply(WorldResults,FUN = GetCCFRelationshipsFromWorld) %>% bind_rows()
CCFPerCountry<- lapply(WorldResults,FUN = GetCCFObjectsFromWorld)

test <- Relationships%>% right_join(CountryNames, by = c("country_code" = "country_code"))
