library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(knitr)
library(tsibble)
library(stringr)
library(stringi)
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
  EmissionsIncrease <- CountryTibble %>% select(year,total_greenhouse_gas_emissions_kt_of_co2_equivalent) %>% lm(total_greenhouse_gas_emissions_kt_of_co2_equivalent~ year,data =.)
  EmissionsIncrease <- EmissionsIncrease$coefficients
  EmissionsIncrease <- (EmissionsIncrease["year"] > 0) %>% unname()
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
                             PosSigRelation = PosSigRelationship,
                             Increasing = EmissionsIncrease
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

Relationships <- Relationships%>% right_join(CountryNames, by = c("country_code" = "country_code"))

RegionalRelationships <- Relationships %>% group_split(region)
RegionalPlots <- list()
WorldRelation <- list()
WorldNotEnoughInfo <- list()
WorldInteresting <- list()
WorldCountryNegInc <- list()
WorldCountryNegDec <- list()
WorldCountryPosInc<-list()
WorldCountryPosDec<-list()
WorldCountryNone <- list()
for(region in RegionalRelationships)
{
  fixedRegion <- region %>% na.omit()%>% mutate(PosInc = PosSigRelation && Increasing %% SigRelation,
                                                PosDec = PosSigRelation && !Increasing && SigRelation,
                                                NegInc = NegSigRelation && Increasing && SigRelation,
                                                NegDec = NegSigRelation && !Increasing && SigRelation)%>%
    summarize(NumNegInc =sum(NegInc == TRUE),
              NumNegDec = sum(NegDec == TRUE),
              NumPosInc = sum(PosInc == TRUE),
              NumPosDec = sum(PosDec == TRUE),
              NumNone = sum(SigRelation == FALSE))
  regionName = region$region[[1]]
  
  interestCountriesFromRegion <- region %>% 
    na.omit() %>% 
    filter(SigRelation == TRUE,PosSigRelation == TRUE, NegSigRelation == TRUE) %>%
    select(country_code,short_name,region) 
  
  NotEnoughInfoCountries <- region %>% filter(is.na(PosSigRelation)) %>% 
    select(country_code,short_name,region) %>% anti_join(interestCountriesFromRegion,by = c("country_code" = "country_code"))
  
  
  PosCountriesInc <- region %>% 
    na.omit() %>% 
    filter(SigRelation == TRUE,PosSigRelation == TRUE, NegSigRelation == FALSE,Increasing == TRUE) %>%
    select(country_code,short_name,region) %>% anti_join(interestCountriesFromRegion,by = c("country_code" = "country_code"))
  
  PosCountriesDec <- region %>% 
    na.omit() %>% 
    filter(SigRelation == TRUE,PosSigRelation == TRUE, NegSigRelation == FALSE,Increasing == FALSE) %>%
    select(country_code,short_name,region) %>% anti_join(interestCountriesFromRegion,by = c("country_code" = "country_code"))
  
  NegCountriesInc <- region %>% 
    na.omit() %>% 
    filter(SigRelation == TRUE,NegSigRelation  == TRUE, PosSigRelation == FALSE,Increasing == TRUE) %>%
    select(country_code,short_name,region) %>% anti_join(interestCountriesFromRegion,by = c("country_code" = "country_code"))
  
  NegCountriesDec <- region %>% 
    na.omit() %>% 
    filter(SigRelation == TRUE,NegSigRelation  == TRUE, PosSigRelation == FALSE,Increasing == FALSE) %>%
    select(country_code,short_name,region) %>% 
    anti_join(interestCountriesFromRegion,by = c("country_code" = "country_code"))
    
  
  NoneCountries <- region %>% 
    na.omit() %>% 
    filter(SigRelation == FALSE) %>%
    select(country_code,short_name,region)
  
  
  fixedRegion <- fixedRegion %>% 
    mutate(NumNegInc = nrow(NegCountriesInc)) %>%
    mutate(NumNegDec = nrow(NegCountriesDec)) %>%
    mutate(NumPosInc = nrow(PosCountriesInc)) %>%
    mutate(NumPosDec = nrow(PosCountriesDec)) %>%
    mutate(notEnoughInfo = nrow(NotEnoughInfoCountries)) %>%
    mutate(both = nrow(interestCountriesFromRegion))
  fixedRegion <- fixedRegion %>% pivot_longer(cols = colnames(.),names_to = "Relationship",values_to = "value")
  regionPlot <-  fixedRegion %>%ggplot(aes(x = Relationship,y = value,colour = Relationship,fill = Relationship)) + 
    geom_bar(stat="identity") + labs(
      title = regionName
    ) + 
    scale_y_discrete(limits = seq(from = 0,to = max(fixedRegion$value), by = 5))
  RegionalPlots[[length(RegionalPlots) + 1]] <- regionPlot
  
  WorldCountryPosInc[[length(WorldCountryPosInc) + 1]] <- PosCountriesInc
  WorldCountryPosDec[[length(WorldCountryPosDec) + 1]] <- PosCountriesDec
  
  WorldCountryNegInc[[length(WorldCountryNegInc) + 1]] <- NegCountriesInc
  WorldCountryNegDec[[length(WorldCountryNegDec) + 1]] <- NegCountriesDec
  
  WorldNotEnoughInfo[[length(WorldNotEnoughInfo) + 1]] <- NotEnoughInfoCountries
  WorldInteresting[[length(WorldInteresting) + 1]] <- interestCountriesFromRegion
  
  WorldCountryNone[[length(WorldCountryNone) + 1]] <-  NoneCountries
  WorldRelation[[length(WorldRelation) + 1]] <- fixedRegion
}

WorldRelation <- WorldRelation %>% bind_rows() %>% 
  pivot_wider(names_from = "Relationship",values_from = "value",values_fn = list(value = sum)) %>%
  pivot_longer(cols = colnames(.),names_to = "Relation",values_to = "value")

WorldNotEnoughInfo <- WorldNotEnoughInfo %>% bind_rows() %>% mutate(RelationshipCategory = "No Info")
WorldInteresting <-WorldInteresting %>% bind_rows() %>% mutate(RelationshipCategory = "Both")

WorldCountryNegInc <- WorldCountryNegInc %>% bind_rows()  %>% mutate(RelationshipCategory = "Negative Increasing")
WorldCountryNegDec <- WorldCountryNegDec %>% bind_rows()  %>% mutate(RelationshipCategory = "Negative Decreasing")

WorldCountryPosInc<-WorldCountryPosInc %>% bind_rows() %>% mutate(RelationshipCategory = "Positive Increasing")
WorldCountryPosDec<-WorldCountryPosDec %>% bind_rows() %>% mutate(RelationshipCategory = "Positive Decreasing")

WorldCountryNone <- WorldCountryNone %>% bind_rows()  %>% mutate(RelationshipCategory = "None")
WorldInfo <- bind_rows(WorldNotEnoughInfo,WorldInteresting,WorldCountryNegInc,WorldCountryNegDec,WorldCountryPosInc,WorldCountryPosDec,WorldCountryNone)
WorldRelationPlot <-WorldRelation %>% 
  ggplot(aes(x = Relation, y = value, colour = Relation, fill = Relation)) +
  geom_bar(stat = "identity") + 
  labs(y = "Number of countries",
       title = "World stats")  + 
  scale_y_discrete(limits = seq(from = 0,to = max(WorldRelation$value), by = 5))
WorldRelationPlot
library(htmltools)
library(tmap)
library(sf)
library(spData)
library(sp)
GetCategory <- function(CountryName)
{
  category <- WorldInfo %>% filter(str_detect(string = CountryName,pattern = str_sub(short_name,1,4)) ) %>% select(RelationshipCategory)
  if(nrow(category) == 0){category = "No Info"}
  else if(nrow(category) == 1){category = category[[1]]}
  else{
    NegativeInc = paste0("Dominican Reblic|","Mauri|","Austria|","Guin|","Niger|","United States|","Mala|","The|","United A|","Turk|","South Afr|","New Z|","Slove")
    #PosInc = paste0("United Kingdom|","Democratic Republic of the Congo|","Slova")
    PosDec = paste0("United Kingdom|","Democratic Republic of the Congo|","Slova")
    
    NoInfo = paste0("North|","Dem. Rep. Korea|","French|","South S|","New C|","Green|","Dominica")
    
    both = paste0("Australia|","Greece")
    if(str_detect(CountryName,NegativeInc)){category = "Negative Increasing"}
    #else if(str_detect(CountryName,PosInc)){category = "Positive Increasing"}
    else if(str_detect(CountryName,PosDec)){category = "Positive Decreasing"}
    else if(str_detect(CountryName,NoInfo)){category = "No Info"}
    else if(str_detect(CountryName,both)){category = "Both"}
    else{category = "WTF"}
      }
  return(category)
}
world <- spData::world
world <- world %>%rowwise()%>%mutate(newC = GetCategory(name_long)) %>% ungroup() %>% st_as_sf()
world %>% tm_shape() + tm_fill(col = "newC")

getColourForRelation
