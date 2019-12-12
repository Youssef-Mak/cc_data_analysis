library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(knitr)
library(tsibble)
library(stringr)

library(feasts)
library(fable)
library(tibble)

library(htmltools)
library(tmap)
library(sf)
library(spData)
library(sp)
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
  return(list(CCFRelationships,emptyCCFList,significanceLevel))
}

GetCCFRelationshipsFromWorld <- function(worldData){return(worldData[[1]])}
GetCCFObjectsFromWorld <- function(worldData){return(worldData[[2]])}
GetSigLevels <- function(worldData){return(worldData[[3]])}
WorldResults <-  lapply(WorldCCF,FUN = GenerateCCF)
Relationships<- lapply(WorldResults,FUN = GetCCFRelationshipsFromWorld) %>% bind_rows()
CCFPerCountry<- lapply(WorldResults,FUN = GetCCFObjectsFromWorld)
AllSignificanceLevels <- lapply(WorldResults,FUN = GetSigLevels) 
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
    scale_y_discrete(limits = seq(from = 0,to = max(fixedRegion$value), by = 5))+
    scale_x_discrete(labels = c("both" = "both",
                                "nothEnoughInfo" = "Not enough info",
                                "NumNegDec" = "Negative correlation with decreasing",
                                "NumNegInc" = "Negative corrrelation with increasing",
                                "NumNone" = "no correlation",
                                "NumPosDec" = "Positive correlation with decreasing",
                                "NumPosInc" = "Positive correlation with increasing"))+
    scale_fill_discrete(labels = c("both",
                                   "Not enough info",
                                   "Negative correlation with decreasing emissions",
                                   "Negative corrrelation with increasing emissions",
                                   "no correlation",
                                   "Positive correlation with decreasing emissions",
                                   "Positive correlation with increasing emissions"))+
    guides(color=FALSE)
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

EastAsiaPacificBar <- RegionalPlots[[1]]
EuropeCentralAsiaBar <- RegionalPlots[[2]]
LatinAmericaCaribbeanBar <- RegionalPlots[[3]]
MiddleEastAfricaBar <- RegionalPlots[[4]]
NorthAmericaBar <- RegionalPlots[[5]]
SouthAsiaBar <- RegionalPlots[[6]]
SubSaharaAfricaBar <- RegionalPlots[[7]]

WorldRelation <- WorldRelation %>% bind_rows() %>% 
  pivot_wider(names_from = "Relationship",values_from = "value",values_fn = list(value = sum)) %>%
  pivot_longer(cols = colnames(.),names_to = "Relation",values_to = "value")

WorldNotEnoughInfo <- WorldNotEnoughInfo %>% bind_rows() %>% mutate(RelationshipCategory = "No Info")
WorldInteresting <-WorldInteresting %>% bind_rows() %>% mutate(RelationshipCategory = "Both negative and positive correlation depending on the lag")

WorldCountryNegInc <- WorldCountryNegInc %>% bind_rows()  %>% mutate(RelationshipCategory = "Negative cross correlation with increasing emissions")
WorldCountryNegDec <- WorldCountryNegDec %>% bind_rows()  %>% mutate(RelationshipCategory = "Negative cross correlation decreasing emissions")

WorldCountryPosInc<-WorldCountryPosInc %>% bind_rows() %>% mutate(RelationshipCategory = "Positive cross correlation with increasing emissions")
WorldCountryPosDec<-WorldCountryPosDec %>% bind_rows() %>% mutate(RelationshipCategory = "Positive cross correlations with decreasing emissions")

WorldCountryNone <- WorldCountryNone %>% bind_rows()  %>% mutate(RelationshipCategory = "No correlation")
WorldInfo <- bind_rows(WorldNotEnoughInfo,WorldInteresting,WorldCountryNegInc,WorldCountryNegDec,WorldCountryPosInc,WorldCountryPosDec,WorldCountryNone)
WorldRelationPlot <-WorldRelation %>% 
  ggplot(aes(x = Relation, y = value, colour = Relation, fill = Relation)) +
  geom_bar(stat = "identity") + 
  labs(y = "Number of countries",
       title = "World stats")  + 
  scale_y_discrete(limits = seq(from = 0,to = max(WorldRelation$value), by = 5))+
  scale_x_discrete(labels = c("both" = "both",
                              "nothEnoughInfo" = "Not enough info",
                              "NumNegDec" = "Negative correlation with decreasing",
                              "NumNegInc" = "Negative corrrelation with increasing",
                              "NumNone" = "No correlation",
                              "NumPosDec" = "Positive correlation with decreasing",
                              "NumPosInc" = "Positive correlation with increasing"))+
  scale_fill_discrete(labels = c("both",
                                 "Not enough info",
                                 "Negative correlation with decreasing emissions",
                                 "Negative corrrelation with increasing emissions",
                                 "no correlation",
                                 "Positive correlation with decreasing emissions",
                                 "Positive correlation with increasing emissions"))+
  guides(color=FALSE)

GetCategory <- function(CountryName)
{
  category <- WorldInfo %>% filter(str_detect(string = CountryName,pattern = str_sub(short_name,1,4)) ) %>% select(RelationshipCategory)
  if(nrow(category) == 0){category = "No Info"}
  else if(nrow(category) == 1){category = category[[1]]}
  else{
    NegativeInc = paste0("Dominican Reblic|","Mauri|","Austria|","Guin|","Niger|","United States|","Mala|","The|","United A|","Turk|","South Afr|","New Z|","Slove")
    
    PosDec = paste0("United Kingdom|","Democratic Republic of the Congo|","Slova")
    
    NoInfo = paste0("North|","Dem. Rep. Korea|","French|","South S|","New C|","Green|","Dominica")
    
    both = paste0("Australia|","Greece")
    if(str_detect(CountryName,NegativeInc)){category = "Negative cross correlation with increasing emissions"}
  
    else if(str_detect(CountryName,PosDec)){category = "Positive cross correlations with decreasing emissions"}
    else if(str_detect(CountryName,NoInfo)){category = "No Info"}
    else if(str_detect(CountryName,both)){category = "Both negative and positive correlation depending on the lag"}
    else{category = "WTF"}
      }
  return(category)
}
world <- spData::world
world <- world %>%rowwise()%>%mutate(Relationship = GetCategory(name_long)) %>% ungroup() %>% st_as_sf()
world %>% tm_shape() + tm_fill(col = "Relationship",palette = c("purple","blue","grey","black","red","green"))

RegionBreakdown <- world %>% group_split(continent)
Africa <- RegionBreakdown[[1]] %>% tm_shape() + tm_fill(col = "Relationship",palette = c("purple","blue","grey","black","red","green"))
Asia <- RegionBreakdown[[3]] %>% tm_shape() + tm_fill(col = "Relationship",palette = c("purple","blue","grey","black","red","green"))
Europe <- RegionBreakdown[[4]] %>% tm_shape() + tm_fill(col = "Relationship",palette = c("purple","blue","grey","black","red","green"))
NorthAmerica <- RegionBreakdown[[5]]  %>% tm_shape() + tm_fill(col = "Relationship",palette = c("purple","blue","grey","black","red","green"))
Oceania <- RegionBreakdown[[6]]  %>% tm_shape() + tm_fill(col = "Relationship",palette = c("purple","blue","grey","black","red","green"))
SouthAmerica <- RegionBreakdown[[8]]  %>% tm_shape() + tm_fill(col = "Relationship",palette = c("purple","blue","grey","black","red","green"))



tut <- WorldDamageEmissions %>%as_tibble() %>% select(- country_code) %>% group_by(year) %>% summarize(averageRate = mean(adjusted_savings_particulate_emission_damage_percent_of_gni))
tut <-tut %>% as_tsibble(index = year)
tut <- tut %>% as_tsibble(index = year)
fit <- tut %>% model(arima = ARIMA(log(averageRate)))
fc <- fit %>% forecast(h = "25 years")
fcplot <- fc %>% autoplot(tut,level = NULL)


#Simulations
ss <- 1000
NumYears <- 10
NumCountries <- WorldInfo %>% filter(RelationshipCategory!="No Info") %>% nrow()
averageSignificanceLevel <- Reduce("+",AllSignificanceLevels)/length(AllSignificanceLevels)

pos <- averageSignificanceLevel
neg <- averageSignificanceLevel * -1
results <- tribble(~positiveResults,~negativeResults,~noneResults,~bothResults)
for(i in 1:ss){
  countrySSTest <-tribble(~positive,~negative,~none,~both)
  for(j in 1:NumCountries){

    currentSample <- rnorm(NumYears,mean = 0,sd = pos/2)

    negativeR <- sum(currentSample <= neg) > 0
    positiveR <- sum(currentSample >= pos) > 0
    
    noneR <- !(positiveR || negativeR)
    
    bothR <- negativeR && positiveR
    
    if(bothR)
    {
      negativeR <- FALSE
      positiveR <- FALSE
    }
    countrySSTest <- countrySSTest %>% add_row(positive = negativeR, negative = positiveR, none = noneR, both = bothR)
  }
  resultsEntry <- countrySSTest %>% summarise(positiveResults = sum(positive),negativeResults = sum(negative),noneResults = sum(none),bothResults = sum(both))
  results <- results %>% bind_rows(resultsEntry)
}
TrueResults <-  WorldInfo %>% filter(RelationshipCategory!="No Info") %>%
  summarise(both = sum(str_detect(RelationshipCategory,"Both")),
            positive = sum(str_detect(RelationshipCategory,"Positive") ) - both, 
            negative = sum(str_detect(RelationshipCategory,"Negative cross correlation")),
            none = sum(RelationshipCategory == "No correlation" )
            )

DunnSidak <- 1 - (1-0.05)^(1/165)
DunnSidakLow <- DunnSidak
DunnSidakHigh <- 1- DunnSidak
bothTrue <- TrueResults$both[1]
noneTrue <- TrueResults$none[1]
NegativeTrue <- TrueResults$negative[1]
PositiveTrue <- TrueResults$positive[1]

NoneSigTestLow <- results %>% filter(noneResults < noneTrue) %>% nrow()
NoneSigTestHigh <- results %>% filter(noneResults > noneTrue) %>% nrow()

BothSigTestLow <- results %>% filter(bothResults < bothTrue) %>% nrow()
BothSigTestHigh <- results %>% filter(bothResults > bothTrue) %>% nrow()

NegSigTestLow <- results %>% filter(negativeResults < NegativeTrue) %>% nrow()
NegSigTestHigh <- results %>% filter(negativeResults > NegativeTrue) %>% nrow()

PosSigTestLow <- results %>% filter(positiveResults < PositiveTrue) %>% nrow()
PosSigTestHigh <- results %>% filter(positiveResults > PositiveTrue) %>% nrow()

bothHist <-results %>% ggplot(aes(x = bothResults)) + geom_histogram(binwidth  = 1,
                                                                     col = "red",
                                                                     fill = "blue") + 
  xlab(" # of both results per simulation") + 
  ylab("# of results") + 
  ggtitle("simulation results for # of countries with both positive and negative cross correlations") + 
  geom_vline(xintercept = bothTrue)

PosHist <- results %>%ggplot(aes(x = positiveResults)) +  geom_histogram(binwidth  = 1,
                                                                         col = "red",
                                                                         fill = "blue") + 
  xlab(" # of positive results per simulation") + 
  ylab("# of results") + 
  ggtitle("simulation results for # of countries with positive cross correlations") + 
  geom_vline(xintercept = PositiveTrue)

NegHist <- results %>%ggplot(aes(negativeResults)) + geom_histogram(binwidth  = 1,
                                                                    col = "red",
                                                                    fill = "blue") + 
  xlab(" # of negative cross correlation countries per simulation") + 
  ylab("# of results") + 
  ggtitle("simulation results for # of countries with negative cross correlations") + 
  geom_vline(xintercept = NegativeTrue)

noneHist <- results %>%ggplot(aes(noneResults)) +  geom_histogram(binwidth  = 1,
                                                                  col = "red",
                                                                  fill = "blue") + 
  xlab(" # of no correlation results per simulation") + 
  ylab("# of results") + 
  ggtitle("simulation results for # of countries with no significant cross correlations") + 
  geom_vline(xintercept = noneTrue)


