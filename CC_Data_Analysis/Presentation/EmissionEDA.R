library(plyr)
library(dplyr)
library(countrycode)
library(tidyr)
library(readxl)
library(janitor)
library(ggplot2)
library(ggfortify)
library(skimr)
library(stringr)
library(ggrepel)
library(tidyverse)
library(tsibble)
library(tseries)
library(countrycode)
library(plotly)
library(ggrepel)
library(ggthemes)


# Datasets ----------------------------------------------------------------

country_emissions_filename <- "../data/raw/CAIT-Country-GHG-Emissions.csv"

activity_value_filename <- "../data/raw/value_added_per_act.csv"

regulation_filename <- "../data/raw/law_search/data.csv"

emission_tax_filename <- "../data/raw/emission_tax.csv"

empl_per_act_filename <- "../data/raw/employment_per_act.csv"

co2_per_sect <- "../data/raw/emission_per_sect/global-carbon-dioxide-emissions-by-sector.csv"

meth_per_sect <- "../data/raw/emission_per_sect/methane-emissions-by-sector-gg-coe.csv"

no2_per_sect <- "../data/raw/emission_per_sect/nitrous-oxide-emissions-by-sector.csv"

gge_per_sect <- "../data/raw/emission_per_sect/greenhouse-gas-emissions-by-sector.csv"


# FUNCTION DEFINITIONS ---------------------------------------------------------------

# Find minimal distance of acf between two bounds
findSignificance <- function(acf, upper_bound, lower_bound) {
  if ( acf >= upper_bound) {
    return(acf - upper_bound)
  } else if (acf <= lower_bound) {
    return(abs(acf - lower_bound))
  } else {
    return((-1) * min(abs(upper_bound - acf), abs(lower_bound - acf)))
  }
}



# Trend Stationarize a time series
stationarize <- function(time_series) {
  
  adf_test <- time_series %>% adf.test()
  adf_p <- adf_test$p.value
  diff <- 0
  # Low p-value indicating stationary
  while ((!is.na(adf_p)) & (adf_p > 0.05)) {
    time_series <- time_series %>% diff()
    diff <- diff + 1 
    adf_test <- time_series %>% adf.test()
    adf_p <- adf_test$p.value
  }
  
  kpss_test <- time_series %>% kpss.test()
  kpss_p <- kpss_test$p.value
  # High p-value indicating non-trend-stationary
  while ((!is.na(kpss_p)) & kpss_p < 0.05) {
    time_series <- time_series %>% diff()
    diff <- diff + 1
    kpss_test <- time_series %>% kpss.test()
    kpss_p <- kpss_test$p.value
  }
  
  return(list(ts = time_series, diff = diff))
}

# Plots Time Series
plot_ccf <- function(em_growth_df, country_name, activity_ss = "Total Growth") {
  
  country_growth_em <- em_growth_df %>% 
    filter(activity == activity_ss) %>% 
    filter(country == country_name)
  
  min_year <- min(country_growth_em$year)
  
  country_growth_ts_obj <- ts(country_growth_em$value, start = min_year) %>% stationarize()
  country_Growth_TimeSeries <- country_growth_ts_obj$ts
  country_Growth_diff <- country_growth_ts_obj$diff
  
  country_Emission_ts_obj <- ts(country_growth_em$total_ghg_emissions_mtco2e, start = min_year) %>% stationarize()
  country_Emission_TimeSeries <- country_Emission_ts_obj$ts
  country_Emission_diff <- country_Emission_ts_obj$diff
  
  
  col_name <- paste("Country Growth TimeSeries With Difference ",country_Growth_diff) # I want to put difference as col name so its  in plot title
  bound_ts <- cbind("Country Growth TimeSeries With Difference "  = country_Growth_TimeSeries, 
                    "Country Emission TimeSeries With Difference " = country_Emission_TimeSeries)
  p <- autoplot(bound_ts, facets=TRUE) +
    xlab("Year") + ylab("") +
    ggtitle(paste("Country Growth vs Emission for ", country_name))
  
  ccf2 <- ccf(country_Growth_TimeSeries, country_Emission_TimeSeries)
  
  return(ccf2)
}

plot_ts <- function(em_growth_df, country_name, activity_ss = "Total Growth", diff = TRUE) {
  country_growth_em <- em_growth_df %>% 
    filter(activity == activity_ss) %>% 
    filter(country == country_name)
  
  min_year <- min(country_growth_em$year)
  
  if (diff){
    country_Growth_TimeSeriesObj <- ts(country_growth_em$value, start = min_year) %>% stationarize()
    country_Growth_TimeSeries <- country_Growth_TimeSeriesObj$ts
    
    country_Emission_TimeSeriesObj <- ts(country_growth_em$total_ghg_emissions_mtco2e, start = min_year) %>% stationarize()
    country_Emission_TimeSeries <- country_Emission_TimeSeriesObj$ts
  } else {
    country_Growth_TimeSeries <- ts(country_growth_em$value, start = min_year) 
    country_Emission_TimeSeries <- ts(country_growth_em$total_ghg_emissions_mtco2e, start = min_year)
  }
  
  bound_ts <- cbind("Country Growth TimeSeries"  = country_Growth_TimeSeries, 
                    "Country Emission TimeSeries" = country_Emission_TimeSeries)
  p <- autoplot(bound_ts, facets=TRUE) +
    xlab("Year") + ylab("") +
    ggtitle(paste("Country Growth vs Emission for ", country_name))
  
  
  return(p) 
}
# Find Cross Correlation
findTotalCrossCorr <- function(em_growth_df, country_name, activity_ss = "Total Growth", rev = FALSE) {
  
  country_growth_em <- em_growth_df %>% 
    filter(activity == activity_ss) %>% 
    filter(country == country_name)
  
  if ((dim(country_growth_em)[1] == 0) | (nrow(country_growth_em) < 10)) {
    return(tribble(~ACF, ~LAG, ~country, ~dist, NA, NA, country_name, NA)) 
  }
  
  min_year <- min(country_growth_em$year)
  
  country_growth_ts_obj <- ts(country_growth_em$value, start = min_year) %>% stationarize()
  country_Growth_TimeSeries <- country_growth_ts_obj$ts
  country_Growth_diff <- country_growth_ts_obj$diff
  
  country_Emission_ts_obj <- ts(country_growth_em$total_ghg_emissions_mtco2e, start = min_year) %>% stationarize()
  country_Emission_TimeSeries <- country_Emission_ts_obj$ts
  country_Emission_diff <- country_Emission_ts_obj$diff
  
  if (rev) {
    # Movement in x(emissions) May have an effect on y(growth)
    ccf2 <- ccf(country_Emission_TimeSeries, country_Growth_TimeSeries)
  } else {
    # Movement in x(Growth) May have an effect on y(emission)
    ccf2 <- ccf(country_Growth_TimeSeries, country_Emission_TimeSeries)
  }
  
  diff_length <- sqrt((country_Growth_diff^2) + (country_Emission_diff^2))
  
  # Critical Values at 5% conf Level = +-2/sqrt(n) (assuming normal distribution of ACF) (alpha = 0.05)
  crit_val_lb <- (-2 / sqrt(ccf2$n.used))
  crit_val_ub <- (2 / sqrt(ccf2$n.used))
  
  xcorrDF <- data.frame(ACF = ccf2$acf, LAG = ccf2$lag) %>% 
    mutate(country = country_name) %>%
    mutate(activity = activity_ss) %>% 
    mutate(diff = (diff_length)) %>% 
    mutate(dist = sapply(ACF, findSignificance, upper_bound = crit_val_ub, lower_bound = crit_val_lb)) %>% 
    filter(ACF >= crit_val_ub | ACF <= crit_val_lb)
  
  return(xcorrDF)
  
}

processCsvData <- function(csv_filename) {
  csv_df <- read_csv(csv_filename) %>% 
    clean_names()
  return(csv_df)
}



# Reading Datasets --------------------------------------------------------

# Employment per activity filename
# empl_per_act_df <- processCsvData(empl_per_act_filename) %>% 
#   dplyr::rename(activity = subject) %>% 
#   dplyr::mutate(location = countrycode::countrycode(location, 'iso3c', 'country.name.en'))

# Country Emission DF (mtCO2)
total_country_ghg_emissions <- read_csv(country_emissions_filename, skip=2) %>% 
  clean_names() %>% 
  select(country, year, total_ghg_emissions_mtco2e = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e) %>% 
  na.omit()
detail_country_ghg_emission <- read_csv(country_emissions_filename, skip=2) %>% 
  clean_names() %>% 
  mutate(total_ghg_emissions_mtco2e = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e) %>% 
  na.omit()

# Environmental tax per country DF
env_tax_df <- processCsvData(emission_tax_filename) %>% 
  dplyr::rename(year = time) %>% 
  dplyr::rename(activity = subject) %>% 
  select(-starts_with("indicator")) %>%
  dplyr::mutate(location = countrycode::countrycode(location, 'iso3c', 'country.name.en'))

# Per Country Activity Value Added DF
coun_growth_add_df <- processCsvData(activity_value_filename) %>% 
  dplyr::rename(activity = subject) %>% 
  dplyr::rename(year = time) %>% 
  select(-starts_with("indicator")) %>% # Remove Indicator 
  dplyr::mutate(location = countrycode::countrycode(location, 'iso3c', 'country.name.en')) %>%
  filter(measure == "AGRWTH")


# Mapping Values ----------------------------------------------------------

# Rename value added sector names to convene to emission dataset sector name
# Industry Mappings
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
coun_growth_add_df <- coun_growth_add_df %>% mutate(activity = mapvalues(activity, subject_mappings$code, subject_mappings$name)) 



# Rename Environment Tax Sectors
env_subject_mappings <- tribble(
  ~code, ~name,
  "ENRG", "Energy",
  "MOTORVEH", "Motored Vehicles",
  "OTH", "Other",
  "TOT", "Total Environmental Tax Generated"
)
env_tax_df <- env_tax_df %>% mutate(activity = mapvalues(activity, env_subject_mappings$code, env_subject_mappings$name))


empl_subject_mappings <- tribble(
  ~code, ~name,
  "AGR", "Agriculture",
  "CONSTR", "Construction",
  "INDUSCONSTR", "Industry Including Construction",
  "MFG", "Manufacturing",
  "SERV", "Services"
)
empl_per_act_df <- empl_per_act_df %>% mutate(activity = mapvalues(activity, empl_subject_mappings$code, empl_subject_mappings$name))

country_mappings <-
  tribble(
    ~world_data, ~country_emissions,
    'South Korea', 'Korea, Rep. (South)', 
    'Russia', 'Russian Federation',
    'United Kingdom', 'UK',
    'Czechia', 'Czech Republic'
  )

total_country_ghg_emissions <- total_country_ghg_emissions %>% 
  mutate(country = mapvalues(country, country_mappings$country_emissions, country_mappings$world_data))

detail_country_ghg_emission <- detail_country_ghg_emission %>% 
  mutate(country = mapvalues(country, country_mappings$country_emissions, country_mappings$world_data))



# Get Countries of Interest -----------------------------------------------


all_countries_em <- unique(detail_country_ghg_emission$country)
all_countries_em
countries_growth <- unique(coun_growth_add_df$location)
countries_growth

countries_of_interest <- 
  tibble(countries = countries_growth, count = unlist(map(countries_growth, function(x) sum(all_countries_em %in% x)))) %>% 
  filter(count == 1) %>% 
  select(countries) %>% 
  dplyr::rename(country = countries)


# Measure used should be Percent of total tax
env_tot_tax_df <- env_tax_df %>%
  filter(measure == "PC_TOT_TAX") %>% 
  filter(activity == "Total Environmental Tax Generated")


# International Subsector Growth Overview --------------------------------------------------

# Per Activity Growth Added DF(total growth added per year per country)
int_growth_added_df <- coun_growth_add_df %>% 
  dplyr::filter(activity!="Total Growth") %>% 
  dplyr::group_by(activity, year) %>% 
  dplyr::summarise(average_val = mean(value))


# Start with different subsectors for same country 

# International Average value added per year
int_growth_added_df %>%
  ggplot(
    aes(
      x = (year),
      y = (average_val),
      color = activity
    )
  ) +
  geom_line() +
  geom_smooth(se = FALSE, method = lm, size = 0.5) +
  facet_grid(rows = vars(activity)) +
  labs(
    x = "Year",
    y = "Average Growth Added (% of Total Growth)",
    colour = 'Activity Category',
    title = "International Economical Growth Added vs Year"
  )

# Per Average Value added DF(global value added per year)
coun_tot_growth_add_df <- coun_growth_add_df %>% 
  filter(activity == "Total Growth")
# %>% dplyr::summarise(average_val = mean(value))

# Emission and TOTAL growth per country merged df
em_tot_growth_merged_df <- dplyr::left_join(detail_country_ghg_emission, coun_tot_growth_add_df, by = c("country" = "location", "year" = "year")) %>% 
  tidyr::drop_na(value)

# Emission and SUBSECTOR growth per country merged df
em_growth_ss_merged_df <- dplyr::left_join(detail_country_ghg_emission, coun_growth_add_df, by = c("country" = "location", "year" = "year")) %>% 
  tidyr::drop_na(value)

# Emission and Total Environment Tax Rev(perc of GDP)
em_env_tax_merged_df <- dplyr::left_join(total_country_ghg_emissions, env_tot_tax_df, by = c("country" = "location", "year" = "year")) %>% 
  tidyr::drop_na(value)

# CCF ANALYSIS FOR COUNTRY EMISSION VS ENV TAX ----------------------------

# Time series plot for visualization


# We're especially concerned about the lag in this case
countries_em_tax <- unique(em_env_tax_merged_df$country)
countries_em_tax_ccf_res <- tribble()
for (country in countries_em_tax) {
  ccf_res <- findTotalCrossCorr(em_env_tax_merged_df, country, "Total Environmental Tax Generated")
  countries_em_tax_ccf_res <- bind_rows(countries_em_tax_ccf_res, ccf_res)
}


# notice how points tend to be left of 0 indicating that 
# movement in total env tax revenue in time t-x has an effect on emission in time t
countries_em_tax_ccf_res %>%
  ggplot(
    aes(
      x = (LAG),
      y = (ACF),
      color = country
    )
  ) +
  geom_point(size = 3) +
  labs(
    x = "LAG (Delay between correlation)",
    y = "ACF (Auto-correlation between Time-Series)",
    colour = 'Country',
    title = "Significant Cross Correlation Between Environmental Tax and Total Emissions Time Series"
  )

countries_em_tax_ccf_res %>% 
  ggplot(
    aes(
      x = (LAG)
    )
  ) +
  geom_histogram() +
  labs(
    x = "LAG (Delay between correlation)",
    # y = "ACF (Auto-correlation between Time-Series)",
    colour = 'Country',
    title = "Significant Cross Correlation Lag Distribution Between Environmental Tax and Total Emissions Time Series"
  )



# CCF ANALYSIS FOR COUNTRY GROWTH vs EMISIONS ------------------------------------------------------------

# Loop through countries of interest
countries_gr_em_ccf_res <- tribble()
for (i in 1:nrow(countries_of_interest)) {
  country <- countries_of_interest[i,]$country
  ccf_res <- findTotalCrossCorr(em_tot_growth_merged_df, country, rev = TRUE)
  countries_gr_em_ccf_res <- bind_rows(countries_gr_em_ccf_res, ccf_res)
}

countries_gr_em_ccf_res %>%
  ggplot(
    aes(
      x = (LAG),
      y = (ACF),
      color = country
    )
  ) +
  geom_point(size = 3) +
  labs(
    x = "LAG (Delay between correlation)",
    y = "ACF (Auto-correlation between Time-Series)",
    colour = 'Country',
    title = "Significant Cross Correlation Between Total Growth and Total Emissions Time Series"
  )

countries_gr_em_ccf_res %>% 
  ggplot(
    aes(
      x = (LAG)
    )
  ) +
  geom_histogram() +
  labs(
    x = "LAG (Delay between auto-correlation)",
    colour = 'Country',
    title = "Significant Cross Correlation Lag Distribution Between Country Growth and Total Emissions Time Series"
  )

countries_gr_em_ccf_res %>% filter(LAG == 0) %>% 
  ggplot(
    aes(
      x = (ACF)
    )
  ) +
  geom_density() +
  labs(
    x = "ACF (Auto-Correlation between Time Series)",
    colour = 'Country',
    title = "Significant Cross Correlation ACF Distribution Between Country Growth and Total Emissions Time Series "
  )


# Top Significantly Auto-Correlated Country (ITS SOUTH KOREA)
top_xcorr_countries <- countries_gr_em_ccf_res[order(-countries_gr_em_ccf_res$ACF),] %>% head(3)

# LETS LOOK AT ITS CROSS CORRELATION INDIVIDUALLY
top_xcorr_countries %>%
  ggplot(
    aes(
      x = (LAG),
      y = (ACF),
      color = country
    )
  ) +
  geom_point(size = 3) +
  labs(
    x = "LAG (Delay between correlation)",
    y = "ACF (Auto-correlation between Time-Series)",
    colour = 'Country',
    title = "Significant Cross Correlation Between Total Growth and Total Emissions Time Series"
  )

countries_gr_em_ccf_res %>% filter(country == "United States") %>%
  ggplot(
    aes(
      x = (LAG),
      y = (ACF),
      color = country
    )
  ) +
  geom_point(size = 3) +
  labs(
    x = "LAG (Delay between correlation)",
    y = "ACF (Auto-correlation between Time-Series)",
    colour = 'Country',
    title = "Significant Cross Correlation Between Total Growth and Total Emissions Time Series"
  )

countries_gr_em_ccf_res %>% filter(country == "Brazil") %>%
  ggplot(
    aes(
      x = (LAG),
      y = (ACF),
      color = country
    )
  ) +
  geom_point(size = 3) +
  labs(
    x = "LAG (Delay between correlation)",
    y = "ACF (Auto-correlation between Time-Series)",
    colour = 'Country',
    title = "Significant Cross Correlation Between Total Growth and Total Emissions Time Series"
  )

# LETS LOOK AT ITS TIME SERIES(SO SIMILAR WOW)
plot_ts(em_tot_growth_merged_df, top_xcorr_countries[1,]$country, diff=TRUE)

# LETS LOOK AT ITS SUB SECTOR GROWTH PER YEAR

coun_growth_add_df %>%
  filter(location == top_xcorr_countries[1,]$country) %>% 
  ggplot(
    aes(
      x = (year),
      y = (value),
      color = activity
    )
  ) +
  geom_line() +
  geom_smooth(se = FALSE, method = lm) +
  facet_grid(rows = vars(activity)) +
  labs(
    x = "Year",
    y = "Growth Added",
    colour = 'Activity Category',
    title = "Economical Growth Added vs Year"
  )


south_kor_df <- em_growth_ss_merged_df %>% 
  filter(country == "South Korea") %>% 
  filter(activity != "Total Growth")

# lets get all the subsectors
sub_sectors <- unique(south_kor_df$activity)

sk_ss_ccf_res <- tribble()
for (ss in sub_sectors) {
  ccf_res <- findTotalCrossCorr(south_kor_df, "South Korea", ss)
  sk_ss_ccf_res <- bind_rows(sk_ss_ccf_res, ccf_res)
}

# Cross Correlations between Growth(per activity) and Total Emissions
sk_ss_ccf_res %>%
  ggplot(
    aes(
      x = (LAG),
      y = (ACF),
      color = activity
    )
  ) +
  geom_point(size = 3) +
  labs(
    x = "LAG (Delay between correlation)",
    y = "ACF (Auto-correlation between Time-Series)",
    colour = 'Activity',
    title = "Significant Cross Correlation Between Subsector Growth and Total Emissions Time Series for South Korea"
  )




