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

country_emissions_filename <- "data/raw/CAIT-Country-GHG-Emissions.csv"

activity_value_filename <- "data/raw/value_added_per_act.csv"

regulation_filename <- "data/raw/law_search/data.csv"

emission_tax_filename <- "data/raw/emission_tax.csv"

empl_per_act_filename <- "data/raw/employment_per_act.csv"

co2_per_sect <- "data/raw/emission_per_sect/global-carbon-dioxide-emissions-by-sector.csv"

meth_per_sect <- "data/raw/emission_per_sect/methane-emissions-by-sector-gg-coe.csv"

no2_per_sect <- "data/raw/emission_per_sect/nitrous-oxide-emissions-by-sector.csv"

gge_per_sect <- "data/raw/emission_per_sect/greenhouse-gas-emissions-by-sector.csv"


# FUNCTION DEFINITIONS ---------------------------------------------------------------

# Find minimal distance of acf between two bounds
findSignificance <- function(acf, upper_bound, lower_bound) {
  print("ACF")
  print(acf)
  print("Upp bound")
  print(upper_bound)
  print("Lower bound")
  print(lower_bound)
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
  print("P value")
  print(adf_p)
  # Low p-value indicating stationary
  while ((!is.na(adf_p)) & (adf_p > 0.05)) {
    print("--------------------------- IN adf LOOP --------------------------- ")
    time_series <- time_series %>% diff()
    diff <- diff + 1 
    adf_test <- time_series %>% adf.test()
    adf_p <- adf_test$p.value
  }
  
  kpss_test <- time_series %>% kpss.test()
  kpss_p <- kpss_test$p.value
  print("P value")
  print(kpss_p)
  # High p-value indicating non-trend-stationary
  while ((!is.na(kpss_p)) & kpss_p < 0.05) {
    print("--------------------------- IN kpss LOOP --------------------------- ")
    time_series <- time_series %>% diff()
    diff <- diff + 1
    kpss_test <- time_series %>% kpss.test()
    kpss_p <- kpss_test$p.value
  }
  
  print("Done stationarizing")
  return(list(ts = time_series, diff = diff))
}

# Plots Time Series
plot_time_series <- function(em_growth_df, country_name, activity_ss = "Total Growth") {
  
  country_growth_em <- em_growth_df %>% 
    filter(activity == activity_ss) %>% 
    filter(country == country_name)
  
  min_year <- min(country_growth_em$year)
  print(min_year)
  
  country_growth_ts_obj <- ts(country_growth_em$value, start = min_year) %>% stationarize()
  country_Growth_TimeSeries <- country_growth_ts_obj$ts
  country_Growth_diff <- country_growth_ts_obj$diff
  print("-------------- DIFFF FOR GROWTH ------------------")
  print(country_Growth_diff)
  
  country_Emission_ts_obj <- ts(country_growth_em$total_ghg_emissions_mtco2e, start = min_year) %>% stationarize()
  country_Emission_TimeSeries <- country_Emission_ts_obj$ts
  country_Emission_diff <- country_Emission_ts_obj$diff
  print("-------------- DIFFF FOR Emissions ------------------")
  print(country_Emission_diff)
  
  col_name <- paste("Country Growth TimeSeries With Difference ",country_Growth_diff) # I want to put difference as col name so its  in plot title
  bound_ts <- cbind("Country Growth TimeSeries With Difference " = country_Growth_TimeSeries, 
                    "Country Emission TimeSeries With Difference " = country_Emission_TimeSeries)
  p <- autoplot(bound_ts, facets=TRUE) +
    xlab("Year") + ylab("") +
    ggtitle(paste("Country Growth vs Emission for ", country_name))
  
  return(p)
}

# Find Cross Correlation
findTotalCrossCorr <- function(em_growth_df, country_name, activity_ss = "Total Growth") {
  
  country_growth_em <- em_growth_df %>% 
    filter(activity == activity_ss) %>% 
    filter(country == country_name)
  
  if (dim(country_growth_em)[1] == 0) {
    return(tribble(~ACF, ~LAG, ~country, ~dist, NA, NA, country_name, NA)) 
  }
  
  min_year <- min(country_growth_em$year)
  print(min_year)
  
  print("Making Time series")
  country_growth_ts_obj <- ts(country_growth_em$value, start = min_year) %>% stationarize()
  print("Made ts Call")
  country_Growth_TimeSeries <- country_growth_ts_obj$ts
  country_Growth_diff <- country_growth_ts_obj$diff
  print(country_Growth_TimeSeries)
  
  country_Emission_ts_obj <- ts(country_growth_em$total_ghg_emissions_mtco2e, start = min_year) %>% stationarize()
  print("Made ts Calls")
  country_Emission_TimeSeries <- country_Emission_ts_obj$ts
  country_Emission_diff <- country_Emission_ts_obj$diff
  print(country_Emission_TimeSeries)
  print("Done Making Time Series")
  
  print("making ccf")
  ccf2 <- ccf(country_Growth_TimeSeries, country_Emission_TimeSeries)
  print("Done making ccf")
  
  diff_length <- sqrt((country_Growth_diff^2) + (country_Emission_diff^2))
  
  # Critical Values at 5% conf Level = +-2/sqrt(n) (assuming normal distribution of ACF) (alpha = 0.05)
  crit_val_lb <- (-2 / sqrt(ccf2$n.used))
  crit_val_ub <- (2 / sqrt(ccf2$n.used))
  
  print("making xcorr")
  xcorrDF <- data.frame(ACF = ccf2$acf, LAG = ccf2$lag) %>% 
    mutate(country = country_name) %>%
    mutate(activity = activity_ss) %>% 
    mutate(diff = (diff_length)) %>% 
    mutate(dist = sapply(ACF, findSignificance, upper_bound = crit_val_ub, lower_bound = crit_val_lb)) %>% 
    filter(ACF >= crit_val_ub | ACF <= crit_val_lb)
  
  print("Done making xcorr")
  return(xcorrDF)
  
}

processCsvData <- function(csv_filename) {
  csv_df <- read_csv(csv_filename) %>% 
    clean_names()
  return(csv_df)
}



# Reading Datasets --------------------------------------------------------

# Employment per activity filename
empl_per_act_df <- processCsvData(empl_per_act_filename)

# Country Emission DF (mtCO2)
# TODO : Add sector emission columns  
total_country_ghg_emissions <- read_csv(country_emissions_filename, skip=2) %>% 
  clean_names() %>% 
  select(country, year, total_ghg_emissions_mtco2e = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e) %>% 
  na.omit()
detail_country_ghg_emission <- read_csv(country_emissions_filename, skip=2) %>% 
  clean_names() %>% 
  mutate(total_ghg_emissions_mtco2e = total_ghg_emissions_including_land_use_change_and_forestry_mt_co_e) %>% 
  na.omit()

# Regulation per country DF
reg_df <- processCsvData(regulation_filename)

# Environmental tax per country DF
env_tax_df <- processCsvData(emission_tax_filename) %>% 
  dplyr::rename(year = time) %>% 
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
env_tax_df <- env_tax_df %>% mutate(subject = mapvalues(subject, env_subject_mappings$code, env_subject_mappings$name))

country_mappings <-
  tribble(
    ~world_data, ~country_emissions,
    # 'North Korea', 'Korea (North)',
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
# env_tot_tax_df <- env_tax_df %>% 
#   filter(measure == "PC_TOT_TAX")


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
  geom_smooth(se = FALSE, method = lm) +
  facet_grid(rows = vars(activity)) +
  labs(
    x = "Year",
    y = "Growth Added",
    colour = 'Activity Category',
    title = "Economical Growth Added vs Year"
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

# Loop through countries of interest
countries_ccf_res <- tribble()
for (i in 1:nrow(countries_of_interest)) {
  country <- countries_of_interest[i,]$country
  print(country)
  ccf_res <- findTotalCrossCorr(em_tot_growth_merged_df, country)
  countries_ccf_res <- bind_rows(countries_ccf_res, ccf_res)
}

countries_ccf_res %>%
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
  # ggrepel::geom_text_repel()

# Top Significantly Auto-Correlated Country (ITS SOUTH KOREA)
top_xcorr_country <- countries_ccf_res[order(-countries_ccf_res$ACF),] %>% head(1) %>% select(country)
print(top_xcorr_country)

# LETS LOOK AT ITS CROSS CORRELATION INDIVIDUALLY
countries_ccf_res %>% filter(country == "South Korea") %>%
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
plot_time_series(em_tot_growth_merged_df, top_xcorr_country[1,]$country)

# LETS LOOK AT ITS SUB SECTOR GROWTH PER YEAR

coun_growth_add_df %>%
  filter(location == top_xcorr_country[1,]$country) %>% 
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

# LETS GO THROUGH EACH SUBSECTOR AND FIND CROSS CORRELATIONS WITH EMMISIONS

south_kor_df <- em_growth_ss_merged_df %>% 
  filter(country == "South Korea") %>% 
  filter(activity != "Total Growth")

# lets get all the subsectors
sub_sectors <- unique(south_kor_df$activity)
print(sub_sectors)

sk_ss_ccf_res <- tribble()
for (ss in sub_sectors) {
  print(ss)
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





aus_growth_em <- em_growth_ss_merged_df %>% filter(activity == "Agriculture and Fishery") %>% filter(country == "Australia")

# First let's assume that Value added and environmental tax are independent to one another
# to perform Pearson corelation test
# We also assume there is a linear relationship between the two

# Assume That both variables are continuous and linearly related (appears linear barring outliers)
plot(aus_growth_em$value, aus_growth_em$agriculture_mt_co2e)

# Assume variables follow a bivariate normal distribution
qqnorm(aus_growth_em$value)
qqline(aus_growth_em$value)

qqnorm(aus_growth_em$agriculture_mt_co2e)
qqline(aus_growth_em$agriculture_mt_co2e)

# Assume variances are homogeneous (no cone shape scatter plot)

# No Major outliers (check scatter plot)

# Correlation test
cor.test(aus_growth_em$value, aus_growth_em$agriculture_mt_co2e, method = "pearson", data=aus_val_tax)

# Output -> Non Significant negative corelation

# Let's plot two time series 

# one for Added growth wrt years
Agriculture_Growth_TimeSeries <- ts(aus_growth_em$value, start = 1996)
Agriculture_Emission_TimeSeries <- ts(aus_growth_em$agriculture_mt_co2e, start = 1996)
bound_ts <- cbind( Agriculture_Growth_TimeSeries, Agriculture_Emission_TimeSeries)
autoplot(bound_ts, facets=TRUE) +
  xlab("Year") + ylab("Hello") +
  ggtitle(paste("Time Series for Australian Agricultural Subsector", 5))

scatter_ts_plot <-
  qplot(Agriculture_Growth_TimeSeries, Agriculture_Emission_TimeSeries, data=as.data.frame(bound_ts)) +
  ylab("Emission (mtCO2)") + xlab("Added Growth")

# Cross corelation testing for time series

# First test for stationarity (the way the time series changes does not change over time)

adf.test(Agriculture_Growth_TimeSeries) # Low p-value indicating stationary

kpss.test(Agriculture_Growth_TimeSeries) # High p-value indicating non-trend-stationary

adf.test(Agriculture_Emission_TimeSeries) # High p-value indicating non-stationary

kpss_t <- kpss.test(Agriculture_Emission_TimeSeries) # High p-value indicating non-trend-stationary

diff_agr_grwt_ts <- diff(Agriculture_Growth_TimeSeries)

diff_agr_em_ts <- diff(Agriculture_Emission_TimeSeries)

# Re-test for Stationarity

diff_agr_grwt_ts %>% diff() %>% diff() %>% diff() %>% diff() %>% diff() %>% adf.test() # Low p-value indicating stationary

diff_agr_grwt_ts %>% diff() %>% diff() %>% diff() %>% diff() %>% diff() %>% kpss.test() # High p-value indicating non-trend-stationary

diff_agr_grwt_ts <- diff_agr_grwt_ts %>% diff() %>% diff() %>% diff() %>% diff() %>% diff() 

# growth IS Trend-Stationary

diff_agr_em_ts %>% diff() %>% diff() %>% diff() %>% diff() %>% diff() %>% adf.test() # Low p-value indicating stationary

diff_agr_em_ts %>% diff() %>% diff() %>% diff() %>% diff() %>% diff() %>% kpss.test() # High p-value indicating non-trend-stationary

diff_agr_em_ts <- diff_agr_em_ts %>% diff() %>% diff() %>% diff() %>% diff() %>% diff()

# Emission is growth stationary

# Cross correlation

ccf2 <- ccf(diff_agr_grwt_ts, diff_agr_em_ts)

lb <- (-2 / sqrt(ccf2$n.used))

up <- (2/ sqrt(ccf2$n.used))

data.frame(ACF = ccf2$acf, LAG = ccf2$lag)

xcorrDF <- data.frame(ACF = ccf2$acf, LAG = ccf2$lag) %>% filter(ACF >= up | ACF <= lb)


print(ccf2$acf)
# Negatively correlated in -2 lag
# Positively correlated in -1 and -3 lag
# Not sure if i took differencing into account TODO: apply ARIMA MOde



# Australia value added per year for Agriculture Sector
# aus_val_tax %>%
#   ggplot(
#     aes(
#       x = (year),
#       y = (value),
#       color = activity
#     )
#   ) +
#   geom_point(size = 2) +
#   geom_smooth(se = FALSE, method = lm) +
#   facet_grid(rows = vars(activity)) +
#   labs(
#     x = "Year",
#     y = "Value Added",
#     colour = 'Activity Category',
#     title = "Economical Value Added vs Year In Australia"
#   )
# 
# aus_val_tax %>% 
#   ggplot(
#     aes(
#       x = (year),
#       y = (total_env_tax),
#       color = activity
#     )
#   ) +
#   geom_point(size = 2) +
#   geom_smooth(se = FALSE, method = lm) +
#   facet_grid(rows = vars(activity)) +
#   labs(
#     x = "Year",
#     y = "Environment Tax Income",
#     colour = 'Activity Category',
#     title = "Environment Tax Income vs Year In Australia"
#   )



