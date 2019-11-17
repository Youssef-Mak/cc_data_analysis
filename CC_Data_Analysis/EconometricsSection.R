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
Econometrics <- read_excel("data/raw/WDIEXCEL.xlsx",sheet = "Data")
Indicators <- read_excel("data/raw/WDIEXCEL.xlsx",sheet = "Series")
Econometrics <- Econometrics %>% clean_names() %>% select(-indicator_code) %>%
  pivot_longer(cols = starts_with("x"),names_to = "year",values_to = "value") %>%
  pivot_wider(names_from = indicator_name, values_from = "value") %>%
  mutate(year = as.numeric(str_remove(year,"x")))
