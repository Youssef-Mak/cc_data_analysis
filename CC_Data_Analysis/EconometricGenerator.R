library(knitr)
library(rmarkdown)
library(markdown)
library(janitor)
library(stringr)
library(readxl)
library(dplyr)

Indicators <- read_excel("data/raw/WDIExcel.xlsx",sheet="Series") %>% clean_names() %>% mutate(topic = str_split(topic,":")[[1]][2])
for(i in 1:nrow(Indicators))
{
  row <- Indicators[i,]
  title <- row[["indicator_name"]]
  Topic <-row[["topic"]]
  SizeOfString <- nchar(paste0(title,Topic))
  if(SizeOfString > 200)
  {
    title <- str_remove_all(row[["series_code"]],".")
  }
  Directory <- paste0("./data/Reports/",Topic)
  rmarkdown::render("IndicatorReportWParams.Rmd",
                    output_file = str_remove_all(title,":"),
                    output_dir = Directory,
                    params = row)
}

