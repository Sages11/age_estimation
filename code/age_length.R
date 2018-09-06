# notes ----
# age_length_estimation and analysis
# sarah.power@alaska.gov
# 9/05/2018

# load ----
library(tidyverse)
library(mixdist)
library(lubridate)
library(zoo) # to convert numeric date back to a number
library(here)
#source('code/distribution_functions.R')

# data ----
df <- read_csv('data/Chinooklengths19862016.csv') %>% 
  dplyr::select(
    Frequency, Haul_Year, Haul_FMP_Area, Species_Length, 
    Length_Species_Sex, Deployment_Date, Maturity_Description, 
    Retrieval_Latitude, Retrieval_Longitude) %>%
  rename_all(tolower) %>%
  rename(
    year = haul_year,
    area = haul_fmp_area,
    length = species_length,
    sex = length_species_sex,
    ddate = deployment_date,
    maturity = maturity_description,
    lat = retrieval_latitude,
    long = retrieval_longitude,)%>%
  mutate(
          ddate = as.Date(as.POSIXct(ddate, format = "%m/%d/%Y %H:%M")),
          day_of_year = yday(ddate),
          year = as.factor(year),
          area = as.factor(area),
          sex = as.factor(sex),
          maturity= as.factor(maturity)) 


