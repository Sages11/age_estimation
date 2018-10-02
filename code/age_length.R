# notes ----
# age_length_estimation and analysis
# sarah.power@alaska.gov
# 9/05/2018

# load ----
library(here)
source('code/age_length_functions.R')

# data ----
df1 <- read_csv('data/Chinooklengths19862016.csv') %>% 
  dplyr::select(
    Frequency, Haul_Year, Haul_FMP_Area, Species_Length, 
    Length_Species_Sex, Deployment_Date, Maturity_Description, 
    Retrieval_Latitude, Retrieval_Longitude) 
age_df <- df1%>%
  rename_all(tolower) %>%
  dplyr::rename(
    yyear = haul_year,
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
          #yyear = as.factor(yyear),
          area = as.factor(area),
          sex = as.factor(sex),
          maturity= as.factor(maturity)
          ) 


# analysis ----
df <- data_prep(age_df, 2003)

(fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75), sigma=c(5,5,5,5)), dist='gamma', iterlim=5000)) 
plot(fitpro)

