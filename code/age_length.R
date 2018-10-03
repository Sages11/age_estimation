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
year_vector <- sort(unique(age_df$yyear))
# remove years 1986-88 since data is too sparse to analyse.
year_vector <- year_vector[26:length(year_vector)]

# create a dataframe with information for each year

variable_names <-c("year", 
                   "pi1", "pi.se1", "mu1", "mu.se1", "sigma1", "sigma.se1", 
                   "pi2", "pi.se2", "mu2", "mu.se2", "sigma2", "sigma.se2", 
                   "pi3", "pi.se3", "mu3", "mu.se3", "sigma3", "sigma.se3", 
                   "pi4", "pi.se4", "mu4", "mu.se4", "sigma4", "sigma.se4", 
                   "chi_pvalue", "chi_df")
variables <- length(variable_names)
iterations <- length(year_vector) # 

data_all_years <- matrix(ncol = variables, nrow = iterations)

for(i in 1:iterations){
  df <- data_prep(age_df, year_vector[i])
  (fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75), sigma=c(5,5,5,5)), dist='gamma', iterlim=5000)) 
  plot(fitpro, main=year_vector[i])
  data_all_years[i,] <-c(year_vector[i], 
                fitpro$parameters$pi[1], fitpro$se$pi.se[1], 
                fitpro$parameters$mu[1], fitpro$se$mu.se[1],
                fitpro$parameters$sigma[1], fitpro$se$sigma.se[1],
                fitpro$parameters$pi[2], fitpro$se$pi.se[2], 
                fitpro$parameters$mu[2], fitpro$se$mu.se[2],
                fitpro$parameters$sigma[2], fitpro$se$sigma.se[2],
                fitpro$parameters$pi[3], fitpro$se$pi.se[3], 
                fitpro$parameters$mu[3], fitpro$se$mu.se[3],
                fitpro$parameters$sigma[3], fitpro$se$sigma.se[3],
                fitpro$parameters$pi[4], fitpro$se$pi.se[4], 
                fitpro$parameters$mu[4], fitpro$se$mu.se[4],
                fitpro$parameters$sigma[4], fitpro$se$sigma.se[4],
                fitpro$P, fitpro$df)
}

data_all_years <- data.frame(data_all_years)
for(i in 1:variables){
  names(data_all_years)[i]= variable_names[i]
}

length(data_all_years[i,])
summary(fitpro)

names(data_all_years)[] <- variable_names
class(data_all_years)

df <- data_prep(age_df, 2015)
(fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75), sigma=c(5,5,5,5)), dist='gamma', iterlim=5000)) 
plot(fitpro)
summary(fitpro)
fitpro$parameters$pi[1]

iterations = 10
variables = 2

output <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  output[i,] <- runif(2)
  
}

output
output[i,] <-c(i, 
               1, fitpro$parameters$pi[1], fitpro$se$pi.se[1], 
               fitpro$parameters$mu[1], fitpro$se$mu.se[1],
               fitpro$parameters$sigma[1], fitpro$sigma$mu.se[1],
               2, fitpro$parameters$pi[2], fitpro$se$pi.se[2], 
               fitpro$parameters$mu[2], fitpro$se$mu.se[2],
               fitpro$parameters$sigma[2], fitpro$sigma$mu.se[2],
               3, fitpro$parameters$pi[3], fitpro$se$pi.se[3], 
               fitpro$parameters$mu[3], fitpro$se$mu.se[3],
               fitpro$parameters$sigma[3], fitpro$sigma$mu.se[3],
               4, fitpro$parameters$pi[4], fitpro$se$pi.se[4], 
               fitpro$parameters$mu[4], fitpro$se$mu.se[4],
               fitpro$parameters$sigma[4], fitpro$sigma$mu.se[4],
               fitpro$p, fitpro$df)
