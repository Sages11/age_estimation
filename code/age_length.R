# notes ----
# age_length_estimation and analysis
# sarah.power@alaska.gov
# 9/05/2018

# load ----
library(here)
source('code/age_length_functions.R')

citation('mixdist')

# data ----
df1 <- read_csv('data/Chinooklengths19862016.csv') %>% 
  dplyr::select(
    Frequency, Haul_Year, Haul_FMP_Area, Haul_NMFS_Area, Species_Length, 
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
    long = retrieval_longitude) %>%
  mutate(ddate = mdy_hm(ddate),
          day_of_year = yday(ddate),
          month_of_year = month(ddate),
         season = case_when(month_of_year<7 ~ "A",
                            month_of_year>=7 ~"B"),# this not working
          #yyear = as.factor(yyear),
          area = as.factor(area),
          sex = as.factor(sex),
          maturity= as.factor(maturity)
          ) 

# analysis ----
year_vector <- sort(unique(age_df$yyear))
# remove years 1986-89 since data is too sparse to analyse.
year_vector <- year_vector[5:(length(year_vector)-1)]
season_vector <- c("A", "B")
#######################################################
# create a dataframe with information for each 1/2 year (Jan - June) & (July - Dec) (Or season A and Season B)
#######################################################
variable_names <-c("year", "season",
                   "pi1", "pi2", "pi3", "pi4", 
                   "mu1", "mu2", "mu3", "mu4",   
                   "sigma1", "sigma2", "sigma3", "sigma4", 
                   "pi.se1", "pi.se2", "pi.se3", "pi.se4", 
                   "mu.se1", "mu.se2", "mu.se3", "mu.se4",  
                   "sigma.se1", "sigma.se2", "sigma.se3", "sigma.se4",
                   "chi_pvalue", "chi_df")
variables <- length(variable_names)
iterations <- length(year_vector) *2 # *2 for season A and B 

data_all_years <- matrix(ncol = variables, nrow = iterations)

for(i in 1:iterations){
  for(j in 1:2){
    df <- data_prep_season(age_df, year_vector[i], season_vector[j])
    (fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75), sigma=c(3.42,5,5,5)), dist='norm')) 
                   #constr =mixconstr(consigma = "SFX", fixsigma = c(TRUE, FALSE, FALSE, TRUE)), iterlim=5000)) 
    plot(fitpro, main=year_vector[i], sub = season_vector[j])
    data_all_years[i,] <-c(year_vector[i], season_vector[j],
                           fitpro$parameters$pi, 
                           fitpro$parameters$mu, 
                           fitpro$parameters$sigma, 
                           fitpro$se$pi.se, 
                           fitpro$se$mu.se,
                           fitpro$se$sigma.se,
                           fitpro$P, fitpro$df)
  }

}

data_all_years <- data.frame(data_all_years)
for(i in 1:variables){
  names(data_all_years)[i]= variable_names[i]
}
class(data_all_years)
data_all_years 

#######################################################
# create a dataframe with information for each year
#######################################################

variable_names <-c("year", 
                   "pi1", "pi2", "pi3", "pi4", 
                   "mu1", "mu2", "mu3", "mu4",   
                   "sigma1", "sigma2", "sigma3", "sigma4", 
                   "pi.se1", "pi.se2", "pi.se3", "pi.se4", 
                   "mu.se1", "mu.se2", "mu.se3", "mu.se4",  
                   "sigma.se1", "sigma.se2", "sigma.se3", "sigma.se4",
                   "chi_pvalue", "chi_df")
variables <- length(variable_names)
iterations <- length(year_vector) # 

data_all_years <- matrix(ncol = variables, nrow = iterations)

for(i in 1:iterations){
  #i <- 14
  df <- data_prep(age_df, year_vector[i])
  (fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75), sigma=c(3.42,5,5,7.05)), dist='norm', #)) 
                 constr =mixconstr(consigma = "SFX", fixsigma = c(TRUE, FALSE, FALSE, TRUE)), iterlim=5000)) 
  plot(fitpro, main=year_vector[i])
  data_all_years[i,] <-c(year_vector[i], 
                fitpro$parameters$pi, 
                fitpro$parameters$mu, 
                fitpro$parameters$sigma, 
                fitpro$se$pi.se, 
                fitpro$se$mu.se,
                fitpro$se$sigma.se,
                fitpro$P, fitpro$df)
}

data_all_years <- data.frame(data_all_years)
for(i in 1:variables){
  names(data_all_years)[i]= variable_names[i]
}
class(data_all_years)
data_all_years 

mean(data_all_years$sigma1[13:19], na.omit="TRUE")
mean(data_all_years$sigma4[c(9,10,22,23,27)], na.omit="TRUE")
mean(data_all_years$sigma4, na.omit="TRUE")


df <- data_prep(age_df, 2015)
(fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75,85), sigma=c(5,5,5,5,5)), dist='gamma', iterlim=5000)) 
plot(fitpro)
summary(fitpro)

