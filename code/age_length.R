# notes ----
# age_length_estimation and analysis
# sarah.power@alaska.gov
# 9/05/2018

# load ----
library(here)
source('code/age_length_functions.R')

citation('mixdist')

setwd("H:\\sarah\\Tools\\Utool\\dsim\\length analysis\\age_estimation")
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
          maturity= as.factor(maturity),
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

par(mfrow= c(4,2))

for(i in 1:iterations){
  for(j in 1:2){
    df <- data_prep_season(age_df, year_vector[i], season_vector[j])
    
    if (season_vector[j] == "A"){
      (fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75), sigma=c(3.42,4,4,4)), dist='norm')) 
       # constr =mixconstr(consigma = "SEQ")))#, fixsigma = c(TRUE, FALSE, FALSE, TRUE)), iterlim=5000)) 
    } else{ 
      (fitpro <- mix(as.mixdata(df), mixparam(mu=c(45,60,75,85), sigma=c(5,5,5,5), pi = c(0.05, 0.50, .435, .015)), dist='norm'))
       # constr = mixconstr(conpi = "PFX", fixpi = c(FALSE, FALSE, FALSE, TRUE))))                     
    }
    
    plot(fitpro, main=year_vector[i], sub = season_vector[j])
    data_all_years[(i+ iterations/2*(j-1)),] <-c(year_vector[i], season_vector[j],
                           fitpro$parameters$pi, 
                           fitpro$parameters$mu, 
                           fitpro$parameters$sigma, 
                           fitpro$se$pi.se, 
                           fitpro$se$mu.se,
                           fitpro$se$sigma.se,
                           fitpro$P, fitpro$df)
    data_all_years <- data_all_years %>%
  }

}

data_all_years <- data.frame(data_all_years)
for(i in 1:variables){
  names(data_all_years)[i]= variable_names[i]
}
class(data_all_years)
data_all_years 

# Below a display of particular years for which an age length key exists.
display <- data_all_years[c(8:10, 35:37, 17:19, 44:46), 1:6]
round(dispaly[,3:6],2)
# By visually inspecting the graphs produced for each season of each year, 
# and seeing how well the individual distributions jive with the combined and original distributions, 
# and by comparing the percentage of each age to those that would be produced using age length keys,
# it appears that some years do a very good job all round using the generic models above. However some could do with some more years specific
# parameter starting values.


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
data_all_years [, 3:28] <- lapply(data_all_years [, 3:28], factorconvert)
class(data_all_years)
data_all_years 


#To look at certain years in particular:

display <- data_all_years[c(8:10, 35:37, 17:19, 44:46), 1:6]
display[, 3:6] <-  round(display[,3:6],2)
display

mean(data_all_years$sigma1[13:19], na.omit="TRUE")
mean(data_all_years$sigma4[c(9,10,22,23,27)], na.omit="TRUE")
mean(data_all_years$sigma4, na.omit="TRUE")


df <- data_prep(age_df, 2015)
(fitpro <- mix(as.mixdata(df), mixparam(mu=c(30,50,65,75,85), sigma=c(5,5,5,5,5)), dist='gamma', iterlim=5000)) 
plot(fitpro)
summary(fitpro)

display2 <- data_all_years[c(22:26, 49:53), 1:6]
display2[, 3:6] <-  round(display[,3:6],2)

#To look at certain years in particular:

df <- data_prep_season(age_df, 1997, "A")
(fitpro <- mix(as.mixdata(df), mixparam(mu=c(40,50,70,85), sigma=c(3.42,4,4,4)), dist='norm')) 
plot(fitpro, main=year_vector[i], sub = season_vector[j])
summary(fitpro)

i <- 1997
mu=c(45,65,75,85)
df <- data_prep_season(age_df, i, "B")
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5)), dist='norm', iterlim=5000))
plot(fitpro, main= paste(i," B"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4]))
summary(fitpro)

i <- 1998
mu=c(40,52,68,80)
df <- data_prep_season(age_df, i, "A") #,pi = c(0.001, 0.50, .435, .0015)
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(3,5,5,5)), dist='norm',constr =mixconstr(consigma = "SEQ"), iterlim=5000))
plot(fitpro, main= paste(i," B"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4]))
summary(fitpro)

i <- 1998
mu=c(30,47,63,75,85)
df <- data_prep_season(age_df, i, "B")
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5,5)),constr =mixconstr(consigma = "SEQ"), dist='norm', iterlim=5000))
plot(fitpro, main= paste(i," B"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4]))
summary(fitpro)


i <- 1999
mu=c(30,50,63,73)
df <- data_prep_season(age_df, i, "A") #,pi = c(0.001, 0.50, .435, .0015)
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(3,5,5,5)), dist='norm'))#,constr =mixconstr(consigma = "SEQ"), iterlim=5000))
plot(fitpro, main= paste(i," A"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)

i <- 1999
mu=c(30,47,63,75,85)
df <- data_prep_season(age_df, i, "B")
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5,5)),constr =mixconstr(consigma = "SEQ"), dist='norm', iterlim=5000))
plot(fitpro, main= paste(i," B"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)

i <- 2000
mu=c(27,48,60,73,85)
df <- data_prep_season(age_df, i, "A") #,pi = c(0.001, 0.50, .435, .0015)
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,4,4,3,4)), dist='norm'))#,constr =mixconstr(consigma = "SEQ"), iterlim=5000))
plot(fitpro, main= paste(i," A"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)

i <- 2000
mu=c(30,47,63,75,85)
df <- data_prep_season(age_df, i, "B")
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5,5)),constr =mixconstr(consigma = "SEQ"), dist='norm', iterlim=5000))
plot(fitpro, main= paste(i," B"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)

# Skip to 2006
par(mfrow= c(4,2))

i <- 2006
mu=c(30,52,63,80)
df <- data_prep_season(age_df, i, "A") #,pi = c(0.001, 0.50, .435, .0015)
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5)), dist='norm', constr =mixconstr(consigma = "SEQ"), iterlim=5000))
plot(fitpro, main= paste(i," A"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)

i <- 2006
mu=c(30,47,63,75,85)
df <- data_prep_season(age_df, i, "B")
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5,5)),constr =mixconstr(consigma = "SEQ"), dist='norm', iterlim=5000))
plot(fitpro, main= paste(i," B"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)

i <- 2007
mu=c(30,52,63,80)
df <- data_prep_season(age_df, i, "A") #,pi = c(0.001, 0.50, .435, .0015)
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5)), dist='norm', constr =mixconstr(consigma = "SEQ"), iterlim=5000))
plot(fitpro, main= paste(i," A"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)

i <- 2007
mu=c(30,47,63,75,85)
df <- data_prep_season(age_df, i, "B")
(fitpro <- mix(as.mixdata(df), mixparam(mu, sigma=c(5,5,5,5,5)),constr =mixconstr(consigma = "SEQ"), dist='norm', iterlim=5000))
plot(fitpro, main= paste(i," B"), sub = paste("chisq = ", round(fitpro$chisq,1), "df = ", fitpro$df, " starting mu = ", mu[1], mu[2], mu[3], mu[4], mu[5]))
summary(fitpro)