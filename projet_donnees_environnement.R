#------------------------------------------
# Extreme Values Theory - Project
# Environment data
#------------------------------------------

### List of authors
# DE CAMPIGNEULLES Charles
# GUERAUD Benjamin
# BRAULT Tom

# We used "blended data" here (corrected data)

rm(list = ls())

### Packages to use

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble) # allows to put columns in row headers
library(tseries) # for adf.test
library(lubridate)  # For date manipulation
library(evd) # for GPD choice of threshold

# Environment (to replace with your folder)
setwd("/home/id2244/3A/theorie_valeurs_extremes/projet_donnees_environnement")
# Alternative solution: open a project (.Rproj) and save it in the same folder 

#------------------------------------------
### Importation of datasets
#------------------------------------------

### Importation of precipitation dataset

precipitation = read.table("precipitation_data.txt", 
                            header = TRUE, 
                            skip = 19, 
                            sep = ",")

precipitation = precipitation[,2:4]

colnames(precipitation) = c("date", "precipitation_amount","fiability_precipitation_amount")

# Precipitation amounts are given in 0.1mm. We use a more natural scale with mm in the following way
precipitation[precipitation$fiability_precipitation_amount == 0,]$precipitation_amount = precipitation[precipitation$fiability_precipitation_amount == 0,]$precipitation_amount / 10

#------------------------------------------
### Importation of cloud cover dataset

cloud_cover = read.table("cloud_cover_data.txt", 
                           header = TRUE, 
                           skip = 19, 
                           sep = ",")

cloud_cover = cloud_cover[,2:4]

colnames(cloud_cover) = c("date", "cloud_cover", "fiability_cloud_cover")

#------------------------------------------
### Importation of humidity dataset

humidity = read.table("humidity_data.txt", 
                      header = TRUE, 
                      skip = 19, 
                      sep = ",")

humidity = humidity[,2:4]

colnames(humidity) = c("date", "humidity", "fiability_humidity")

#------------------------------------------
# Convert column "date" with format Date
precipitation$date = as.Date(as.character(precipitation$date), format = "%Y%m%d")
cloud_cover$date = as.Date(as.character(cloud_cover$date), format = "%Y%m%d")
humidity$date = as.Date(as.character(humidity$date), format = "%Y%m%d")

#------------------------------------------
# Missing or unreliable data
missing_values_precipitation = precipitation[precipitation$fiability != 0, ]
# 1 missing value for precipitation

missing_values_cloud_cover = cloud_cover[cloud_cover$fiability != 0, ]
summary(missing_values_cloud_cover) # only missing values here
# 4619 missing values for cloud cover

missing_values_humidity = humidity[humidity$fiability != 0, ]
summary(missing_values_humidity) # only missing values here
# 5172 missing values for humidity

#------------------------------------------
# Global dataframe with 3 variables: precipitation, humidity, cloud_coverage
global_rain_data = full_join(precipitation, full_join(cloud_cover, humidity, by = "date"), by = "date")

# Additional missing values compared to precipitation data
sum(is.na(global_rain_data$cloud_cover)) # 61 missing values compared to precipitation data
sum(is.na(global_rain_data$humidity)) # 638 missing values compared to precipitation data

# Dates as row headers (if necessary)
# precipitation = column_to_rownames(precipitation, var = "date")
# cloud_cover = column_to_rownames(cloud_cover, var = "date")
# humidity = column_to_rownames(humidity, var = "date")
# global_rain_data = column_to_rownames(global_rain_data, var = "date")

#------------------------------------------
### Preliminary analysis of time series
#------------------------------------------

### Preprocessing the data - Treatment of missing values: imputation?
# Question: Are the missing values occuring completely at random or
# in particular situations (high rain for example)?
# We recall that we had one missing value for precipitation data,
# occuring on 2006-01-07. What were the conditions at this date?

## Time series with missing values
# Precipitation
ggplot(data = precipitation, aes(x = date, y = precipitation_amount)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(title = "Time series of precipitation", x = "Date", y = "Precipitation amount (mm)") +
  theme_minimal()

humidity[humidity$date == "2006-01-07",] # missing value also
cloud_cover[cloud_cover$date == "2006-01-07",] # cloud cover = 8 oktas at this date
max(cloud_cover$cloud_cover) # 8 oktas
# The missing value of precipitation occurs when the cloud cover is 8 oktas (maximum)
# Thus, the precipitation amount could have been high. We will impute the mean precipitation amount (if available)
# for dates such that the cloud cover is maximum (8 oktas).
mean_precipitation_high_cloud_cover = mean(global_rain_data[(global_rain_data$cloud_cover == 8) & !(is.na(global_rain_data$cloud_cover)),]$precipitation_amount
)
mean_precipitation_high_cloud_cover
# about 7.9 mm of precipitation in average -> we impute the missing value with this mean value
precipitation[precipitation$date == "2006-01-07",]$precipitation_amount = mean_precipitation_high_cloud_cover
global_rain_data[global_rain_data$date == "2006-01-07",]$precipitation_amount = mean_precipitation_high_cloud_cover

#------------------------------------------
# Cloud cover
ggplot(data = cloud_cover, aes(x = date, y = cloud_cover)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(title = "Time series of cloud cover", x = "Date", y = "Cloud cover (oktas)") +
  theme_minimal()
# We notice that no measure is given at the end of the series (after 1 March 2012)
# We must remove them

cloud_cover = cloud_cover[cloud_cover$date < as.Date("2012-03-01"),]

# Remaining missing values
count(cloud_cover[cloud_cover$fiability != 0,]) # 53 missing values

#------------------------------------------
# Humidity
ggplot(data = humidity, aes(x = date, y = humidity)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(title = "Time series of humidity", x = "Date", y = "Humidity (%)") +
  theme_minimal()
# We notice that no measure is given at the end of the series (after 29 February 2012)
# We must remove them

humidity = humidity[humidity$date < as.Date("2012-02-29"),]

# Remaining missing values
count(humidity[humidity$fiability != 0,]) # 604 missing values (too much!)

par(mfrow = c(1,2))
acf(cloud_cover$cloud_cover)
pacf(cloud_cover$cloud_cover)
dev.off()

#------------------------------------------
# New variable: season for analysis by seasons

# Precipitation
precipitation$season <- with(precipitation, 
                             ifelse((format(date, "%m-%d") >= "12-21" & format(date, "%m-%d") <= "12-31") | 
                                      (format(date, "%m-%d") >= "01-01" & format(date, "%m-%d") <= "03-20"), "Winter",
                                    ifelse(format(date, "%m-%d") >= "03-21" & format(date, "%m-%d") <= "06-20", "Spring",
                                           ifelse(format(date, "%m-%d") >= "06-21" & format(date, "%m-%d") <= "09-20", "Summer",
                                                  ifelse(format(date, "%m-%d") >= "09-21" & format(date, "%m-%d") <= "12-20", "Fall", NA)))))

# Split the data frame by season
precipitation_winter = precipitation[precipitation$season == "Winter", ]
precipitation_spring = precipitation[precipitation$season == "Spring", ]
precipitation_summer = precipitation[precipitation$season == "Summer", ]
precipitation_fall = precipitation[precipitation$season == "Fall", ]


# Create a new data frame with seasonal mean values of precipitation for each year
precipitation_seasons = precipitation %>%
  # For winter, the related year will be the one associated with January, February and the beginning of March
  mutate(season_year = ifelse(season == "Winter" & month(date) == 12, year(date)+1, year(date))) %>%
  # Write in the order: Winter, Spring, Summer, Fall instead of alphabetical order
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))) %>%
  group_by(season_year, season) %>%
  # Keep only the mean precipitation for each group
  summarise(mean_precipitation_amount = mean(precipitation_amount, na.rm = TRUE), .groups = 'drop')

#------------------------------------------
# Cloud cover
cloud_cover$season <- with(cloud_cover, 
                             ifelse((format(date, "%m-%d") >= "12-21" & format(date, "%m-%d") <= "12-31") | 
                                      (format(date, "%m-%d") >= "01-01" & format(date, "%m-%d") <= "03-20"), "Winter",
                                    ifelse(format(date, "%m-%d") >= "03-21" & format(date, "%m-%d") <= "06-20", "Spring",
                                           ifelse(format(date, "%m-%d") >= "06-21" & format(date, "%m-%d") <= "09-20", "Summer",
                                                  ifelse(format(date, "%m-%d") >= "09-21" & format(date, "%m-%d") <= "12-20", "Fall", NA)))))

# Split the data frame by season
cloud_cover_winter = cloud_cover[cloud_cover$season == "Winter", ]
cloud_cover_spring = cloud_cover[cloud_cover$season == "Spring", ]
cloud_cover_summer = cloud_cover[cloud_cover$season == "Summer", ]
cloud_cover_fall = cloud_cover[cloud_cover$season == "Fall", ]


#------------------------------------------
# Humidity
humidity$season <- with(humidity, 
                             ifelse((format(date, "%m-%d") >= "12-21" & format(date, "%m-%d") <= "12-31") | 
                                      (format(date, "%m-%d") >= "01-01" & format(date, "%m-%d") <= "03-20"), "Winter",
                                    ifelse(format(date, "%m-%d") >= "03-21" & format(date, "%m-%d") <= "06-20", "Spring",
                                           ifelse(format(date, "%m-%d") >= "06-21" & format(date, "%m-%d") <= "09-20", "Summer",
                                                  ifelse(format(date, "%m-%d") >= "09-21" & format(date, "%m-%d") <= "12-20", "Fall", NA)))))

# Split the data frame by season
humidity_winter = humidity[humidity$season == "Winter", ]
humidity_spring = humidity[humidity$season == "Spring", ]
humidity_summer = humidity[humidity$season == "Summer", ]
humidity_fall = humidity[humidity$season == "Fall", ]


#------------------------------------------
### Time series without missing values
# Precipitation
par(mfrow = c(1,1))
ggplot(data = precipitation, aes(x = date, y = precipitation_amount)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(title = "Time series of precipitation in Valencia (1937-2024)", x = "Date", y = "Precipitation amount (mm)") +
  theme_minimal()
precipitation_ts = ts(precipitation$precipitation_amount)
plot(precipitation_ts, col = "darkblue", ylab = "Precipitation amount (mm)",
     main = "Time series of precipitation in Valencia (1937-2024)") # time in units

## Analysis of stationarity
par(mfrow=c(1,2))
acf(precipitation_ts, main = "ACF de la série de précipitation")
pacf(precipitation_ts, main = "PACF de la série de précipitation")
dev.off()


# Augmented Dickey-Fuller test
adf.test(precipitation_ts) # Low p-value: 0.01 (we reject H0 with confidence level 95%)
# Recall: H0 corresponds to the existence of a unit root, i.e., to the non-stationarity case
# So, we can assume that the initial time series is already stationary

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(precipitation_ts) # High p-value: 0.1 (we do not reject H0 with confidence level 95%)
# Recall: H0 corresponds to the stationarity case
# So, we can assume that the initial time series is already stationary


## Analysis of seasonality: does the precipitation amount depend on the season?

mean(precipitation_winter$precipitation_amount)
mean(precipitation_spring$precipitation_amount)
mean(precipitation_summer$precipitation_amount)
mean(precipitation_fall$precipitation_amount)
# more rain during fall, less during summer

# Smoothed periodogram with daily values
spec.pgram(precipitation_ts, spans=2, main="Smoothed periodogram of the precipitation series")
# no clear seasonality based on daily data (no particularly high spectral estimation)
# The precipitation amount does not seem to depend a lot on the season we consider.


# Smoothed periodogram with monthly values
spec.pgram(precipitation_seasons$mean_precipitation_amount, spans=2, main="Smoothed periodogram of the precipitation series")
# We have two peak located for frequencies omega = 1/4 and omega = 1/2,
# suggesting a seasonal periodicity this time or, at least, a semestrial periodicity

#------------------------------------------
# Cloud cover
cloud_cover_ts = ts(cloud_cover$cloud_cover)
plot(cloud_cover_ts)
mean(precipitation_ts[precipitation_ts >= 0])

#------------------------------------------
### Analysis of the extreme values - Precipitation
#------------------------------------------

max(precipitation_ts)
# maximum value of 262.6 mm recorded on 14 October 1957

#------------------------------------------
## GEV analysis
#------------------------------------------

# Étape 1 : Filtrer les données (supprime NA et valeurs <= 0) -> plus nécessaire ici (déjà fait)
precipitation_clean <- precipitation %>%
  filter(!is.na(precipitation_amount) & precipitation_amount > 0)

# Étape 2 : Groupement par année et saison
# Convertir la variable `date` en année et utiliser `season`
precipitation_clean <- precipitation_clean %>%
  mutate(
    year = as.numeric(format(date, "%Y"))  # Extraire l'année pour chaque date
  )

# Calcul des maxima par saison et année
max_per_season <- precipitation_clean %>%
  group_by(year, season) %>%
  summarize(Max_precipitation = max(precipitation_amount, na.rm = TRUE), .groups = "drop")

# Afficher un aperçu des maxima par saison
head(max_per_season)

# Étape 3 : Ajuster un modèle GEV aux maxima saisonniers
fitted <- fgev(max_per_season$Max_precipitation, nsloc = NULL, prob = NULL, 
               std.err = TRUE, corr = FALSE, method = "BFGS", warn.inf = TRUE)

# Résumé des résultats
summary(fitted)

# Affichage des diagnostics du modèle
par(mfrow = c(1, 1))
plot(fitted)

# Étape 4 : Estimation des paramètres du modèle ajusté
fitted$estimate

# Calcul des intervalles de confiance pour 'loc'
IC_lower_loc <- fitted$estimate['loc'] - qnorm(0.975) * fitted$std.err['loc']
IC_upper_loc <- fitted$estimate['loc'] + qnorm(0.975) * fitted$std.err['loc']
IC_loc <- cbind(IC_lower_loc, IC_upper_loc)

# Calcul des intervalles de confiance pour 'scale'
IC_lower_scale <- fitted$estimate['scale'] - qnorm(0.975) * fitted$std.err['scale']
IC_upper_scale <- fitted$estimate['scale'] + qnorm(0.975) * fitted$std.err['scale']
IC_scale <- cbind(IC_lower_scale, IC_upper_scale)

# Calcul des intervalles de confiance pour 'shape'
IC_lower_shape <- fitted$estimate['shape'] - qnorm(0.975) * fitted$std.err['shape']
IC_upper_shape <- fitted$estimate['shape'] + qnorm(0.975) * fitted$std.err['shape']
IC_shape <- cbind(IC_lower_shape, IC_upper_shape)

# Afficher les intervalles de confiance
list(
  loc = IC_loc,
  scale = IC_scale,
  shape = IC_shape
)

# Étape 5 : Tracer le profil de vraisemblance
plot(profile(fitted))

# Calculer les intervalles de confiance avec `confint()`
confint(fitted)  # Basés sur la méthode par défaut
confint(profile(fitted))  # Basés sur le profil de vraisemblance

# Étape 6 : Calcul des niveaux de retour
# Exemple pour une période de retour de 10 ans (par saison)
rl(fitted)

# Calcul manuel pour une période de retour spécifique (par exemple, 10 ans)
period <- 10
# Période de retour
level_10_years <- qgev(1 - 1/(4 * period), 
                       fitted$estimate["loc"], 
                       fitted$estimate["scale"], 
                       fitted$estimate["shape"])
level_10_years

#------------------------------------------
## GPD analysis
#------------------------------------------

## Choice of the threshold

# mrlplot: Empirical Mean Residual Life Plot
par(mfrow = c(1,1))
mrlplot(precipitation_ts, 
        main = "",
        xlab = "Threshold (mm)",
        col = "darkblue")
abline(v = 20, col="darkred")
abline(v = 80, col = "darkred")
# thresholds between 20 and 80 based on the graph

# tcplot: Threshold Choice Plot
par(mfrow = c(1,1))
tcplot(precipitation_ts, tlim = c(10,100), which=1, xlab = "Threshold (mm)") # tlim: interval of thresholds
grid()
abline(v = 10, col="darkred")
abline(v = 25, col = "darkred")
# thresholds between 10 and 25 may be acceptable based on the graph

par(mfrow = c(1,1))
tcplot(precipitation_ts, tlim = c(10,100), which=2, xlab = "Threshold (mm)") # tlim: interval of thresholds
grid()
abline(v = 10, col="darkred")
abline(v = 25, col = "darkred")
# thresholds between 10 and 25 may be acceptable based on the graph

# overall, best threshold values in [20,25]

sum(precipitation_ts >= 25)
sum(precipitation_ts >= 20)

# choice of threshold: 25 mm

## GPD fitting

precipitation_gpd_fit = fpot(precipitation_ts, 25, npp = 365)

par(mfrow = c(2,2))
plot(precipitation_gpd_fit)

par(mfrow = c(1,1))
plot(precipitation_gpd_fit, which = 1) # Probability Plot
plot(precipitation_gpd_fit, which = 2, ylim=c(0,400)) # Quantile-quantile plot
plot(precipitation_gpd_fit, which = 4, ylim=c(0,400)) # Return Level Plot
# both plots look acceptable

# Number of exceedances with this threshold
n_exceedances = sum(precipitation_ts > 25)
n_exceedances
# 351 exceedances in total

# Parameters estimations and confidence interval
precipitation_gpd_fit
confint(precipitation_gpd_fit)
# Both parameters seem significant as the value 0 does not lie in
# any confidence interval.

## Return periods and return levels

inverse_function_gpd_shape0 = function(x,tau,xi){
  return(tau * ((1-x)^(-xi) -1)/ xi)}

return_level_gpd_shape0 = function(T,tau,xi){
  u = 0.03
  lambda = length(precipitation_gpd_fit$exceedances)/length(precipitation_ts) # proportion of threshold exceedances
  return(u+inverse_function_gpd_shape0(1-1/(T*lambda),tau,xi))
}
# T: return period (days)
max(precipitation_ts)
# Return level for a return period of 2 years
return_level_gpd_shape0(T = 2*365, tau = as.numeric(precipitation_gpd_fit$estimate["scale"]), 
                        xi = as.numeric(precipitation_gpd_fit$estimate["shape"]))
# Return level corresponding to a period of return of 20 years: 48.8mm
# with the GEV, we found a return level of 172.9mm approximately

# Return level for a return period of 10 years
return_level_gpd_shape0(T = 10*365, tau = as.numeric(precipitation_gpd_fit$estimate["scale"]),
                        xi = as.numeric(precipitation_gpd_fit$estimate["shape"]))
# Return level corresponding to a period of return of 10 years: 118.2mm
# with the GEV, we found a return level of 367mm approximately

