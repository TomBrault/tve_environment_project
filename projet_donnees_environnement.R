#------------------------------------------
# Projet de théorie des valeurs extrêmes
# Données environnementales
#------------------------------------------

# On utilisera ici des "blended data" (données corrigées)

rm(list = ls())

### Packages à utiliser

library(dplyr)
library(tidyr)
library(forcats)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(class)
library(tibble) # allows to put columns in row headers
library(tseries) # for adf.test
library(lubridate)  # For date manipulation

# Environnement
setwd("/home/id2244/3A/theorie_valeurs_extremes/projet_donnees_environnement")

#------------------------------------------
### Importation des jeux de données
#------------------------------------------

### Importation du jeu de données de précipitation

precipitation = read.table("precipitation_data.txt", 
                            header = TRUE, 
                            skip = 19, 
                            sep = ",")

precipitation = precipitation[,2:4]

colnames(precipitation) = c("date", "precipitation_amount","fiability_precipitation_amount")

# Precipitation amounts are given in 0.1mm. We use a more natural scale with mm in the following way
precipitation[precipitation$fiability_precipitation_amount == 0,]$precipitation_amount = precipitation[precipitation$fiability_precipitation_amount == 0,]$precipitation_amount / 10

#------------------------------------------
### Importation du jeu de données de couverture nuageuse (cloud cover)

cloud_cover = read.table("cloud_cover_data.txt", 
                           header = TRUE, 
                           skip = 19, 
                           sep = ",")

cloud_cover = cloud_cover[,2:4]

colnames(cloud_cover) = c("date", "cloud_cover", "fiability_cloud_cover")

#------------------------------------------
### Importation du jeu de données d'humidité

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
acf(precipitation_ts)
pacf(precipitation_ts)
dev.off()

adf.test(precipitation_ts) # Low p-value: 0.01 (we reject H0 with confidence level 95%)
# Recall: H0 corresponds to the existence of a unit root, i.e., the series is not stationary
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

#------------------------------------------
## GEV analysis
#------------------------------------------

#------------------------------------------
## GPD analysis
#------------------------------------------
