library(tidyverse)
library(FNN)
library(caret)


weather <-
  read.csv("WeatherSample.csv", na.strings = c("", " ", "NA"))

#handle missing data
weather <- weather[!weather$WindDir9am=="Not Available",]
weather <- weather[!weather$WindDir3pm=="Not Available",]

# Dummy Vars

# WindGustDir
dummiesWind <-
  as.data.frame(model.matrix( ~ 0 + WindGustDir, data = weather))
dummiesWind <- dummiesWind[, -1]

# Direction 9am

dummies9am <-
  as.data.frame(model.matrix( ~ 0 + WindDir9am, data = weather))
dummies9am <- dummies9am[, -1]

# Direction 3 pm

dummies3pm <-
  as.data.frame(model.matrix( ~ 0 + WindDir3pm, data = weather))
dummies3pm <- dummies3pm[, -1]

# Rain today

rainToday <-
  as.data.frame(model.matrix( ~ 0 + RainToday, data = weather))
rainToday <- rainToday[, -1]

# Rain Tomorrow

rainTomorrow <-
  as.data.frame(model.matrix( ~ 0 + RainTomorrow, data = weather))
rainTomorrow <- rainTomorrow[, -1]

# Funciton to scale all numeric data
scaleData <- function(x){
  scaledD <- (x - min(x)) / (max(x) - min(x))
  return(scaledD)
}


# Scaling the data
# We dont want any catageorical data, remove all string
scaled_weather <- weather %>%
  select(-c(
    Date,
    WindGustDir,
    WindDir9am,
    WindDir3pm,
    RainToday,
    RainTomorrow
  )) %>%
  # Apply all numeric columns to the scaleData function
  apply(2, scaleData) %>%
  data.frame()

# Final Data
final.weather <- cbind(scaled_weather,dummiesWind,dummies9am,dummies3pm,rainToday,rainTomorrow)

# Partition data
t(t(names(final.weather)))

# Leave out predictors

set.seed(1)

#EVERYTHING GOT FIXED HERE
rownames(final.weather) <- NULL
############

train.index <- sample(nrow(final.weather), nrow(final.weather)*0.6)
valid.index <- as.numeric(setdiff(rownames(final.weather), train.index))

weather.train <- final.weather[train.index,]
weather.valid <- final.weather[-train.index,]

# 
# #Leaving out predictors
weather <- final.weather 
weather.train <- weather.train 
weather.valid <- weather.valid 

t(t(names(weather.train)))
#Classification Functions


library(DiscriMiner)
# Does not work with learn test uncommented
weather.da <- linDA(
  final.weather[,1:62],
  final.weather$rainTomorrow,
  validation = "learntest",
  learn = train.index, 
  test = valid.index
  )


weather.da$functions
#Probabilities
# classification scores, predicted classes, and probabilities
# compute probabilities manually
propensity.high <- exp(weather.da$scores[, "1"]) / (exp(weather.da$scores[, "0"]) + exp(weather.da$scores[, "1"]))
da.results <- data.frame(Actual = weather.valid $rainTomorrow, weather.da$classification, weather.da$scores,
                         propensity.high = propensity.high)
options(scipen = 999)
head(da.results, 25)

weather.dat.clasas <- weather.da$classification

confusionMatrix(weather.da$classification,
                as.factor(weather.valid$rainTomorrow),
                positive = "1")



