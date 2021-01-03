weather.df <- read.csv("WeatherSample.csv", header = TRUE, stringsAsFactors = TRUE)

summary(weather.df)
weather.df <- weather.df[,-1]
# create a binary dummy variable for RainTomorrow
weather.df$RainTomDum <- ifelse(weather.df$RainTomorrow == "Yes",1,0)

# create box plots showing the distribution of all numeric
# variables by RainTomorrow
par(mfrow = c(2,2))
boxplot(weather.df$MinTemp ~ weather.df$RainTomorrow)
boxplot(weather.df$MaxTemp ~ weather.df$RainTomorrow)
boxplot(weather.df$Rainfall ~ weather.df$RainTomorrow)
boxplot(weather.df$Evaporation ~ weather.df$RainTomorrow)
boxplot(weather.df$Sunshine ~ weather.df$RainTomorrow)
boxplot(weather.df$WindGustSpeed ~ weather.df$RainTomorrow)
boxplot(weather.df$WindSpeed9am ~ weather.df$RainTomorrow)
boxplot(weather.df$WindSpeed3pm ~ weather.df$RainTomorrow)
boxplot(weather.df$Humidity9am ~ weather.df$RainTomorrow)
boxplot(weather.df$Humidity3pm ~ weather.df$RainTomorrow)
boxplot(weather.df$Pressure9am ~ weather.df$RainTomorrow)
boxplot(weather.df$Pressure3pm ~ weather.df$RainTomorrow)
boxplot(weather.df$Cloud9am ~ weather.df$RainTomorrow)
boxplot(weather.df$Cloud3pm ~ weather.df$RainTomorrow)
boxplot(weather.df$Temp9am ~ weather.df$RainTomorrow)
boxplot(weather.df$Temp3pm ~ weather.df$RainTomorrow)

# create contingency tables and perform chi-squared tests for independence
table.rain <- table(weather.df$RainToday, weather.df$RainTomorrow)
table.rain
chisq.test(table(weather.df$RainToday, weather.df$RainTomorrow))

table.gustdir <- table(weather.df$WindGustDir, weather.df$RainTomorrow)
table.gustdir
chisq.test(table.gustdir)

table.wind9 <- table(weather.df$WindDir9am, weather.df$RainTomorrow)
table.wind9
chisq.test(table.wind9)

table.wind3 <- table(weather.df$WindDir3pm, weather.df$RainTomorrow)
table.wind3
chisq.test(table.wind3)

str(weather.df)

# calculate summary statistics for numeric and factor variables
summary(Filter(is.numeric, weather.df))
summary(Filter(is.factor, weather.df))

# create dummy variables for all factors
dumgustdir <- as.data.frame(model.matrix(~0 + WindGustDir, data = weather.df))
t(t(names(dumgustdir)))

dumwind9 <- as.data.frame(model.matrix(~0 + WindDir9am, data = weather.df))
t(t(names(dumwind9)))

dumwind3 <- as.data.frame(model.matrix(~0 + WindDir3pm, data = weather.df))
t(t(names(dumwind3)))

dumtoday <- as.data.frame(model.matrix(~0 + RainToday, data = weather.df))
t(t(names(dumtoday)))

dummies <- cbind(dumgustdir, dumwind9, dumwind3, dumtoday)
t(t(names(dummies)))
t(t(names(weather.df)))

weather.df <- cbind(weather.df[, -c(6,8:9,20:21)], dummies)
t(t(names(weather.df)))

# create new record
weather.new <- weather.df[1, -17] # just copy the first row of weather.df and drop the outcome variable
cols.new <- colnames(weather.new)
# set all variables to 0
for (i in cols.new) {
  weather.new[[i]] <- 0
}
# only update those variables whose values are not 0
weather.new$MinTemp <- 24.2
weather.new$MaxTemp <- 31.7
weather.new$Evaporation <- 8
weather.new$Sunshine <- 6.1
weather.new$WindGustDirSSE <- 1
weather.new$WindGustSpeed <- 33
weather.new$WindDir9amNE <- 1
weather.new$WindDir3pmS <- 1
weather.new$WindSpeed9am <- 7
weather.new$WindSpeed3pm <- 17
weather.new$Humidity9am <- 68
weather.new$Humidity3pm <- 78
weather.new$Pressure9am <- 1005.6
weather.new$Pressure3pm <- 1002.7
weather.new$Cloud9am <- 6
weather.new$Cloud3pm <- 7
weather.new$Temp9am <- 29.4
weather.new$Temp3pm <- 27
weather.new$RainTodayNo <- 1
weather.new

# standardize numerical predictors to 0-1 scale
cols <- colnames(weather.df[,-17])
for (i in cols) {
  weather.new[[i]] <- (weather.new[[i]] - min(weather.df[[i]])) / (max(weather.df[[i]]) - min(weather.df[[i]]))
  weather.df[[i]] <- (weather.df[[i]] - min(weather.df[[i]])) / (max(weather.df[[i]]) - min(weather.df[[i]]))
}
summary(weather.df)
weather.new

# partition the data
set.seed(7)
train.rows <- sample(nrow(weather.df), nrow(weather.df)*0.6)
train.data <- weather.df[train.rows, ]
valid.data <- weather.df[-train.rows, ]
valid.rows <- as.numeric(row.names(valid.data))

# perform discriminant analysis
library(DiscriMiner)
rain.da <- linDA(weather.df[, c(1:16, 18:32, 34:49, 51:66, 69)], weather.df$RainTomDum, 
                 validation = "learntest", 
                 learn = train.rows, 
                 test = valid.rows)
rain.da$functions

# confusion matrix
library(caret)
confusionMatrix(rain.da$classification, 
                as.factor(valid.data$RainTomDum), 
                positive = "1")


# lift chart
library(gains)
gain <- gains(valid.data$RainTomDum, exp(rain.da$scores[, 2]) / (exp(rain.da$scores[, 1]) + exp(rain.da$scores[, 2])), 
              groups = length(valid.data))

# plot lift chart
par(mfcol = c(1,2))
plot(c(0, gain$cume.pct.of.total * sum(as.numeric(valid.data$RainTomDum))) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(as.numeric(valid.data$RainTomDum))) ~ c(0, nrow(valid.data)), lty = 2)
abline(v = 200, col = "red")
abline(h = 155, lty = 3)
abline(h = 40, lty = 3)


# compute deciles and plot decile-wise lift chart
gain <- gains(as.numeric(valid.data$RainTomDum), 
              exp(rain.da$scores[, 2]) / (exp(rain.da$scores[, 1]) + exp(rain.da$scores[, 2])))
heights <- gain$mean.resp / mean(as.numeric(valid.data$RainTomDum))
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 6),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

# classify new record
classifyrain <- classify(rain.da, weather.new[,c(1:31, 33:48, 50:65, 68)])
classifyrain

# perform k-nn
library(FNN)
knn.pred.1 <- knn(train.data[, c(1:16,18:69)], valid.data[, c(1:16,18:69)], 
                     cl = train.data[, 17], k = 1, prob = TRUE)
library(caret)
confusionMatrix(knn.pred.1, as.factor(valid.data[, 17]), positive = "1")

#classifying the new record
knn.pred.new1 <- knn(train.data[, c(1:16,18:69)], weather.new, 
                  cl = train.data[, 17], k = 1, prob = TRUE)
knn.pred.new1

## measuring accuracy of different k-values
# initialize a data frame with two columns: k and accuracy
accuracy.df <- data.frame(k = seq(1, 100, 1), accuracy = rep(0, 100))

# compute knn for different k on validation set
for (i in 1:100) {
  knn.pred <- knn(train.data[, c(1:16,18:69)], valid.data[, c(1:16,18:69)], cl = train.data[, 17], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, as.factor(valid.data[, 17]))$overall[1]
}
accuracy.df

accuracy.df[accuracy.df$accuracy == max(accuracy.df$accuracy),]

# confusion matrix using best k on validation data
knn.pred.best <- knn(train.data[, c(1:16,18:69)], valid.data[, c(1:16,18:69)], 
                     cl = train.data[, 17], k = 75, prob = TRUE)
confusionMatrix(knn.pred.best, as.factor(valid.data[, 17]), positive = "1")

#classifying the new record using the optimal k
knn.pred.newbest <- knn(train.data[, c(1:16,18:69)], weather.new, 
                     cl = train.data[, 17], k = 75, prob = TRUE)
knn.pred.newbest
