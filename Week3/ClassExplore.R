library(tidyverse)

toyota.df <- read.csv("ToyotaCorolla.csv")

# drop the record(s) containing the outlier for CC
toyota.df <- toyota.df[toyota.df$CC != 16000, ]

# create new data frame containing only the variables to be used for analysis
toyotasub.df <- toyota.df[, c(3, 4, 7:10, 12:14, 17:18)]
head(toyotasub.df)

# partitioning the data
# use set.seed() to get the same partitions when re-running the R code
set.seed(1)


## partitioning into training (60%) and validation (40%) 
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
sample.set <- .6

train.rows <- sample(rownames(toyotasub.df), nrow(toyotasub.df)*sample.set)
# collect all the columns with training row ID into training set:
train.data <- toyotasub.df[train.rows, ]
# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(toyotasub.df), train.rows)
valid.data <- toyotasub.df[valid.rows, ]

# use lm() to run a linear regression of Price on all 10 predictors in the
# training set
# use . after ~ to include all the remaining columns in train.df as predictors.
toyota.lm <- lm(Price ~ ., data = train.data)
# use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
summary(toyota.lm)

library(forecast)
# use predict() to make predictions on a new set
valid.lm.pred <- predict(toyota.lm, valid.data)
options(scipen = 999, digits = 1)
# calculate the residuals
valid.resid <- valid.data$Price - valid.lm.pred
# look at the first 20 residuals
data.frame("Predicted" = valid.lm.pred[1:20], "Actual" = valid.data$Price[1:20],
           "Residual" = valid.resid[1:20])

# use accuracy() to compute common accuracy measures
accuracy(valid.lm.pred, valid.data$Price)

# exhaustive variqvle search
library(leaps)



