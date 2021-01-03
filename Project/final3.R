library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)
library(randomForest)
library(corrplot)
library(ggplot2)

set.seed(1)

horse <- read.csv("runs.csv")
# remove unnecessary variables
horse <- horse[-c(1:3,8,11,36:37)]
str(horse)
summary(horse)

# convert variables into factors
horse$result <- as.factor(horse$result)
horse$position_sec1 <- as.factor(horse$position_sec1)
horse$position_sec2 <- as.factor(horse$position_sec2)
horse$position_sec3 <- as.factor(horse$position_sec3)
horse$position_sec4 <- as.factor(horse$position_sec4)
horse$position_sec5 <- as.factor(horse$position_sec5)
horse$position_sec6 <- as.factor(horse$position_sec6)
str(horse)

# create dummy variables for horse_type
dummies <- as.data.frame(model.matrix(~0 + horse_type, data = horse))
horse <- cbind(horse[,-c(5)], dummies[,-c(1)])

# handle missing data
horse$behind_sec4[is.na(horse$behind_sec4)] <- median(horse$behind_sec4, na.rm = TRUE)
horse$behind_sec5[is.na(horse$behind_sec5)] <- median(horse$behind_sec5, na.rm = TRUE)
horse$behind_sec6[is.na(horse$behind_sec6)] <- median(horse$behind_sec6, na.rm = TRUE)
horse$time4[is.na(horse$time4)] <- median(horse$time4, na.rm = TRUE)
horse$time5[is.na(horse$time5)] <- median(horse$time5, na.rm = TRUE)
horse$time6[is.na(horse$time6)] <- median(horse$time6, na.rm = TRUE)
horse <- horse[!is.na(horse$place_odds),]

# correlation matrix with heatmap 
# select numeric variables
horse2 <- horse[-c(30:38)]
nums <- unlist(lapply(horse2, is.numeric))  
horse.filt <- horse2  
# number of rows with NA value
horse.num <- horse.filt[,nums]
# correlation plot
M <- cor(horse.num)
corrplot(M)

# histogram
hist(horse$horse_rating, main = "Distribution of Horse Ratings", xlab = "Horse Rating")
mean(horse$horse_rating)
median(horse$horse_rating)
mode(horse$horse_rating)
quantile(horse$horse_rating, c(.25,.5,.75), type = 1)

hist(horse$actual_weight, main = "Distribution of Horse Actual Weight", xlab = "Horse Weight (Pounds)")
mean(horse$actual_weight)
median(horse$actual_weight)
mode(horse$actual_weight)
quantile(horse$actual_weight, c(.25,.5,.75), type = 1)

# boxplots of 4 predictors 
horse$won <- as.factor(horse$won)
boxplot1 <- ggplot(horse, aes(x = won, y= win_odds)) + geom_boxplot()
boxplot1
boxplot2 <- ggplot(horse, aes(x = won, y = declared_weight)) + geom_boxplot() 
boxplot2
boxplot3 <- ggplot(horse, aes(x = won, y = finish_time)) + geom_boxplot() 
boxplot3
boxplot4 <- ggplot(horse, aes(x = won, y = behind_sec1)) + geom_boxplot()+ylim(0,30)
boxplot4

# CLASSIFICATION
# horse.glm used for predicting a WINNER or not based on Won column
# Remove converting to factor
horse$won <- as.numeric(as.character(horse$won))

horse.glm <- horse %>%
  # get rid of na columns
  select(-c(position_sec4,position_sec5,position_sec6)) %>%
  # get rid of result column, found after and lengths behind
  select(-c(result,lengths_behind))

# oversample the winners
# separate the won and lost

horse.glm.won <- horse.glm[horse.glm$won == 1, ]
horse.glm.lost <- horse.glm[horse.glm$won == 0, ]
# use half of the won or the training data and the rest for validation
train.won.index <- sample(nrow(horse.glm.won), nrow(horse.glm.won) * 0.5)
train.won.data <- horse.glm.won[train.won.index, ]
valid.won.data <- horse.glm.won[-train.won.index, ]
# use the same number of won for training
train.lost.index <- sample(nrow(horse.glm.lost), nrow(horse.glm.won)*0.5)
train.lost.data <- horse.glm.lost[train.lost.index, ]
# add enough lost to the validation data to maintain the original ratio of
# won to lost
valid.lost.index <- setdiff(row.names(horse.glm.lost), train.lost.index)
valid.lost.index <- sample(valid.lost.index, 
                            nrow(train.lost.data) * (1 - mean(horse.glm$won)) / mean(horse.glm$won))
valid.lost.data <- horse.glm.lost[valid.lost.index, ]
# combine the won and nonwon losts into the training and validation sets
horse.train <- rbind(train.won.data, train.lost.data)
horse.valid <- rbind(valid.won.data, valid.lost.data)


# partition and split, NOT oversample
# train.rows <- sample(nrow(horse.glm), nrow(horse.glm) * 0.6)
# horse.train <- horse.glm[train.rows, ]
# horse.valid <- horse.glm[-train.rows, ]

# fit the linear regression
model2 <- glm(won ~ . , data = horse.train)
summary(model2)
model2$deviance
AIC(model2)
BIC(model2)
# confusion matrix
logit.reg.pred <- predict(model2, horse.valid, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred >= 0.75, "1", "0")), as.factor(horse.valid$won),
                positive = "1")

# CART 
# classification tree using cp = 0 and  minsplit = 1
horse.ct <- rpart(won ~ ., data = horse.train, method = "class", cp = 0, minsplit = 1)
horse.ct
# count number of leaves
length(horse.ct$frame$var[horse.ct$frame$var == "<leaf>"])
# plot tree
prp(horse.ct, digits = 4, type = 1, extra = 1, varlen = -10, box.col = ifelse(horse.ct$frame$var == "<leaf>", 'gray', 'white'))

# confusion matrix
default.ct.pred.valid <- predict(horse.ct, horse.valid, type = "class")
confusionMatrix(default.ct.pred.valid, 
                as.factor(horse.valid$won), 
                positive = "1")

# prune the tree
plotcp(horse.ct)
printcp(horse.ct)

# left most of relative error
newcp <- 0.0015193

# classification tree using cp = 0.0015193  and  minsplit = 2
horse.ct.pruned <- rpart(won ~ ., data = horse.train, method = "class", cp =0.0015193 , minsplit = 2)
horse.ct.pruned
# count number of leaves
length(horse.ct.pruned$frame$var[horse.ct.pruned$frame$var == "<leaf>"])
# plot tree
prp(horse.ct.pruned, digits = 4, type = 1, extra = 1, varlen = -10, box.col = ifelse(horse.ct.pruned$frame$var == "<leaf>", 'gray', 'white'))

# confusion matrix
pruned.ct.pred.valid <- predict(horse.ct.pruned, horse.valid, type = "class")
confusionMatrix(pruned.ct.pred.valid, 
                as.factor(horse.valid$won), 
                positive = "1")

# random forest
rf <- randomForest(as.factor(won) ~ ., data = horse.train, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)

# variable importance plot
varImpPlot(rf, type = 1)

# confusion matrix
rf.pred <- predict(rf, horse.valid)
confusionMatrix(rf.pred, as.factor(horse.valid$won), positive = "1")

# REGRESSION
set.seed(1)
# Pack to factor
horse$result <- as.factor(horse$result)
# get rid of na columns
horse.reg <- horse %>%
  select(-c(position_sec4, position_sec5, position_sec6)) %>%
  # get rid of result and won column, rating is determined pre-race
  select(-c(result, lengths_behind))

# most of horse ratings are 60, need to remove some records to make better predications
hist(horse.reg$horse_rating)

# separate the won and lost
horse.reg.60 <- horse.reg[horse.reg$horse_rating == 60,]
horse.glm.not <- horse.reg[horse.reg$horse_rating != 60,]

# use half of the won or the training data and the rest for validation
train.60.index <-
  sample(nrow(horse.glm.won), nrow(horse.glm.won) * 0.4)
horse.reg.60 <- horse.reg.60[train.60.index, ]
horse.reg <- rbind(horse.reg.60, horse.glm.not)

hist(horse.reg$horse_rating)

# partition
train.rows <- sample(nrow(horse.reg), nrow(horse.reg) * 0.6)
horse.train <- horse.reg[train.rows, ]
horse.valid <- horse.reg[-train.rows, ]

# full linear model
# basic model 
horse.lm <- lm(horse_rating ~ ., data=horse.train)
summary(horse.lm)
pred.horse.lm <- predict(horse.lm, newdata = horse.valid)
accuracy(pred.horse.lm, horse.valid$horse_rating)

#variable Selection
horse.null <- lm(horse_rating ~ 1, data = horse.train)
horse.full <- lm(horse_rating ~ ., data = horse.train)

#stepwise Selection
horse.step <- step(horse.null, scope = list(lower = horse.null, upper = horse.full),
                  direction = "both")
summary(horse.step)


# regression Tree
horse.rt <- rpart(horse_rating ~ ., data = horse.train)
horse.rt
prp(horse.rt, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(horse.rt$frame$var == "<leaf>", 'gray', 'white'))

#Prediction
#In- sample

horse.train.rt.pred <- predict(horse.rt)
RMSE(horse.train.rt.pred, horse.train$won)

#Out of sample

horse.valid.rt.pred <- predict(horse.rt, newdata = horse.valid)
RMSE(horse.valid.rt.pred, horse.valid$won)

prp(horse.rt, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(horse.rt$frame$var == "<leaf>", 'gray', 'white'))

#Prediction
#In- sample

horse.train.rt.pred <- predict(horse.rt)
RMSE(horse.train.rt.pred, horse.train$won)

#Out of sample

horse.valid.rt.pred <- predict(horse.rt, newdata = horse.valid)
RMSE(horse.valid.rt.pred, horse.valid$won)

