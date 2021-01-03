library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")

plot(Lot_Size ~ Income, data = mower.df, col = ifelse(mower.df$Ownership == "Owner", "black", "red"), 
     pch = 16, xlab = "Income ($000s)", ylab = "Lot Size (000s of square feet)")
legend("topright", c("owner", "non-owner"), pch = 16, col = c("black", "red"))
abline(v = 60)
segments(0, 21, 60, 21)
segments(60, 20, 200, 20)
segments(85, 0, 85, 20)
segments(62, 0, 62, 20)

# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~ ., data = mower.df,
                    control = rpart.control(maxdepth = 2), method = "class")

## plot tree
# use prp() to plot the tree. You can control plotting parameters such as color, shape,
# and information displayed (which and where).
prp(class.tree, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(class.tree$frame$var == "<leaf>", 'gray', 'white'))

# for the full tree...
class.tree2 <- rpart(Ownership ~ ., data = mower.df, method = "class", cp = 0, minsplit = 1)

## plot tree
prp(class.tree2, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(class.tree2$frame$var == "<leaf>", 'gray', 'white'))

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[, -c(1, 5)] # drop ID and ZIP.Code
bank.df$Education <- as.factor(bank.df$Education)

# partition
set.seed(1)
train.index <- sample(nrow(bank.df), nrow(bank.df)*0.6)
bank.train <- bank.df[train.index, ]
bank.valid <- bank.df[-train.index, ]

## creating a default classification tree
# classification tree
default.ct <- rpart(Personal.Loan ~ ., data = bank.train, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, varlen = -10,
    box.col = ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white'))

## creating a deeper classification tree
deeper.ct <- rpart(Personal.Loan ~ ., data = bank.train, method = "class", cp = 0, minsplit = 1)
# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
# prp(deeper.ct, type = 1, extra = 1, under = TRUE, varlen = -10,
#     box.col = ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))

# classify records in the training data
# set argument type = "class" in predict() to generate predicted class membership
default.ct.pred.train <- predict(default.ct, bank.train, type = "class")
# generate confusion matrix for training data
library(caret)
confusionMatrix(default.ct.pred.train, 
                as.factor(bank.train$Personal.Loan), 
                positive = "1")

# classify records in the validation data
default.ct.pred.valid <- predict(default.ct, bank.valid, type = "class")
confusionMatrix(default.ct.pred.valid, 
                as.factor(bank.valid$Personal.Loan), 
                positive = "1")

# classify records in the training data using the deeper tree
deeper.ct.pred.train <- predict(deeper.ct, bank.train, type = "class")
confusionMatrix(deeper.ct.pred.train, 
                as.factor(bank.train$Personal.Loan), 
                positive = "1")

# classify records in the validation data using the deeper tree
deeper.ct.pred.valid <- predict(deeper.ct, bank.valid, type = "class")
confusionMatrix(deeper.ct.pred.valid, 
                as.factor(bank.valid$Personal.Loan), 
                positive = "1")

## tabulating tree error as a function of complexity parameter
# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter
cv.ct <- rpart(Personal.Loan ~ ., data = bank.df, method = "class",
               cp = 0.00001, minsplit = 5, xval = 10)
# use printcp() to print the table
printcp(cv.ct)

# prune by lower cp
pruned.ct <- prune(cv.ct, cp = 0.0062500)
prp(pruned.ct, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))

## random forest
# install.packages("randomForest")
library(randomForest)
rf <- randomForest(as.factor(Personal.Loan) ~ ., data = bank.train, ntree = 500,
                             mtry = 4, nodesize = 5, importance = TRUE)
# variable importance plot
varImpPlot(rf, type = 1)
#confusion matrix
rf.pred <- predict(rf, bank.valid)
confusionMatrix(rf.pred, as.factor(bank.valid$Personal.Loan), positive = "1")

## boosted tree
# install.packages("adabag")
library(adabag)
bank.train$Personal.Loan <- as.factor(bank.train$Personal.Loan)
bank.valid$Personal.Loan <- as.factor(bank.valid$Personal.Loan)
boost <- boosting(Personal.Loan ~ ., data = bank.train)
pred <- predict(boost, bank.valid)
confusionMatrix(as.factor(pred$class), bank.valid$Personal.Loan, positive = "1")

