spam.df <- read.csv("spambase.csv", header = TRUE, stringsAsFactors = TRUE)

par(mfcol = c(1,1))

# convert Spam to a factor
spam.df$Spam <- as.factor(spam.df$Spam)
summary(spam.df)

# rename variables
library(dplyr)
spam.df <- rename(spam.df, "re" = re., "C_semicolon" = C., "C_parenthesis" = C..1, "C_bracket" = C..2, "C_exclamation" = C..3, 
                  "C_dollar" = C..4, "C_pound" = C..5)
t(t(names(spam.df)))

# partition the data
set.seed(7)
train.rows <- sample(nrow(spam.df), nrow(spam.df)*0.6)
train.data <- spam.df[train.rows, ]
valid.data <- spam.df[-train.rows, ]

# create the full classification tree
library(rpart)
library(rpart.plot)
spam.ct <- rpart(Spam ~ ., data = train.data, method = "class", 
                 cp = 0, minsplit = 1)
# prp(spam.ct, type = 1, extra = 1, varlen = -10,
#     box.col = ifelse(spam.ct$frame$var == "<leaf>", 'gray', 'white'))
spam.ct
length(spam.ct$frame$var[spam.ct$frame$var == "<leaf>"])

spam.ct.pred.train <- predict(spam.ct, train.data, type = "class")
# generate confusion matrix for training data
library(caret)
confusionMatrix(spam.ct.pred.train, 
                train.data$Spam, 
                positive = "1")

# classify records in the validation data
spam.ct.pred.valid <- predict(spam.ct, valid.data, type = "class")
confusionMatrix(spam.ct.pred.valid, 
                as.factor(valid.data$Spam), 
                positive = "1")

# perform cross-validation
cv.ct <- rpart(Spam ~ ., data = train.data, method = "class",
               cp = 0, minsplit = 1, xval = 10)
# use printcp() to print the table
options(scipen = 999)
printcp(cv.ct)
0.22098+0.013693
0.234673

# create the best pruned tree
pruned.ct <- prune(cv.ct, cp =  0.00464253)
prp(pruned.ct, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))
pruned.ct
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

prp(pruned.ct, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(spam.ct$frame$var == "<leaf>", 'gray', 'white'))

# classify records in the validation data based on best pruned tree
best.pred.valid <- predict(pruned.ct, valid.data, type = "class")
confusionMatrix(best.pred.valid, 
                as.factor(valid.data$Spam), 
                positive = "1")

# create a random forest to predict spam
library(randomForest)
spam.rf <- randomForest(Spam ~ ., data = train.data, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)
# variable importance plots
varImpPlot(spam.rf, type = 1)

#confusion matrix
rf.pred <- predict(spam.rf, valid.data)
confusionMatrix(rf.pred, valid.data$Spam, positive = "1")

######## pre-processing for neural nets ###############
str(spam.df)
t(t(names(spam.df)))
 

# neural net with one hidden layer containing 3 nodes
library(neuralnet)
spam.nn3 <- neuralnet(Spam ~ ., data = train.data, 
                linear.output = FALSE, hidden = 3)

# plot network
plot(spam.nn3, rep = "best")

## confusion matrix
library(caret)
predict.valid <- neuralnet::compute(spam.nn3, valid.data[,-58])
predicted.class.valid <- apply(predict.valid$net.result, 1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class.valid == 1, "1", "0")),
                valid.data$Spam, positive = "1")

# neural net with one hidden layer containing 28 nodes
spam.nn28 <- neuralnet(Spam ~ ., data = train.data, 
                     linear.output = FALSE, hidden = 28)

# plot network
plot(spam.nn28, rep = "best")

## confusion matrix
predict.valid <- neuralnet::compute(spam.nn28, valid.data[,-58])
predicted.class.valid <- apply(predict.valid$net.result, 1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class.valid == 1, "1", "0")),
                valid.data$Spam, positive = "1")

# neural net with two hidden layers containing 12 nodes each
spam.nn12.12 <- neuralnet(Spam ~ ., data = train.data, 
                      linear.output = FALSE, hidden = c(12,12))

# plot network
plot(spam.nn12.12, rep = "best")

## confusion matrix
predict.valid <- neuralnet::compute(spam.nn12.12, valid.data[,-58])
predicted.class.valid <- apply(predict.valid$net.result, 1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class.valid == 1, "1", "0")),
                valid.data$Spam, positive = "1")
