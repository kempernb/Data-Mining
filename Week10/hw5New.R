library(rpart)
library(rpart.plot)
library(tidyverse)
library(caret)

# Read and clean
spam.base <- read.csv("spambase.csv")
dim(spam.base)
# t(t(names(spam.base)))
spam.base <- rename(spam.base, semicolon=C.)
spam.base <- rename(spam.base, parentheses=C..1)
spam.base <- rename(spam.base, bracket=C..2)
spam.base <- rename(spam.base, exclamation=C..3)
spam.base <- rename(spam.base, dollar.sign=C..4)
spam.base <- rename(spam.base, pound.sign=C..5)

# Partition
set.seed(1)
train.index <- sample(nrow(spam.base), nrow(spam.base)*0.6)
spam.train <- spam.base[train.index,]
spam.valid <- spam.base[-train.index,]

# Tree using cp = 0 and  minsplit = 1
spam.rt <- rpart(Spam ~ ., data = spam.train, method = "class", cp = 0, minsplit = 1)

spam.rt

#Plot classification tree
# prp(spam.rt, digits = 4, type = 1, extra = 1, varlen = -10,box.col = ifelse(spam.rt$frame$var == "<leaf>", 'gray', 'white'))


default.ct.pred.valid <- predict(spam.rt, spam.valid, type = "class")
confusionMatrix(default.ct.pred.valid, 
                as.factor(spam.valid$Spam), 
                positive = "1")

# Pruning the tree
# plotcp(spam.rt)
# printcp(spam.rt)

# left most of relative error
newcp <- 0.00721371

# optimized tree
spam.pruned <- prune(spam.rt, cp = newcp)
spam.pruned

new.ct.pred.valid <- predict(spam.pruned, spam.valid, type = "class")
confusionMatrix(new.ct.pred.valid, 
                as.factor(spam.valid$Spam), 
                positive = "1")

# prp(spam.pruned, digits = 4, type = 1, extra = 1, varlen = -10,
#     box.col = ifelse(spam.pruned$frame$var == "<leaf>", 'gray', 'white'))

# classify a new record
#Creating    
spam.new <- spam.base[1,]
cols.new <- colnames(spam.new)

# set all variables to 0
for (i in cols.new) {
  spam.new[[i]] <- 0
}

spam.new$our <- 0.94
spam.new$you <- 1.88
spam.new$your <- 2.83
spam.new$exclamation <- 0.428
spam.new$CAP_avg <- 4.652
spam.new$CAP_long <- 31
spam.new$CAP_tot <- 107
spam.new

pred.spam.new <- predict(spam.pruned, spam.new, type = "class")
pred.spam.new


spam.pruned$functions

# random forests
library(randomForest)
rf <- randomForest(as.factor(spam.base$Spam)~., 
                   data=spam.base, ntree=500, mtry=4, 
                   nodesize=5, importance=TRUE)

# variable importance plot
# varImpPlot(rf, type=1)

# rownames(spam.valid) <- NULL

# confusion matrix
rf.pred <- predict(rf,spam.valid)
spam.valid$Spam <- as.factor(spam.valid$Spam)

confusionMatrix(rf.pred,spam.valid$Spam, postive="1")
