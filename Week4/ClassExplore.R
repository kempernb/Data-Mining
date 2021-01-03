# Confusion Matrix

library(caret)
library(e1071)
library(tidyverse)

owner.df <- read.csv("ownerExample.csv",stringsAsFactors = T)
# 
# owner.df$Class <- factor(owner.df$Class)
# owner.df$Probability <- factor(owner.df$Probability)

owner.df$confusion <- factor(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner'))

## cutoff = 0.5
confusionMatrix(owner.df$confusion, owner.df$Class,
                positive = "owner")

owner.df$dumClass <- as.factor(ifelse(owner.df$Class == "owner", 1, 0))
library(pROC)
r <- roc(owner.df$dumClass, owner.df$Probability)
plot.roc(r)
auc(r)

library(gains)
df <- read.csv("liftexample.csv")
gain <- gains(df$actual, df$prob, groups = dim(df)[1])
plot(c(0, gain$cume.pct.of.total * sum(df$actual)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", type = "l")
lines(c(0, sum(df$actual)) ~ c(0, dim(df)[1]), col = "gray", lty = 2)


### Logistic regression

bank.df <- read.csv("UniversalBank.csv")

dim(bank.df)
str(bank.df)

bank.df <- bank.df %>%
  select(-c(ID,ZIP.Code))

bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3),
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(2)
train.index <- sample(nrow(bank.df), nrow(bank.df) * 0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# single predictor model
# use glm() (general linear model) with family = "binomial" to fit a logistic
# regression
logit.single <- glm(Personal.Loan ~ Income, data = train.df, family = "binomial")
options(scipen = 999)
summary(logit.single)

# full logistic regression model
# use glm() (general linear model) with family = "binomial" to fit a logistic
# regression
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial")
options(scipen = 999)
summary(logit.reg)


