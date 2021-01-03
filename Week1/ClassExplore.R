library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(corrplot)

df <- read_csv("BostonHousing.csv")

#Get summary of data
summary(df)

HistNumeric <- function(x){
  d <- melt(x)
  ggplot(d,aes(x = value)) +
    facet_wrap(~variable,scales = "free_x") +
    geom_histogram()
}

HistNumeric(df)

#### Line Plot
ggplot(df,aes(x=LSTAT, y =  MEDV)) + geom_point()

#Boxplot group
ggplot(df, aes(x=LSTAT, y=MEDV, group=ZN)) +
  geom_boxplot()

#Corr plot / table
M <- cor(df)
# corrplot(M)

# corrplot(M, method = "number")

# Create dummy variables

