library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(corrplot)

library(caret)

df.imdb <- read_csv("IMDB Movie Dataset.csv")

#Get summary of data
summary(df.imdb)

HistNumeric <- function(x){
  d <- melt(x)
  ggplot(d,aes(x = value)) +
    facet_wrap(~variable,scales = "free_x") +
    geom_histogram()
}


#### Line Plot
# ggplot(df.imdb,aes(x=cast_total_facebook_likes, y = budget)) + geom_point()

### Select only Numerifc & drop na
nums <- unlist(lapply(df.imdb, is.numeric))  
df.imdb.filt <- df.imdb %>% 
  drop_na()

#Number of rows with NA value
n_rows_missing <- nrow(df.imdb) - nrow(df.imdb.filt)

df.imdb.num <- df.imdb.filt[,nums]

#Correleation
M <- cor(df.imdb.num)
# corrplot.mixed(M)
# corrplot(M)
# corrplot(M, method = "number") 

## Bar Plot
# ggplot(data=df.imdb.filt, aes(x=color, y=imdb_score)) +
#   geom_bar(stat="identity", color="blue", fill="white")

## Box plot
# ggplot(data=df.imdb.filt, aes(x=color, y=imdb_score)) +
#   geom_boxplot()

# Scatter Plot matrix
# pairs(df.imdb.num)

### Create Dummy variables on the COLOR category, drop the first column 
dummy.color <- model.matrix(~ 0 + color + country, data = df.imdb.filt) %>% 
  data.frame() %>% 
  select(-1)

dummy.imdb <- cbind(df.imdb.filt,dummy.color)




