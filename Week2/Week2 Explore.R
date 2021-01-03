# Load libraries ----
library(tidyverse)
library(corrplot)
library(ggplot2)

df.imdb <- read.csv("IMDB Movie Dataset.csv")

HistNumeric <- function(x){
  d <- melt(x)
  ggplot(d,aes(x = value)) +
    facet_wrap(~variable,scales = "free_x") +
    geom_histogram()
}

#Selecting if columns are numeric, drop na
df.plot <- df.imdb %>%
  select_if(is.numeric) %>% 
  # summary() %>% 
  drop_na()
  
# ggplot(df.plot,aes(x = duration,y = imdb_score)) + geom_point()

# model.matrix(~0 + color, data = df.imdb)

#Multi-linear regression

