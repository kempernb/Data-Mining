library(tidyverse)
library(corrplot)
library(caret)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(corrplot)

set.seed(1)

# read data ----
imdb <- read.csv ("IMDB Movie Dataset.csv")
imdb <- imdb [-c(1:2,7,11:12,tt15,17:18,20:21)]
# parse out the pipe-delimited data ----
parse_genre <- data.frame(table(unlist(strsplit(as.character(imdb$genres), split = "|", fixed = TRUE))))
head(parse_genre[order(parse_genre$Freq, decreasing = TRUE),],20)
imdb$Drama <- ifelse(grepl("Drama", imdb$genres),1,0)
imdb$Comedy <- ifelse(grepl("Comedy", imdb$genres),1,0)
imdb$Thriller <- ifelse(grepl("Thriller", imdb$genres),1,0)
imdb$Action <- ifelse(grepl("Action", imdb$genres),1,0)
imdb$Romance <- ifelse(grepl("Romance", imdb$genres),1,0)
imdb$Adventure <- ifelse(grepl("Adventure", imdb$genres),1,0)
imdb$Crime <- ifelse(grepl("Crime", imdb$genres),1,0)
imdb$Scifi <- ifelse(grepl("Sci-Fi", imdb$genres),1,0)
imdb$Fantasy <- ifelse(grepl("Fantasy", imdb$genres),1,0)
imdb$Horror <- ifelse(grepl("Horror", imdb$genres),1,0)
imdb$Family <- ifelse(grepl("Family", imdb$genres),1,0)
imdb$Mystery <- ifelse(grepl("Mystery", imdb$genres),1,0)
imdb$Biography <- ifelse(grepl("Biography", imdb$genres),1,0)
imdb$Animation <- ifelse(grepl("Animation", imdb$genres),1,0)
imdb$Music <- ifelse(grepl("Music", imdb$genres),1,0)
imdb$War <- ifelse(grepl("War", imdb$genres),1,0)
imdb$History <- ifelse(grepl("History", imdb$genres),1,0)
imdb$Sport <- ifelse(grepl("Sport", imdb$genres),1,0)
imdb$Musical <- ifelse(grepl("Musical", imdb$genres),1,0)
imdb$Documentary <- ifelse(grepl("Documentary", imdb$genres),1,0)
imdb <- imdb [-c(7)]
# handle missing data ----
summary(imdb)
imdb <- imdb[!(is.na(imdb$num_critic_for_reviews) |
                 is.na(imdb$duration) |
                 is.na(imdb$actor_3_facebook_likes) |
                 is.na(imdb$actor_1_facebook_likes) |
                 is.na(imdb$facenumber_in_poster) |
                 is.na(imdb$num_user_for_reviews) |
                 is.na(imdb$actor_2_facebook_likes)), ]
imdb$director_facebook_likes[is.na(imdb$director_facebook_likes)] <- median(imdb$director_facebook_likes, na.rm = TRUE)
imdb$gross[is.na(imdb$gross)] <- median(imdb$gross, na.rm = TRUE)
imdb$budget[is.na(imdb$budget)] <- median(imdb$budget, na.rm = TRUE)
imdb$title_year[is.na(imdb$title_year)] <- median(imdb$title_year, na.rm = TRUE)
imdb$aspect_ratio[is.na(imdb$aspect_ratio)] <- median(imdb$aspect_ratio, na.rm = TRUE)
# Remove highly correlated ----
library(tidyverse)
library(corrplot)
library(caret)

imdb.numeric <- imdb %>% 
  select_if(is.numeric) 

imdb.cat <- imdb %>% 
  select_if(is.character)

m <- imdb.numeric %>% 
  cor()

# corrplot(m)

max.cor <- .95

highlyCorDescr <- findCorrelation(m, cutoff = max.cor)
imdb.numeric <- imdb.numeric[,-highlyCorDescr]
# descrCor2 <- cor(imdb.numeric)

imdb <- cbind(imdb.numeric,imdb.cat)

# Scale data ---- 

imdb$title_year <- as.factor(imdb$title_year )
titleYears <- model.matrix(~ 0 + title_year,data = imdb) %>% 
  data.frame() %>% 
  select(-1)

imdb <- imdb %>% 
  select(-title_year) %>% 
  cbind(titleYears)

# Partitioning Data -----

train.rows <- sample(rownames(imdb), nrow(imdb)*0.6)
train.data <- imdb[train.rows,]
valid.rows <- setdiff(rownames(imdb),train.rows)
valid.data <- imdb[valid.rows,]

valid.data <- valid.data[valid.data$content_rating != "TV-Y", ]


# basic model -----

imdb.lm = lm(imdb_score ~ ., data=train.data)
summary(imdb.lm)

sum.imdb.lm <- summary (imdb.lm)
(sum.imdb.lm$sigma)^2 #MSE
sum.imdb.lm$adj.r.squared
AIC(imdb.lm)
BIC(imdb.lm)

library(leaps)
# imdb.search <- regsubsets(imdb_score ~ ., data = train.data, nbest = 1, nvmax = ncol(train.data),really.big=T)
#variable Selection

library(forecast)
pred.imdb.lm <- predict(imdb.lm, newdata = valid.data)
accuracy(pred.imdb.lm, valid.data$imdb_score)

#variable Selection
imdb.null <- lm(imdb_score ~ 1, data = train.data)
imdb.full <- lm(imdb_score ~ ., data = train.data)
#forward Selection
imdb.fwd <- step(imdb.null, scope = list(lower = imdb.null, upper = imdb.full), 
                 direction = "forward")
summary(imdb.fwd)
AIC(imdb.fwd)

#stepwise Selection
imdb.step <- step(imdb.null, scope = list(lower = imdb.null, upper = imdb.full),
                  direction = "both")
summary(imdb.step)
AIC(imdb.step)


# Accuracy
library(forecast)

valid.data <- valid.data[valid.data$content_rating != "TV-Y", ]
# forward selection
pred.imdb.fwd <- predict(imdb.fwd, newdata = valid.data)
accuracy(pred.imdb.fwd, valid.data$imdb_score)


#stepwise selection
pred.imdb.step <- predict(imdb.step, newdata = valid.data)
accuracy(pred.imdb.step, valid.data$imdb_score)


