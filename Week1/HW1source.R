# Load libraries ----
library(tidyverse)
library(corrplot)

# Read dataset ----
df.imdb <- read_csv("IMDB Movie Dataset.csv")
imdbmovie <- df.imdb
IMDB_Movie_Dataset <- df.imdb
IMDB.df <- df.imdb

# Cor plot----
#Select numeric columns
nums <- unlist(lapply(df.imdb, is.numeric))  
df.imdb.filt <- df.imdb %>% 
  drop_na()

#Number of rows with NA value
df.imdb.num <- df.imdb.filt[,nums]

#Correleation
M <- cor(df.imdb.num)
#Plot
corrplot(M)

# Histogram -----
hist(IMDB_Movie_Dataset$imdb_score, main = "Total Count of IMDB Scores", xlab = "IMDB Score")
mean(IMDB_Movie_Dataset$imdb_score)
median(IMDB_Movie_Dataset$imdb_score)
mode(IMDB_Movie_Dataset$imdb_score)
quantile(IMDB_Movie_Dataset$imdb_score, c(.25,.5,.75), type = 1)


hist(IMDB_Movie_Dataset$imdb_score, main = "Total Count of IMDB Scores", xlab = "IMDB Score")
mean(IMDB_Movie_Dataset$imdb_score)
median(IMDB_Movie_Dataset$imdb_score)
mode(IMDB_Movie_Dataset$imdb_score)
quantile(IMDB_Movie_Dataset$imdb_score, c(.25,.5,.75), type = 1)


# Bar Chart ----
IMDB_Movie_Dataset <- df.imdb
ratingcount <- table(IMDB_Movie_Dataset$content_rating)
barplot(ratingcount, xlab = "Movie Rating", ylab = "Count of Movies", main ="Count of Movie Ratings")


ratingcount <- table(IMDB_Movie_Dataset$content_rating)
barplot(ratingcount, xlab = "Movie Rating", ylab = "Count of Movies", main ="Count of Movie Ratings")

boxplot(IMDB_Movie_Dataset$imdb_score ~ IMDB_Movie_Dataset$content_rating, xlab = "Content Rating", ylab = "IMDB Score",notch = TRUE)

plot(IMDB.df$imdb_score~IMDB.df$gross, xlab= "GROSS", ylab = "IMDB SCORE", log='xy',col="navyblue")
plot(IMDB.df$gross~IMDB.df$budget,xlab="BUDGET", ylab= "GROSS",log='xy', col="navyblue")

# Scatterplot Matrix ----
library(GGally)
ggpairs(IMDB.df[, c(4, 24, 26)])
ggpairs(IMDB.df[, c(5, 8, 25, 26, 28)])




# Handle Missing data ----

summary(IMDB.df$num_critic_for_reviews)
IMDB.df <- IMDB.df[!is.na(IMDB.df$num_critic_for_reviews),]
dim(IMDB.df)

summary(IMDB.df$duration)
IMDB.df <- IMDB.df[!is.na(IMDB.df$duration),]

summary(IMDB.df$director_facebook_likes)
IMDB.df$director_facebook_likes[is.na(IMDB.df$director_facebook_likes)] <- median(IMDB.df$director_facebook_likes, na.rm = TRUE)
summary(IMDB.df$director_facebook_likes)

summary(IMDB.df$actor_3_facebook_likes)
IMDB.df <- IMDB.df[!is.na(IMDB.df$actor_3_facebook_likes),]

summary(IMDB.df$actor_1_facebook_likes)
IMDB.df <- IMDB.df[!is.na(IMDB.df$actor_1_facebook_likes),]

summary(IMDB.df$gross)
IMDB.df$gross[is.na(IMDB.df$gross)] <- median(IMDB.df$gross, na.rm = TRUE)
summary(IMDB.df$gross)

summary(IMDB.df$budget)
IMDB.df$budget[is.na(IMDB.df$budget)] <- median(IMDB.df$budget, na.rm = TRUE)
summary(IMDB.df$budget)



# Dummy vars ----
IMDB.df <- IMDB.df %>% 
  drop_na()
dummies <- model.matrix(~0+color, data=IMDB.df)
dummies <- as.data.frame(dummies)
t(names(dummies))
head(dummies)
dummies <- dummies[,-1]
IMDB.df <- cbind(IMDB.df[,-c(1)], dummies)

