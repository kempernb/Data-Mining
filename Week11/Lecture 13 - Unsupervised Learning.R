fp.df <- read.csv("Faceplate.csv")
fp.df

# remove first column and convert to matrix
fp.mat <- as.matrix(fp.df[, -1])
fp.mat

# convert the binary incidence matrix into a transactions database
# install.packages("arules")
library(arules)
fp.trans <- as(fp.mat, "transactions")
`inspect(fp.trans)
`
## get rules
# when running apriori(), include the minimum support, minimum confidence, and target
# as arguments.
rules <- apriori(fp.trans, parameter = list(support = 0.2, conf = 0.5, target = "rules"))

# inspect the rules, sorted by their lift
 

all.books.df <- read.csv("CharlesBookClub.csv")

# create a binary incidence matrix
count.books.df <- all.books.df[, 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
incid.books.mat <- as.matrix(incid.books.df)
head(incid.books.mat, 20)

# convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(head(books.trans, 20))

# plot data
itemFrequencyPlot(books.trans, ylim = c(0,0.5))

# run apriori function
rules <- apriori(books.trans,
                 parameter = list(supp = 200/4000, conf = 0.5, target = "rules"))

# inspect the top 10 rules sorted by lift
inspect(head(sort(rules, by = "lift"), 10))


utilities.df <- read.csv("Utilities.csv")

plot(Fuel_Cost ~ Sales, data = utilities.df, xlim = c(0, 20000), ylim = c(0, 2.5))
text(Fuel_Cost ~ Sales, labels = Company, data = utilities.df, pos = 4, 
     cex = 0.8)

# set row names to the Company column
row.names(utilities.df) <- utilities.df[, 1]

# remove the Company column
utilities.df <- utilities.df[, -1]

# normalize input variables
library(caret)
norm.values <- preProcess(utilities.df, method = c("center", "scale"))
utilities.df.norm <- predict(norm.values, utilities.df)
utilities.df.norm

# compute normalized distances
d.norm <- dist(utilities.df.norm, method = "euclidean")
d.norm

# in hclust() set argument method =
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
abline(h=3.5, col = "red")
abline(h=2.6, col = "green")

## computing cluster membership by "cutting" the dendrogram
# single linkage
memb <- cutree(hc1, k = 6)
memb

## creating heatmap
# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df.norm), sep = "")
utilities.df.norm <- utilities.df.norm[order(row.names(utilities.df.norm)), ]

# plot heatmap
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, Rowv = NA, hclustfun = hclust, 
        margins = c(10, 5), col = rev(paste("gray", 1:99, sep = "")))

## k-means clustering
# load and preprocess data
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[, 1]
utilities.df <- utilities.df[, -1]

# normalized distance
norm.values <- preProcess(utilities.df, method = c("center", "scale"))
utilities.df.norm <- predict(norm.values, utilities.df)
utilities.df.norm

# run kmeans algorithm
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
km$cluster

# centroids
km$centers

# within-cluster sum of squares
km$withinss

# cluster size
km$size

## plotting profile plot of centroids
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df.norm))

# plot centroids
for (i in c(1:6)) {
  lines(km$centers[i, ], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),"black", "dark gray"))
}

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))
