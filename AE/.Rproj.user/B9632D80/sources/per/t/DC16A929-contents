
# Script for ...


######## 0. Set the working environemnt

# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# Set the working folder
# getwd()
setwd("D:/School Stuff/40 .220 The Analytics Edge/W10/1) Recommendation Systems")

# Load the MovieLens datasets
movies <- read.csv("movies.csv",stringsAsFactors=FALSE)
# With the option stringsAsFactors=FALSE we make sure that character vectors are 
# not converted into factors.
str(movies) # The dataframe consists of two variables with 8569 movies with movieID and title
head(movies) # First part of the dataframe
summary(movies) # Summary of the data

# To read in the movie genres, we use the following commands 
# This helps count the number of fields separated by "|" in each row of genres.csv
countfields <- count.fields("genres.csv", sep="|") 
countfields[1] # The first movie, for example has 5 genres listed
# With this command, we can see that movies have from 1 to 7 genres listed
min(countfields)
max(countfields)

# Now, we load genres.csv. The command creates a dataframe with 8569 observations 
# and 7 variables with column names X1 ... X7.
genres <- read.csv("genres.csv", header=FALSE, sep="|", col.names=c("X1","X2","X3","X4","X5","X6","X7"))
#
# Note that each variable has different number of levels. 
str(genres) 
# To obtain the overall set, we use
fac <- union(union(union(union(union(union(levels(genres$X1), levels(genres$X2)),levels(genres$X3)),
                               levels(genres$X4)), levels(genres$X5)), levels(genres$X6)),levels(genres$X7))
fac # fac has a total of 20 categories, from Action to IMAX, where "" simply illustrates missing category.
str(fac)

# To standardize across all variables
genres$X1 <- factor(genres$X1, fac)
genres$X2 <- factor(genres$X2, fac)
genres$X3 <- factor(genres$X3, fac)
genres$X4 <- factor(genres$X4, fac)
genres$X5 <- factor(genres$X5, fac)
genres$X6 <- factor(genres$X6, fac)
genres$X7 <- factor(genres$X7, fac)
levels(genres$X1)

# Then, we create a matrix with 8569 rows (movies) and 20 columns (categories), 
# and we assign the column name to matrix as genres
M <- matrix(0,nrow=8569,ncol=20)
colnames(M) <- fac
# With this command, we create a matrix with entry 1 if a movie is of a particular genre (and 0 otherwise)
for(i in 1:8569)
{M[i,genres[i,"X1"]] <- 1
M[i,genres[i,"X2"]] <- 1
M[i,genres[i,"X3"]] <- 1 
M[i,genres[i,"X4"]] <- 1 
M[i,genres[i,"X5"]] <- 1 
M[i,genres[i,"X6"]] <- 1
M[i,genres[i,"X7"]] <- 1}

# Last step, we create a dataframe (named "Data") with movie title and classification
Data <- as.data.frame(M)
Data$title <- movies$title
Data <- Data[,-19] # Drops the 19th column, which corresponds to the "" category


######## 1. Hierarchical clustering

# With the function dist, we calculate the distance 
# between movies using the first 19 columns (genres of movies)
distances <- dist(Data[,1:19], method="euclidean")
dim(Data)
# Note that we computed 8569*8568/2 distances
length(distances)

# Execute hierarchical clustering. We use Ward's distance method to find compact clusters.
clusterMovies1 <- hclust(distances, method="ward.D2")
# Plots the dendrogram. We have several movies, so the lists at the bottom cannot be read
plot(clusterMovies1) 

# Let's then cut the dendrogram into 10 clusters
clusterGroups1 <- cutree(clusterMovies1, k=10)
# Compute the average value across the clusters for the Action variable. 
# Higher value indicates many movies in the clusters are action movies.
tapply(Data[,1], clusterGroups1, mean) 

# Create a matrix "Cat1" where rows denote categories and columns indicate clusters
Cat1 <- matrix(0,nrow=19,ncol=10)
for(i in 1:19)
{Cat1[i,] <- tapply(Data[,i], clusterGroups1, mean)}
rownames(Cat1) <- colnames(Data)[1:19]
Cat1
# Looking at Cat1, we can infer that the 10 clusters are organized as follows:
# Cluster 1: Fantasy, Comedy, Children, Adventure
# Cluster 2: Romance, Comedy, Drama
# Cluster 3: Comedy, Drama
# Cluster 4: Drammar, Thriller, Crime
# Cluster 5: Sci-fi, Adventure, Action
# Cluster 6: Horror, Thriller
# Cluster 7: Drama
# Cluster 8: Romance, Drama
# Cluster 9: Documentary
# Cluster 10: War, Drama, Action

# Let's take a look at various movies
# subset(Data$title, clusterGroups1==6)
subset(Data, movies$title=="Grand Budapest Hotel, The (2014)")
clusterGroups1[8418]
subset(Data, movies$title=="Moneyball (2011)")
clusterGroups1[7925]
subset(Data, movies$title=="X-Men: First Class (2011)")
clusterGroups1[7849]


######## 2. K-means clustering

set.seed(1)
# This performs a K-means clustering using k=10 and 20 random initial configurations
# (a random set of rows is chosen as initial centers)
clusterMovies2 <- kmeans(Data[,1:19],centers=10,nstart=20)
# Total within cluster sum of squares (we want this number to be small)
clusterMovies2$tot.withinss # 7324.78
#
# Let's try again, but, this time, with just one initial configuration
set.seed(1)
clusterMovies3 <- kmeans(Data[,1:19],centers=10,nstart=1)
clusterMovies3$tot.withinss # 7601.802
# We can see that picking more starting point is useful!

# Let's now vary the value of k and plot the error in the cluster as a function of k
set.seed(1)
fit <- 0
for(k in 1:15){
  clusterMovies4 <- kmeans(Data[,1:19], centers=k, nstart=20)
fit[k] <- clusterMovies4$tot.withins
}
plot(1:15,fit)

# Let's see the corresponding clusters
Cat2 <- matrix(0,nrow=19,ncol=10)
for(i in 1:19)
{Cat2[i,] <- tapply(Data[,i], clusterMovies2$cluster, mean)}
rownames(Cat2) <- colnames(Data)[1:19]
Cat2
# Looking at Cat2, we can infer that the 10 clusters are organized as follows:
# Cluster 1: Horror
# Cluster 2: Crime, Drama, Thriller, Action
# Cluster 3: Drama
# Cluster 4: Documentary
# Cluster 5: Action, Adventure, Sci-Fi, Thriller
# Cluster 6: Comedy
# Cluster 7: Thriller, Horror
# Cluster 8: Children, Adventure, Animation, Fantasy, Comedy
# Cluster 9: War, Drama
# Cluster 10: Comedy, Drama


