""" 
Clustering ! 

""" 
# Load in text file
movies = read.table("movieLens.txt", header =FALSE, sep="|", quote = "\"")
# Look at # of observations and variables!
# since we know the variables, we'll add the column names.
str(movies)

colnames(movies) = c("ID","Title","ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown","Action","Adventure","Animation","Childrens", "Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
str(movies)

# Remove variables not going to be used.

movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicate entries with unique function

movies = unique(movies)

# Check changes
str(movies)

# Hierarchical Clustering .. 

# 2 steps  
# 1. compute distances
# 2. Cluster points

# CLuster bases only on genre variable not title variable!

distances = dist(movies[2:20], method="euclidean")
clusterMovies = hclust(distances,method="ward.D") #ward centroid distance and variance.


plot(clusterMovies)   # Dendrogram

# We can label each of the data points according to what cluster it belongs to using the cutree function! We decided 10 clusters.

clusterGroups = cutree(clusterMovies, k = 10)

# View % of movies in each genre cluster using tapply
tapply(movies$Action, clusterGroups, mean)  # This divides out data points into 10 clusters, then coputes the avergae value of the action  variable for each cluster.
# Action variables is binary value, were computing % of movies in this cluster that belong to that genre!

# In cluster 2, 78% of movies have action genre label. Cluster 4, none are action movies!

# Try again, but with romance.

tapply(movies$Romance, clusterGroups, mean)
# All movies in cluster 6 & 7, where as only 4% of movies in cluster 2 are labeled romance.

# apply this to all, and we can see which genres are mostly associated in each cluster!

# Lets figure out what cluster men in black is in!
subset(movies, Title=="Men in Black (1997)")  # 257th row!

clusterGroups[257] # went into cluster 2. Action/Adventure/SciFi cluster!

#Create new dataset with movies in cluster 2!
cluster2 = subset(movies, clusterGroups==2)
cluster2$Title[1:10] # shows which movies are in cluster 2.
