###############################################################
#########  LECTURE 8 - CLUSTERING IN R: IRIS EXAMPLE  #########
###############################################################

# IMPORTANT: Remember that clustering is an Unsupervised method. Therefore, you cannot use the label/class attribute! That is why, we are not using iris[,5], which has the Species of the dataset, at any point!

# A. CLUSTERING METHODS:
# A.1. Hierarchical clustering: 
hc= hclust(dist(iris[,1:4])) #applies hierarchical clustering
plot(hc) #shows the whole hierarchy
iris$HC3= cutree(hc,3) #stops hierarchy at level 3 and saves it in iris$HC3
iris$HC3
table(iris$Species,iris$HC3) #shows clusters class label according to clusters
pairs(iris[,1:4],col=iris$HC3) #shows pairs for all clusters/attributes

# A.2. K-means clustering:
# Note: we are not giving the kmeans the initial means! As a consequence, these will be chosen randomly each time. So, the results of K-means might be different each time!
km3= kmeans(iris[,1:4],3,iter.max=100) #applies k-means with 3 clusters and 100 iterations


#kmeans returns several values, including intra and extra measurements.
#cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: A matrix of cluster centres.
#totss:	The total sum of squares.
#withinss:	Vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
#betweenss:The between-cluster sum of squares, i.e. totss-tot.withinss.
#size:The number of points in each cluster.
#iter: The number of (outer) iterations.
#ifault: integer: indicator of a possible algorithm problem – for experts.
km3


iris$KM3= km3$cluster #saves clusters in iris$KM3
# IMPORTANT: Check the value of iris$KM3. If you notice, the clustering result is just a list of the cluster each point belongs to, nothing more!
iris$KM3

table(iris$Species,iris$KM3) #shows clusters class label according to clusters
pairs(iris[,1:4],col=iris$KM3) #shows pairs for all clusters/attributes

#A.2.1. Visualisation: We will compare the distributions of the iris dataset once K-means is done
#Boxplot by class
par(mfrow=c(2,2))
boxplot(iris[,1:4],las=3,main="whole data")
boxplot(iris[iris$Species=="setosa",1:4],las=3,main="setosa")
boxplot(iris[iris$Species=="versicolor",1:4],las=3,main="versicolor")
boxplot(iris[iris$Species=="virginica",1:4],las=3,main="virginica")

#Boxplot by clusters
par(mfrow=c(2,2))
boxplot(iris[,1:4],las=3,main="whole data")
boxplot(iris[iris$KM3==1,1:4],las=3,main="Cluster 1")
boxplot(iris[iris$KM3==2,1:4],las=3,main="Cluster 2")
boxplot(iris[iris$KM3==3,1:4],las=3,main="Cluster 3")

# A.3. PAM clustering:
library(cluster)

pam3 = pam(iris[,1:4], 3)

#Similarly to k-means, PAM also returns several values:
pam3

iris$PAM3 = pam3$clustering #Saves clustering result only
table(iris$Species,iris$PAM3)
pairs(iris[,1:4],col=iris$PAM3) 


# A.4. FCM (Fuzzy) clustering:
#install.packages("e1071", dep = TRUE) #installs relevant library
library(e1071) #loads library

fcm3= cmeans(iris[,1:4],3,iter.max=100)

#Similarly to k-means and PAM, cmeans also returns several values
fcm3
fcm3$centers
fcm3$membership

iris$FCM3= fcm3$cluster #We are keeping the clustering results (i.e. the cluster with highest prob)
table(iris$Species,iris$FCM3) #note how clusters are not aligned
pairs(iris[,1:4],col=iris$FCM3)

#you can align clusters as follows:
iris$FCM3= 4 - fcm3$cluster #rearranges order - this will depend your data and solution, so you may have to adjust!
table(iris$Species,iris$FCM3) #now cluster 1 is setosa, cluster 2 is vericolor and cluster 3 is virginica
pairs(iris[,1:4],col=iris$FCM3) 

iris

# We use simple plots to show the different results obtained by clustering
layout(mat = matrix(c(1,1,2,3,4,5), nrow = 3, byrow = TRUE))
p1=plot(iris[,1:2], col=iris$Species, main="Original Iris dataset")
p2=plot(iris[,1:2], col=iris$HC3,main="HCA cust")
p3=plot(iris[,1:2], col=iris$KM3,main="K-means clust")
p4=plot(iris[,1:2], col=iris$PAM3, main="PAM clust")
p5=plot(iris[,1:2], col=iris$FCM3,main="Fuzzy clust")

# B. STATS: You can calculate two different families of metrics, depending on whether or not you really have the label class.
# 
# B.1. INTERNAL METRICS: When a clustering result is evaluated based on the data that was clustered itself (the class/label is not used at all). 
# These methods usually assign the best scores algorithms that produce high similarity within a cluster and low similarity between clusters. This is refered to as:
#####    1. inter-cluster distance: Distance between clusters (summatory of the distance between the different cluster centroids). The higher the better. 
#####    2. intra-cluster distance: Distance between the points in a cluster (the summatory of the distance between the cluster members to the center of the cluster). The lower the better.
# 
# IMPORTANT: One drawback of using internal criteria in cluster evaluation is that high scores on an internal measure do not necessarily result in effective information retrieval applications.

# You can use the package fpc to generate cluster valudation statistics
#install.packages("fpc")
library(fpc)

distance = dist(iris[,1:4]);

statisticsFCM3 = cluster.stats(distance, iris$FCM3, iris$HC3) 
statisticsHC3 = cluster.stats(distance, iris$HC3, iris$FCM3)

statisticsFCM3 = cluster.stats(distance, iris$FCM3, iris$HC3) 
statisticsKM3 = cluster.stats(distance, iris$KM3)
#### IMPORTANT: this is not comparing both methods. 
#### The third argument is a clustering, indicating an alternative clustering. If provided, the corrected Rand index and Meila's VI for clustering vs. alt.clustering are computed. ONLY THEN THEY ARE COMPARED

##### From "statistics", these are the two metrics that are the most used:
####   1. $diameter: The bigger the diameter of the cluster, the worst the clustering (points that belong to the cluster are more scattered)
#####  2. $average.distance: The higher the average distance of each clustering, the worst the clustering method. (Let's assume that the average distance is the average of the distances from each point in the cluster to the center of the cluster.). Also useful: $median.distance
##### 


statisticsFCM3$diameter
statisticsHC3$diameter
statisticsKM3$diameter

mean(statisticsFCM3$diameter)
mean(statisticsHC3$diameter)
mean(statisticsKM3$diameter)

statisticsFCM3$average.distance
statisticsHC3$average.distance
statisticsKM3$average.distance

statisticsFCM3$median.distance
statisticsHC3$median.distance
statisticsKM3$median.distance

# Other insteresting metrics
#### 1. $dunn: minimum separation / maximum diameter. Dunn index, see Halkidi et al. (2002).
#### 2. $dunn2: minimum average dissimilarity between two cluster / maximum average within cluster dissimilarity, another version of the family of Dunn indexes.
#### 3. $pearsongamma: correlation between distances and a 0-1-vector where 0 means same cluster, 1 means different clusters. "Normalized gamma" in Halkidi et al. (2001).
#### 4. $entropy: entropy of the distribution of cluster memberships, see Meila(2007).
#### 5. cwidegap: vector of widest within-cluster gaps.


#B.2. EXTERNAL METRICS: when clustering results are evaluated using the known classes of the dataset.

#The major challenge here is: how do you know which class belongs to which cluster?
#We will see more external measures once we cover classification.
#Here we are going to introduce only three simple ones: Confusion matrix, true positive rate


#Confusion matrix - aligns clusters with classes so that the diagonal is maximised
t1 = table(iris$Species, iris$KM3)
t2= table(iris$Species, iris$HC3)

#Align confusion matrix automatically - this is only possible in clustering (since we don't know which class is which cluster)
res_1 = maximise_diag(t1)
t11=res_1[[1]]
res_2 = maximise_diag(t2)
t22 = res_2[[1]]

# True positives rate:
sum(diag(t11))/sum(t11)
sum(diag(t22))/sum(t22)


#Purity: Measure of the extent to which clusters contain a single class. For each cluster, count the number of data points from the most common class in said cluster. Now take the sum over all clusters and divide by the total number of data points.
#col_max = apply(t11, 2, max);
#sum(col_max)/sum(t11)

#col_max = apply(t22, 2, max);
#sum(col_max)/sum(t22)

#TP rate and purity are the same in this case because we have aligned the results.