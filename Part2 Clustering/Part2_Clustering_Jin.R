
# https://www.statmethods.net/advstats/cluster.html


################################## Part 2 Clustering data ############################################

setwd('/Users/zhaojin/Desktop/NowTasks/DMA_R_Cw/DMA_CW_Codes/Part2')

library(ggplot2)
library(dplyr)


plt <- read.table(file = "g54dma-plant-dataset.csv",header = TRUE,sep = ',')

plt_pca_7 =  read.table(file = "plt_pca_fiii.csv",header = TRUE,sep = ',')
plt_pca_7$Class = plt$Class

stdd_plt_fi =  read.table(file = "stdd_plt_fi.csv",header = TRUE,sep = ',')
head(stdd_plt_fi)

################################## 2-a ##################################################################

##################################### Class itself ##########################################################################

plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(plt_pca_7$Class), 5)
colcode = plotclr[colornum] # assign color

jpeg("Figure 34 Classes in Attributes.jpg", width = 800, height = 700)
pairs(plt_pca_7[,2:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 34 Classes in Attributes (dataset: plt_pca_7)',
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5) #shows pairs for all clusters/attributes
par(xpd = TRUE)
legend("bottomright",
       fill = rainbow(5),
       legend = c("A", "B","C", "D",'E'))
dev.off()


################# A. CLUSTERING METHODS: 层次聚类算法 ##########################################################################

# IMPORTANT: Clustering is an Unsupervised method. Therefore, label/class attribute cannot be used! 

##################################
# A.1. HCA: Hierarchical clustering: 

ncol(plt_pca_7)
head(plt_pca_7)

# A.1. HCA: Hierarchical clustering: 
hc = hclust(dist(plt_pca_7[,2:8])) #applies hierarchical clustering
plot(hc) #shows the whole hierarchy
plt_pca_7$HC5 = cutree(hc,5) #stops hierarchy at level 5 and saves it in plt_pca_7$HC5
plt_pca_7$HC5

table(plt_pca_7$Class,plt_pca_7$HC5) #shows clusters class label according to clusters


#############################################################
plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(plt_pca_7$HC5), 5)
colcode = plotclr[colornum] # assign color
##############################################################

jpeg("Figure 31 Results of HCA.jpg", width = 800, height = 700)
pairs(plt_pca_7[,2:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 31 Results of HCA (dataset: plt_pca_7)',
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5) #shows pairs for all clusters/attributes
par(xpd = TRUE)
legend("bottomright",
       fill = rainbow(5),
       legend = c("1", "2","3", "4",'5'))
dev.off()

################# A.2. K-means clustering 均值聚类算法########################################################################################################################

# Note: we are not giving the kmeans the initial means! As a consequence, these will be chosen randomly each time. So, the results of K-means might be different each time!
km5 = kmeans(plt_pca_7[,2:8],5,iter.max=100) #applies k-means with 3 clusters and 100 iterations

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
km5


plt_pca_7$KM5= km5$cluster #saves clusters in plt_pca_7$KM3
# IMPORTANT: Check the value of plt_pca_7$KM3. If you notice, the clustering result is just a list of the cluster each point belongs to, nothing more!
plt_pca_7$KM5
plt_pca_7$Class

table(plt_pca_7$Class,plt_pca_7$KM5) #shows clusters class label according to clusters

#############################################################
plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(plt_pca_7$KM5), 5)
colcode = plotclr[colornum] # assign color
##############################################################

jpeg("Figure 32 Results of K-Means.jpg", width = 800, height = 700)
pairs(plt_pca_7[,2:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 32 Results of K-Means (dataset: plt_pca_7)',
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5) #shows pairs for all clusters/attributes
par(xpd = TRUE)
legend("bottomright",
       fill = rainbow(5),
       legend = c("1", "2","3", "4",'5'))
dev.off()


#A.2.1. Visualisation: We will compare the distributions of the plt_pca_7 dataset once K-means is done
#Boxplot by class
par(mfrow=c(2,2))
boxplot(plt_pca_7[,2:8],las=3,main="whole data")
boxplot(plt_pca_7[plt_pca_7$Class=="A",2:8],las=3,main="A")
boxplot(plt_pca_7[plt_pca_7$Class=="B",2:8],las=3,main="B")
boxplot(plt_pca_7[plt_pca_7$Class=="C",2:8],las=3,main="C")
boxplot(plt_pca_7[plt_pca_7$Class=="D",2:8],las=3,main="D")
boxplot(plt_pca_7[plt_pca_7$Class=="E",2:8],las=3,main="E")
#Boxplot by clusters
par(mfrow=c(2,2))
boxplot(plt_pca_7[,2:8],las=3,main="whole data")
boxplot(plt_pca_7[plt_pca_7$KM5==1,2:8],las=3,main="Cluster 1")
boxplot(plt_pca_7[plt_pca_7$KM5==2,2:8],las=3,main="Cluster 2")
boxplot(plt_pca_7[plt_pca_7$KM5==3,2:8],las=3,main="Cluster 3")
boxplot(plt_pca_7[plt_pca_7$KM5==4,2:8],las=3,main="Cluster 4")
boxplot(plt_pca_7[plt_pca_7$KM5==5,2:8],las=3,main="Cluster 5")

################# A.3. PAM clustering - Partitioning Around Medoids 中心点算法 ########################################################################################################################

# A.3. PAM clustering:
library(cluster)

PAM5 = pam(plt_pca_7[,2:8], 5)

#Similarly to k-means, PAM also returns several values:
PAM5

plt_pca_7$PAM5 = PAM5$clustering #Saves clustering result only
table(plt_pca_7$Class, plt_pca_7$PAM5)

#############################################################
plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(plt_pca_7$PAM5), 5)
colcode = plotclr[colornum] # assign color
##############################################################

jpeg("Figure 33 Results of PAM.jpg", width = 800, height = 700)
pairs(plt_pca_7[,2:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 33 Results of PAM (dataset: plt_pca_7)',
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5) #shows pairs for all clusters/attributes
par(xpd = TRUE)
legend("bottomright",
       fill = rainbow(5),
       legend = c("1", "2","3", "4",'5'))
dev.off()



################################# Compare the results ######################################################################################

#######################################################################################################################
# We use simple plots to show the different results obtained by clustering
jpeg("Figure 35 Compare PC1 in 3 ways.jpg", width = 800, height = 700)
layout(mat = matrix(c(1,1,2,2,3,3,4,4), nrow = 4, byrow = TRUE))
p1=plot(plt_pca_7[,1:2], col=plt_pca_7$Class, main="Original plt_pca_7 dataset",
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
p2=plot(plt_pca_7[,1:2], col=plt_pca_7$HC5,main="HCA cust",
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
p3=plot(plt_pca_7[,1:2], col=plt_pca_7$KM5,main="K-means clust.",
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
p4=plot(plt_pca_7[,1:2], col=plt_pca_7$PAM5, main="PAM clust.",
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
dev.off()


###################################################################################################################
# vary parameters for most readable graph
library(cluster) 

jpeg("Figure 36 Cluster_Compare 3 ways with Original Class.jpg", width = 800, height = 700)

layout(mat = matrix(c(1,1,2,2,3,3,4,4), nrow = 4, byrow = TRUE))

c1 = clusplot(plt_pca_7, plt_pca_7$Class, color=TRUE, shade=TRUE, 
              main="Original plt_pca_7 dataset",
              cex.lab=1.5, cex.axis=1.5, cex.main=2, 
              labels=0.3, lines=0)
c2 = clusplot(plt_pca_7, plt_pca_7$HC5, color=TRUE, shade=TRUE,
              main="HCA clust",
              cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
              labels=0.3, lines=0)

c3 = clusplot(plt_pca_7, plt_pca_7$KM5, color=TRUE, shade=TRUE,
              main="K-means clust",
              cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
              labels=0.3, lines=0)

c4 = clusplot(plt_pca_7, plt_pca_7$PAM5, color=TRUE, shade=TRUE,
              main="PAM clust",
              cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
              labels=0.3, lines=0)
dev.off()


###################################################################################################################


