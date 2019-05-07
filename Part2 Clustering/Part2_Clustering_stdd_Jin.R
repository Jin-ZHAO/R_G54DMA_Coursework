
# https://www.statmethods.net/advstats/cluster.html
# https://www.zhihu.com/question/19982667
# Standardization / Normalization  https://www.cnblogs.com/bjwu/p/8977141.html
# remove outliers http://rpubs.com/Mentors_Ubiqum/removing_outliers
# remove outliers https://www.zhihu.com/question/60066149/answer/173034959
# Improving K-Means by removing outliers http://cs.joensuu.fi/~villeh/35400978.pdf


library(ggplot2)
library(dplyr)
source('Normal_Standard_Mean.R')
source('summary.R')

plt <- read.table(file = "g54dma-plant-dataset.csv",header = TRUE,sep = ',')

imp_plt =  read.table(file = "imp_plt.csv",header = TRUE,sep = ',')


################################## Part 2 Clustering data ############################################

###### Before clustering, I did further pre-process to the dataset 'imp_plt' (from 1-a-fi) #################################

# 1: delete the high correlated columns ###########################################
imp_plt1 = subset(imp_plt, select = -Orientation0)
imp_plt1 = subset(imp_plt1, select = -Orientation1)
imp_plt1 = subset(imp_plt1, select = -Orientation5)
imp_plt1 = subset(imp_plt1, select = -Orientation6)
imp_plt1 = subset(imp_plt1, select = -Orientation3)
imp_plt1 = subset(imp_plt1, select = -Orientation8)
imp_plt1 = subset(imp_plt1, select = -Orientation4)
imp_plt1 = subset(imp_plt1, select = -Orientation7)
imp_plt1 = subset(imp_plt1, select = -Depth)
imp_plt2 = imp_plt1[3:11]
nrow(imp_plt2)


# 2: delete the outliers ###########################################

round(analysis(imp_plt2[,1:8]),3) 

# choose attributes with highest sd, 'LeafArea'
outliers <- boxplot(imp_plt2[,7])$out # get the outliers
imp_plt3 <- imp_plt2[-which(imp_plt2[,7] %in% outliers),] #remove the rows containing the outliers
round(analysis(imp_plt3[,1:8]),3)
nrow(imp_plt3) #595

# check the outliers of each attribute
boxplot(imp_plt3[,1])$out # the outliers are near the Q3+1.5*IQR, so I kepted it.
outliers <- boxplot(imp_plt3[,1])$out # get the outliers
imp_plt4 <- imp_plt3[-which(imp_plt3[,1] %in% outliers),] #remove the rows containing the outliers
round(analysis(imp_plt4[,1:8]),1)
nrow(imp_plt4) #589

boxplot(imp_plt4[,2])$out # without outliers
boxplot(imp_plt4[,3])$out 
outliers <- boxplot(imp_plt4[,3])$out # get the outliers
imp_plt5 <- imp_plt4[-which(imp_plt4[,3] %in% outliers),] #remove the rows containing the outliers
round(analysis(imp_plt5[,1:8]),3)
nrow(imp_plt5) #586

boxplot(imp_plt5[,4])$out  # without outliers
boxplot(imp_plt5[,5])$out  # without outliers
boxplot(imp_plt5[,6])$out  # without outliers
boxplot(imp_plt5[,8])$out  # without outliers
outliers <- boxplot(imp_plt6[,8])$out # get the outliers
imp_plt6 <- imp_plt6[-which(imp_plt5[,8] %in% outliers),] #remove the rows containing the outliers
round(analysis(imp_plt5[,1:8]),3)
nrow(imp_plt6) #569


# 3: Finally, Standardized the dataset by the function written before.###########################################
stdd_plt = standard_z(imp_plt6[,1:8])
stdd_plt$Class = imp_plt6$Class
boxplot(stdd_plt[,1:8])
head(stdd_plt) # 8 attributes
ncol(stdd_plt) # 9
nrow(stdd_plt) # 569 instances

##################################### Class itself ##########################################################################

plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(stdd_plt$Class), 5)
colcode = plotclr[colornum] # assign color

jpeg("Figure 34 Classes in Attributes.jpg", width = 800, height = 700)
pairs(stdd_plt[,1:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 34 Classes in Attributes (dataset: stdd_plt)',
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
hc = hclust(dist(stdd_plt[,1:8])) #applies hierarchical clustering
plot(hc) #shows the whole hierarchy
stdd_plt$HC5 = cutree(hc,5) #stops hierarchy at level 5 and saves it in stdd_plt$HC5
stdd_plt$HC5

#############################################################
plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(stdd_plt$HC5), 5)
colcode = plotclr[colornum] # assign color
##############################################################

jpeg("Figure 31 Results of HCA.jpg", width = 800, height = 700)
pairs(stdd_plt[,1:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 31 Results of HCA (dataset: stdd_plt)',
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5) #shows pairs for all clusters/attributes
par(xpd = TRUE)
legend("bottomright",
       fill = rainbow(5),
       legend = c("1", "2","3", "4",'5'))
dev.off()

################# A.2. K-means clustering 均值聚类算法########################################################################################################################

# Note: we are not giving the kmeans the initial means! As a consequence, these will be chosen randomly each time. So, the results of K-means might be different each time!
km5 = kmeans(stdd_plt[,1:8],5,iter.max=100) #applies k-means with 3 clusters and 100 iterations

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


stdd_plt$KM5= km5$cluster #saves clusters in stdd_plt$KM3
# IMPORTANT: Check the value of stdd_plt$KM3. If you notice, the clustering result is just a list of the cluster each point belongs to, nothing more!
stdd_plt$KM5
stdd_plt$Class

#############################################################
plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(stdd_plt$KM5), 5)
colcode = plotclr[colornum] # assign color
##############################################################

jpeg("Figure 32 Results of K-Means.jpg", width = 800, height = 700)
pairs(stdd_plt[,1:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 32 Results of K-Means (dataset: stdd_plt)',
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5) #shows pairs for all clusters/attributes
par(xpd = TRUE)
legend("bottomright",
       fill = rainbow(5),
       legend = c("1", "2","3", "4",'5'))
dev.off()



################# A.3. PAM clustering - Partitioning Around Medoids 中心点算法 ########################################################################################################################

# A.3. PAM clustering:
library(cluster)

PAM5 = pam(stdd_plt[,1:8], 5)

#Similarly to k-means, PAM also returns several values:
PAM5

stdd_plt$PAM5 = PAM5$clustering #Saves clustering result only


#############################################################
plotclr = rainbow(5)
# the code of color: red, yellow, seagreen, blueviolet , darkblue
colornum = cut(rank(stdd_plt$PAM5), 5)
colcode = plotclr[colornum] # assign color
##############################################################

jpeg("Figure 33 Results of PAM.jpg", width = 800, height = 700)
pairs(stdd_plt[,1:8],col=colcode,oma=c(5,5,5,15),
      main = 'Figure 33 Results of PAM (dataset: stdd_plt)',
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5) #shows pairs for all clusters/attributes
par(xpd = TRUE)
legend("bottomright",
       fill = rainbow(5),
       legend = c("1", "2","3", "4",'5'))
dev.off()



################################# Compare the results ######################################################################################
###################################################################################################################

t1= table(stdd_plt$Class,stdd_plt$HC5) 
t1
t2 = table(stdd_plt$Class,stdd_plt$KM5)
t2
t3 = table(stdd_plt$Class, stdd_plt$PAM5)
t3

# Confusion matrix - aligns clusters with classes so that the diagonal is maximised


#Align confusion matrix automatically - this is only possible in clustering (since we don't know which class is which cluster)

#######################################################################################################################
# 00 We use simple plots to show the different results obtained by clustering
# jpeg("Figure 34 Compare  in 3 ways.jpg", width = 800, height = 700)
# layout(mat = matrix(c(1,1,2,2,3,3,4,4), nrow = 4, byrow = TRUE))
# p1=plot(stdd_plt[,1:2], col=stdd_plt$Class, main="Original stdd_plt dataset",
#        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
# p2=plot(stdd_plt[,1:2], col=stdd_plt$HC5,main="HCA cust",
#      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
# p3=plot(stdd_plt[,1:2], col=stdd_plt$KM5,main="K-means clust.",
#      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
# p4=plot(stdd_plt[,1:2], col=stdd_plt$PAM5, main="PAM clust.",
#       cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
# dev.off()


###################################################################################################################
# 00 vary parameters for most readable graph
# library(cluster) 

# jpeg("Figure 36 Cluster_Compare 3 ways with Original Class.jpg", width = 800, height = 700)

# layout(mat = matrix(c(1,1,2,2,3,3,4,4), nrow = 4, byrow = TRUE))

# c1 = clusplot(stdd_plt, stdd_plt$Class, color=TRUE, shade=TRUE, 
#             main="Original stdd_plt dataset",
#             cex.lab=1.5, cex.axis=1.5, cex.main=2, 
#             labels=0.3, lines=0)
# c2 = clusplot(stdd_plt, stdd_plt$HC5, color=TRUE, shade=TRUE,
#             main="HCA clust",
#             cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
#             labels=0.3, lines=0)
#
# c3 = clusplot(stdd_plt, stdd_plt$KM5, color=TRUE, shade=TRUE,
#            main="K-means clust",
#             cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
#             labels=0.3, lines=0)
#
# c4 = clusplot(stdd_plt, stdd_plt$PAM5, color=TRUE, shade=TRUE,
#             main="PAM clust",
#             cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
#             labels=0.3, lines=0)
# dev.off()


