library(ggplot2)
library(dplyr)
library(scatterplot3d)
library(RColorBrewer)
source('1-A-i-summary.R')
source('replace_NA_3ways.R')
source('Normal_Standard_Mean.R')

plt <- read.table(file = "g54dma-plant-dataset.csv",header = TRUE,sep = ',')
head(plt)


############################## 1-a-i ###############################################################

# call the function written in '1-A-i-summary.R'
sum_plt = descriptive(plt[2:19])  
sum_plt

##############################  1-a-ii ##############################################################

# see file='1-A-ii-histogram.R'
# also, call the function written in '1-A-i-summary.R'

sum_plt2 = distribution(plt[2:19])
sum_plt2


###############################  1-b-i & ii & iii #####################################################

# see file='1-B-i-ii-iii-correlation.R'


#############################  1-b-iv #################################################################

#see the file='1-B-iv-scatterplots.R'


#############################  1-d ###################################################################

# call the function written in file 'replace_NA_3ways.R'


# Replace NA by O
plt_NA_0 = NA_by_0(plt)
write.csv(plt_NA_0,file = 'plt_NA_0.csv')


# Replace NA by mean
plt_NA_mean = NA_by_mean(plt)
write.csv(plt_NA_mean,file = 'plt_NA_mean.csv')


# Replace NA by mean
plt_NA_median = NA_by_median(plt)
write.csv(plt_NA_median,file = 'plt_NA_median.csv')


###############  1-e Transforming Data from Lec 5 ######################################################

# call the function written in file 'Normal_Standard_Mean.R'


###############  1-e-1 Mean Centring #################################

# Call the function
mean_centre_plt_NA_0 = mean_centre(plt_NA_0)
mean_centre_plt_NA_0
mean_centre_plt_NA_median = mean_centre(plt_NA_median)
mean_centre_plt_NA_median
mean_centre_plt_NA_mean = mean_centre(plt_NA_mean)
mean_centre_plt_NA_mean

###############  1-e-2 Standardisation (Z-Score) #################################

# Call the function
standard_z_plt_NA_0 = standard_z(plt_NA_0)
standard_z_plt_NA_0
standard_z_plt_NA_median = standard_z(plt_NA_median)
standard_z_plt_NA_median
standard_z_plt_NA_mean = standard_z(plt_NA_mean)
standard_z_plt_NA_mean

# plt_0_scale = scale(plt_NA_0[2:19])
# plt_mean_scale = scale(plt_NA_median[2:19])
# plt_median_scale = scale(plt_NA_mean[2:19])


###############  1-e-3 Normalizaton (Min-Max) #################################


# Call the function
normal_min_max_NA_0 = normal_min_max(plt_NA_0)
normal_min_max_NA_0
normal_min_max_NA_median = normal_min_max(plt_NA_median)
normal_min_max_NA_median
normal_min_max_NA_mean = normal_min_max(plt_NA_mean)
normal_min_max_NA_mean



###############  1-f-i #################################


# Q1:  Missing Value Processing 1 : what's your strategies for attribute and instance deletion for missing value in col/row?

# Step 1: t1 = team[!duplicated(team[,2:3]),] Delete the instances which are the same. 
# As col2 and col3 are the ones without missing value and the following columns seems the same when col2 and col3 are the same
unique_plt = plt[!duplicated(plt[2:3]),] 
nrow(unique_plt) # 703 rows

# call function analysis() in 1-a-i
analysis(unique_plt[2:19])[8,]
# As the col_Leaf.weight has 396 NA, account for 56% of the total, so decided to delete it

unique_plt = subset(unique_plt, select = -Leaf.weight)
uni_sum_plt = analysis(unique_plt[2:18])
write.csv(unique_plt,file = 'plt_unique_fi.csv')
write.csv(uni_sum_plt,file = 'plt_uni_sum_fi.csv')


# Q2:  Missing Value Processing 2 : How to handle or replace other missing value ?

# Using 0 to replace is not very good, as the observed value is various. (但是是否能用0来让人容易看出这是个异常值呢？)
# Using Mean & Median mostly is the same according to our dataset, except the column [ CentroidY, Depth, LeafArea ]

# call function of NA replacing in 1-d, replacing NA by 0, mean, median

plt_f_median = NA_by_median(unique_plt)
write.csv(plt_f_median,file = 'plt_fi_median.csv')
plt_f_mean = NA_by_mean(unique_plt)
write.csv(plt_f_mean,file = 'plt_fi_mean.csv')
plt_f_0 = NA_by_0(unique_plt)
write.csv(plt_f_0,file = 'plt_fi_0.csv')



###############  1-f-ii ################################# Do it again !

cor_graph = symnum(cor(plant_f[2:18],use = "complete.obs"))

# From the graph, we can see: 
# Orientation8 & Orientation9 are extremely high-correlated (>=0.95).
# Orientation7 & Orientation8 are high-correlated (>=0.90).
# Orientation6 & Orientation7 are high-correlated (>=0.90).
# Orientation6 & Orientation1 are high-correlated (>=0.90)
# ...
# Only CentroidX,CentroidY,Mass,Width,Depth,Leaf.weight,LeafArea,Leaf.Hue are uncorrelation

plt_uncor = subset(plt[2:19], select = -c(Depth,Orientation0,Orientation1,Orientation3,Orientation4,
                                 Orientation6,Orientation7,Orientation8))
head(plt_uncor)

analysis(plt_uncor)[8,] # Got the Missing value of each column

# CentroidX   CentroidY    Mass       Width       Depth   Leaf.weight    LeafArea  Leaf.Hue
# 0           0              6           6           4         411          13         11

plt_uncor = subset(plt_uncor, select = -c(Leaf.weight))
nrow(plt_uncor) # return 724 (instances)
ncol(plt_uncor)

# delete the NA by na.omit()
plt_uncor_noNA = na.omit(plt_uncor)

# check if NAs have been deleted seccussfully, and output the dataset
nrow(plt_uncor_noNA)  # return 674 (instances)
analysis(plt_uncor_noNA)[8,] # returns all 0

write.csv(plt_uncor_noNA,file = 'plt_uncor_noNA_fii.csv')


###############  1-f-iii #################################
head(plt_f_mean)
ncol(plt_f_mean)
plt_f_iii = plt_f_mean[,2:18]
# 4. Before applying PCA: we must standardize our variables with scale() function:
plt.stand = as.data.frame(scale(plt_f_iii)) # 'standardize' means, (it - average value) / sd
head(plt.stand)
sapply(plt.stand,sd) #now, standard deviations are 1
sapply(plt.stand,mean) #now, mean should be 0 (or very very close to 0)

plt_pca = prcomp(plt.stand,scale=T)
summary(plt_pca)
screeplot(plt_pca, type="lines",col=2, main="Variance explained by PC")
title(xlab="Principal Components")
ggsave('plt_pca.jpg')

plt_pca_7= plt_pca$x[,1:7]

head(plt_pca_7)
write.csv(plt_pca_7,file = 'plt_pca_fiii.csv')








