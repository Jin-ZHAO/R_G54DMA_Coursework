library(ggplot2)
library(dplyr)
library(scatterplot3d)
library(RColorBrewer)
source('1-A-i-summary.R')
source('summary.R')
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
sum_plt_0 = analysis(plt_NA_0[2:19])
write.csv(sum_plt_0,file = 'sum_plt_0.csv')

# Replace NA by mean
plt_NA_mean = NA_by_mean(plt)
write.csv(plt_NA_mean,file = 'plt_NA_mean.csv')
sum_plt_mean = analysis(plt_NA_mean[2:19])
write.csv(sum_plt_mean,file = 'sum_plt_mean.csv')

# Replace NA by mean
plt_NA_median = NA_by_median(plt)
write.csv(plt_NA_median,file = 'plt_NA_median.csv')
sum_plt_median = analysis(plt_NA_median[2:19])
write.csv(sum_plt_median,file = 'sum_plt_median.csv')


###############  1-e Transforming Data from Lec 5 ######################################################

# call the function written in file 'Normal_Standard_Mean.R'


###############  1-e-1 Mean Centring ################################################################

# Call the function
mean_centre_plt_NA_0 = mean_centre(plt_NA_0)
mean_centre_plt_NA_0
mean_centre_plt_NA_median = mean_centre(plt_NA_median)
mean_centre_plt_NA_median
mean_centre_plt_NA_mean = mean_centre(plt_NA_mean)
mean_centre_plt_NA_mean

###############  1-e-2 Standardisation (Z-Score) ################################################################

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


###############  1-e-3 Normalizaton (Min-Max) ################################################################


# Call the function
normal_min_max_NA_0 = normal_min_max(plt_NA_0)
normal_min_max_NA_median = normal_min_max(plt_NA_median)
normal_min_max_NA_median
normal_min_max_NA_mean = normal_min_max(plt_NA_mean)
normal_min_max_NA_mean



###############  1-f-i ################################################################################################




###############  1-f-ii ################################################################################################

uncor_f = subset(plt, select = -Leaf.weight)
uncor_f = subset(uncor_f, select = -Orientation0)
uncor_f = subset(uncor_f, select = -Orientation1)
uncor_f = subset(uncor_f, select = -Orientation5)
uncor_f = subset(uncor_f, select = -Orientation6)

uncor_f = subset(uncor_f, select = -Orientation3)
uncor_f = subset(uncor_f, select = -Orientation8)
uncor_f = subset(uncor_f, select = -Depth)

uncor_f = subset(uncor_f, select = -Orientation4)
uncor_f = subset(uncor_f, select = -Orientation7)

ncol(uncor_f)
head(uncor_f)

gg_cor = ggcorr(uncor_f[2:9], geom = "blank",
                 label = TRUE, label_round = 3, label_alpha = F, 
                 hjust = 0.75, size = 5 ) 

gg_cor = gg_cor + 
  geom_point(size = 10, aes(alpha = abs(coefficient) > 0.5, color ='tomato')) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) 

gg_cor = gg_cor + ggtitle('Figure 28: Uncorrelated Attributes') + 
  theme(plot.title = element_text(size = 40, face = "bold")) 



print(gg_cor)
ggsave('1-f-ggcor-3.jpg')


sum_uncor_f = analysis(uncor_f[2:9])
sum_uncor_f[9,]

# delete the NA by na.omit()
plt_uncor_noNA = na.omit(uncor_f)
plt_uncor_mean = NA_by_mean(uncor_f)
plt_uncor_median = NA_by_median(uncor_f)

# check if NAs have been deleted seccussfully, and output the dataset
nrow(plt_uncor_noNA)  # return 679 (instances)
analysis(plt_uncor_noNA[2:9])[9,] # returns all 0

write.csv(plt_uncor_noNA,file = 'plt_uncor_omit.csv')
write.csv(plt_uncor_median,file = 'plt_uncor_median.csv')
write.csv(plt_uncor_mean,file = 'plt_uncor_mean.csv')


###############  1-f-iii ###############################################################################################

# for Deleting Duplicated Instances 
# As col2 and col3 are the ones without missing value and the following columns seems the same when col2 and col3 are the same
unique_plt = plt[!duplicated(plt[,2:3]),]
nrow(unique_plt) # 703 rows

unique_plt = subset(unique_plt, select = -Leaf.weight)

plt_f_mean = NA_by_mean(unique_plt)

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

# 1. Open jpeg file
jpeg("plt_pca.jpg", width = 800, height = 600)
# 2. Create the plot
screeplot(plt_pca, type="lines",col=2, main="Variance explained by PC")
title(xlab="Principal Components")

# 3. Close the file
dev.off()


plt_pca_7= plt_pca$x[,1:7]

head(plt_pca_7)
write.csv(plt_pca_7,file = 'plt_pca_fiii.csv')




