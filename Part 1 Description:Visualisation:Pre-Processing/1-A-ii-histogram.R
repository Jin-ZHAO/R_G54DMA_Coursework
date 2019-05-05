library(ggplot2)
plt <- read.table(file = "g54dma-plant-dataset.csv",header = TRUE,sep = ',')
sum_plt <- read.table(file = "Descriptive_Table.csv",header = TRUE,sep = ',')


nrow(plt) # 724
ncol(sum_plt) # 19
nrow
sum_plt[,1:2]


ggplot(plt,aes(Orientation9)) + geom_histogram(binwidth=0.003)

#######################################################################################################################

Width_decision = function(sum_plt){
  # Methods 1 : calculate the optimal width according to Scott,1979
  # sum_plt[9,] is for N_NA
  # sum_plt[8,] is for sd
  sum_plt[10,2] = c( 3.49*sum_plt[8,2]*(724 - sum_plt[9,2] )^(-1/3) )
  
  # Methods 2 : calculate the optimal width according to Frredman and Diaconis (Izenman,1991)
  # sum_plt[6,] is IQR of data in row 5
  sum_plt[11,2] = c( 2*sum_plt[6,2]*(724 - sum_plt[9,2] )^(-1/3) )
  
  # Methods 3 : calculate the optimal width according to Scott,1992 
  # sum_plt[5,] is Range of data 
  sum_plt[12,2] = c( sum_plt[5,2] / (1 + log2(724 - sum_plt[9,2])))

  for (i in 3:19){
    # sd in row8, n_na in row9
  sum_plt[10,i] = c( 3.49*sum_plt[8,i]*(724 - sum_plt[9,i] )^(-1/3))
  sum_plt[11,i] = c( 2*sum_plt[6,i]*(724 - sum_plt[9,i] )^(-1/3))
  sum_plt[12,i] = c( sum_plt[5,i] / (1 + log2(724 - sum_plt[9,i])))
  }

  return(sum_plt[10:12,2:19])
}


Width_decision(sum_plt)  # Got the suggested width for each attributes


# https://www.fmrib.ox.ac.uk/datasets/techrep/tr00mj2/tr00mj2/node24.html


################################################################################################################################################# ############################################################ ############################################################ 
########## CentroidX ############################################################ 

# 1. Open jpeg file
jpeg("Hist_CentroidX.jpg", width = 800, height = 600)
# 2. Create the plot


hist(plt$CentroidX, prob = T, breaks = 15, col = 8, xlab = "CentroidX", main = "Hist of CentroidX",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$CentroidX,na.rm = T))
abline(v = mean(plt$CentroidX,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$CentroidX,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$CentroidX,na.rm = T), sd=sd(plt$CentroidX,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")

legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))




# 3. Close the file
dev.off()



########## CentroidY ############################################################ 

# 1. Open jpeg file
jpeg("Hist_CentroidY.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$CentroidY, prob = T, breaks = 15, col = 8, xlab = "CentroidY", main = "Hist of CentroidY",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$CentroidY,na.rm = T))
abline(v = mean(plt$CentroidY,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$CentroidY,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$CentroidY,na.rm = T), sd=sd(plt$CentroidY,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topleft", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()


########## Mass ############################################################ 



# 1. Open jpeg file
jpeg("Hist_Mass.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Mass, prob = T, breaks = 15, col = 8, xlab = "Mass", main = "Hist of Mass",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Mass,na.rm = T))
abline(v = mean(plt$Mass,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Mass,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Mass,na.rm = T), sd=sd(plt$Mass,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()


########## Width ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Width.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Width, prob = T, breaks = 15, col = 8, xlab = "Width", main = "Hist of Width",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Width,na.rm = T))
abline(v = mean(plt$Width,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Width,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Width,na.rm = T), sd=sd(plt$Width,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()


########## Depth ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Depth.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Depth, prob = T, breaks = 15, col = 8, xlab = "Depth", main = "Hist of Depth",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Depth,na.rm = T))
abline(v = mean(plt$Depth,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Depth,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Depth,na.rm = T), sd=sd(plt$Depth,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()


########## Orientation0 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation0.jpg", width = 800, height = 600)
# 2. Create the plot
ggplot(plt,aes(Orientation0)) + geom_histogram(binwidth=0.01)

hist(plt$Orientation0, prob = T, breaks = 15, col = 8, 
     xlab = "Orientation0",main = "Hist of Orientation0",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation0, na.rm = T))
abline(v = mean(plt$Orientation0, na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation0, na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation0, na.rm = T), sd=sd(plt$Orientation0, na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))


# 3. Close the file
dev.off()



########## Orientation1 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation1.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation1, prob = T, breaks = 15, col = 8, xlab = "Orientation1", main = "Hist of Orientation1",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation1,na.rm = T))
abline(v = mean(plt$Orientation1,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation1,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation1,na.rm = T), sd=sd(plt$Orientation1,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()



########## Orientation2 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation2.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation2, prob = T,  breaks = 15, col = 8, xlab = "Orientation2", main = "Hist of Orientation2",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation2,na.rm = T))
abline(v = mean(plt$Orientation2,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation2,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation2,na.rm = T), sd=sd(plt$Orientation2,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()


########## Orientation3 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation3.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation3, prob = T, breaks = 15, col = 8, xlab = "Orientation3", main = "Hist of Orientation3",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation3,na.rm = T))
abline(v = mean(plt$Orientation3,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation3,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation3,na.rm = T), sd=sd(plt$Orientation3,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))


# 3. Close the file
dev.off()


########## Orientation4 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation4.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation4, prob = T, breaks = 15, col = 8, xlab = "Orientation4", main = "Hist of Orientation4",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation4,na.rm = T))
abline(v = mean(plt$Orientation4,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation4,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation4,na.rm = T), sd=sd(plt$Orientation4,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))


# 3. Close the file
dev.off()

########## Orientation5 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation5_15.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation5, prob = T, breaks = 15, col = 8, xlab = "Orientation5", main = "Hist of Orientation5",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation5,na.rm = T))
abline(v = mean(plt$Orientation5,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation5,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation5,na.rm = T), sd=sd(plt$Orientation5,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))


# 3. Close the file
dev.off()

########## Orientation6 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation6.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation6, prob = T, breaks = 15, col = 8, xlab = "Orientation6", main = "Hist of Orientation6",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation6,na.rm = T))
abline(v = mean(plt$Orientation6,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation6,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation6,na.rm = T), sd=sd(plt$Orientation6,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()

########## Orientation7 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation7.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation7, prob = T, breaks = 15, col = 8, xlab = "Orientation7", main = "Hist of Orientation7",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation7,na.rm = T))
abline(v = mean(plt$Orientation7,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation7,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation7,na.rm = T), sd=sd(plt$Orientation7,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()

########## Orientation8 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation8.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation8, prob = T, breaks = 15, col = 8, xlab = "Orientation8", main = "Hist of Orientation8",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation8,na.rm = T))
abline(v = mean(plt$Orientation8,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation8,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation8,na.rm = T), sd=sd(plt$Orientation8,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()

########## Orientation9 ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Orientation9.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Orientation9, prob = T, breaks = 15, col = 8, xlab = "Orientation9", main = "Hist of Orientation9",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Orientation9,na.rm = T))
abline(v = mean(plt$Orientation9,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Orientation9,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Orientation9,na.rm = T), sd=sd(plt$Orientation9,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()



########## Leaf.weight ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Leaf.weight.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Leaf.weight, prob = T, breaks = 15, col = 8, xlab = "Leaf.weight", main = "Hist of Leaf.weight",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Leaf.weight,na.rm = T))
abline(v = mean(plt$Leaf.weight,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Leaf.weight,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Leaf.weight,na.rm = T), sd=sd(plt$Leaf.weight,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()



########## LeafArea ############################################################ 

# 1. Open jpeg file
jpeg("Hist_LeafArea.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$LeafArea, prob = T, breaks = 15, col = 8, xlab = "LeafArea", main = "Hist of LeafArea",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$LeafArea,na.rm = T))
abline(v = mean(plt$LeafArea,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$LeafArea,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$LeafArea,na.rm = T), sd=sd(plt$LeafArea,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()


########## Leaf.Hue ############################################################ 

# 1. Open jpeg file
jpeg("Hist_Leaf.Hue.jpg", width = 800, height = 600)
# 2. Create the plot
hist(plt$Leaf.Hue, prob = T, breaks = 15, col = 8, xlab = "Leaf.Hue", main = "Hist of Leaf.Hue",
     cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  
lines(density(plt$Leaf.Hue,na.rm = T))
abline(v = mean(plt$Leaf.Hue,na.rm = T), col = "blue", lwd = 2)
abline(v = median(plt$Leaf.Hue,na.rm = T), col = "red", lwd = 2)
curve(dnorm(x, mean(plt$Leaf.Hue,na.rm = T), sd=sd(plt$Leaf.Hue,na.rm = T)), 
      col='green', lwd=2, add=TRUE, yaxt="n")


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Standard Normal plot","Mean", "Median"),
       col = c("black", "green", "blue",'red'),
       lwd = c(2, 2, 2))

# 3. Close the file
dev.off()


