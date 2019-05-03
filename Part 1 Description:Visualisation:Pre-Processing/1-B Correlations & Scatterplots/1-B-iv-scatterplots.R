install.packages('scatterplot3d')
install.packages('RColorBrewer')
install.packages('ggplot2')
install.packages('rgl')


###############  1-b-iv   ########################################################################################################################

# Produce scatterplots between the class variable and orientation 2, depth and area variables 
# (note: you may have to recode the class variable as numeric to produce scatterplots). 
# What do these tell you about the relationships between these three variables and the class?



############ Step 0: load the dataset #######################################################

plt <- read.table(file = "g54dma-plant-dataset.csv",header = TRUE,sep = ',')


############ Step 1: Use different colors  ################################################

library(RColorBrewer)

plotvar = plt$Class  # pick a variable to plot for color
nclr = 5  # number of colors
# plotclr = brewer.pal(nclr,"PuBu") # get the colors
plotclr = c("#FA8072","#FFD700","#3CB371","#8A2BE2","#045A8D") 
# the code of color: red, yellow, seagreen, blueviolet , darkblue

colornum = cut(rank(plotvar), nclr, labels=FALSE)
colcode = plotclr[colornum] # assign color



# Alternative for Step 1 : 

# colors <- c("#F1EEF6","#BDC9E1","#74A9CF","#2B8CBE","#045A8D")
# colors <- colors[as.numeric(plt$Class)]
# scatterplot3d( plt$Orientation2, plt$LeafArea, plt$Depth, pch = 16, color=colors)


############ Step 2: Several ways  #######################################################



#############################################  ggplot2 ################################################

library(ggplot2)

gg_class_Ori2 =
  ggplot(plt, aes(x = Class, y = Orientation2, color = Class)) +
  geom_point(size = 1, stroke = 1.5) + 
  ggtitle("Class and Orientation2")

ggplot(subset(dat, Team %in% c("Bism", "Dragon Island", "Calormen")), aes(x=Speed, y=Height, color=Gender)) + geom_point(size = 1, stroke = 1.5) + geom_smooth(method = "lm", se=TRUE) + ggtitle("Height vs Speed according to Gender") 



gg_class_Depth = ggplot(plt, aes(x = Class, y = Depth,color = Class)) +
  geom_point(size = 1, stroke = 1.5) +
  ggtitle("Class and Depth")

gg_class_Area = ggplot(plt, aes(x = Class, y = LeafArea, color = Class)) + 
  geom_point(size = 1, stroke = 1.5) + 
  ggtitle("Class and Area")


print(gg_class_Ori2)
ggsave('1-b-iv-gg_class_Ori2.jpg')

print(gg_class_Depth)
ggsave('1-b-iv-gg_class_Depth.jpg')

print(gg_class_Area)
ggsave('1-b-iv-gg_class_Area.jpg')



#############################################  scatterplot matrixs ################################################

#  Basic scatterplot matrix of the four measurements
# E.g - Basic R : pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)
# Similar plot using ggplot2
# plotmatrix(with(iris, data.frame(Sepal.Length, Sepal.Width, Petal.Length)))
# plotmatrix didn't exist any more!
# dev.off()

Orientation2 = plt$Orientation2
Depth = plt$Depth
LeafArea = plt$LeafArea
sub_plt = cbind(Orientation2, Depth, LeafArea)

pairs(sub_plt, col=colcode)


dev.off()

??pairs


#############################################   3D scatterplots by scatterplot3d ################################################

# seems this library works betther than rgl, I personally think.

library(scatterplot3d)

x = plt$Orientation2
y = plt$LeafArea
z = plt$Depth


# 1. Open jpeg file
jpeg("1-b-iv-3Dscatterplot-classes.jpg", width = 800, height = 600)
jpeg("1-b-iv-3Dscatterplot-classes2.jpg", width = 800, height = 600)

# scatter plot
s3d = scatterplot3d(x, y, z, angle= 45, type='h',color=colcode, pch = 1, 
                    grid=TRUE, box=FALSE, 
                    xlab = 'Orientation2', ylab = 'LeafArea', zlab = 'Depth',
                    main = "Relationships: Class - Orientation2 / Depth / LeafArea")

# when [ type = 'h' ] , there will have a line to the botton for each points.

# add legend for each colors / classes. the xyz.convert() indicates the location.
legend(s3d$xyz.convert(0.17,6000,800), legend = levels(plt$Class),
       col =  plotclr, pch = 1, xpd = TRUE, horiz = TRUE)
# }

# Don't Run this part 
# Add legend in another way
legend("right", legend = levels(plt$Class),
       col = plotclr, pch = 1, inset = 0.1,xpd = TRUE, horiz = TRUE)


# 3. Close the file
dev.off()


### DON'T RUN THIS PART #########################   3D scatterplots by rgl ################################################

library(rgl)

x = plt$Orientation2
y = plt$LeafArea
z = plt$Depth

plot3d(x, y, z, type = 's', size = 0.75, lit = F)

