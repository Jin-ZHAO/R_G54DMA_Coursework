
install.packages('GGally')

plt <- read.table(file = "g54dma-plant-dataset.csv",header = TRUE,sep = ',')
nrow(plt) #724
ncol(plt) #20

##############################  1-b-i ###############################################################


# use is "complete.obs" then missing values are handled by casewise deletion.
cor_o1_o7 = cor(plt$Orientation1, plt$Orientation7,use = "complete.obs")
cor_mass_o0 = cor(plt$Mass, plt$Orientation0,use = "complete.obs")
cor_o7_o8 = cor(plt$Orientation7, plt$Orientation8,use = "complete.obs")
sprintf('correlation_o1_o7 is %f, correlation_mass_o0 is %f, and correlation_o7_o8 is %f.',cor_o1_o7,cor_mass_o0,cor_o7_o8)
# Output: "correlation_o1_o7 is -0.869672, correlation_mass_o0 is -0.087690, and correlation_o7_o8 is 0.929113."


###############################  1-b-ii & iii #########################################################

library(GGally)


########### Step 1: Get the sub dataset and rename the column: ###################################

subplt = cbind(plt[2:16],plt[18:19])
names(subplt) = c('Cx','Cy','Ms','Wd','Dh','Or0','Or1','Or2','Or3','Or4','Or5','Or6','Or7','Or8','Or9','Lf_A','Lf_H')



########### Step 2-1: Visualise the correlation between variables  ###################################
gg_cor1 = ggcorr(subplt, nbreaks = 8, 
                 label = T, label_round = 5,label_alpha = F, 
                 hjust = 0.5, size = 8)  + 
  ggtitle('Correlations of Variables') + 
  theme(plot.title = element_text(size = 40, face = "bold"))

print(gg_cor1)
ggsave('1-b-ggcor1.jpg')



########### Step 2-2: Find the least & most correlated ones ###################################

gg_cor2 = ggcorr(subplt, geom = "blank",
       label = TRUE, label_round = 5, label_alpha = F, 
       hjust = 0.5, size = 8 ) 

gg_cor3 = gg_cor2 + 
  geom_point(size = 20, aes(alpha = abs(coefficient) > 0.92, color ='tomato')) +
  geom_point(size = 20, aes(alpha = abs(coefficient) < 0.01)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) 

gg_cor4 = gg_cor3 + ggtitle('Correlations of Variables') + 
  theme(plot.title = element_text(size = 40, face = "bold")) 
  

print(gg_cor4)
ggsave('1-b-ggcor4.jpg')


########### Reference ###################################

# 1. GGally package: Extension tp ggplot2 for [ correlation matrix and survival plots ]
# http://www.sthda.com/english/wiki/ggally-r-package-extension-to-ggplot2-for-correlation-matrix-and-survival-plots-r-software-and-data-visualization

# 2. ggcor()  # Good one !!!!
# https://briatte.github.io/ggcorr/

