
plt <- read.table(file = "g54dma-plant-dataset.csv",header = TRUE,sep = ',')

install.packages("e1071") 
library(e1071)

shape = function(plt){
  it = data.frame(type = c('kurtosis'))
  for (i in 2:19){
    it = rbind(it,c(kurtosis(plt[,i],na.rm = T)))}
  return(it)
  }

shape(plt)



kurtosis(plt[,2])
kurtosis(plt[,3])
kurtosis(plt[,4],na.rm = T)
skewness(plt[,4],)
