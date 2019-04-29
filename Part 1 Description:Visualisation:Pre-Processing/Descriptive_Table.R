descriptive = function(plt){
  
  p_means = colMeans(plt[,1:(ncol(plt))],(ncol(plt)))
  p_medians = c(median(plt[,1],na.rm = TRUE))
  p_mins = c(min(plt[,1],na.rm = TRUE))
  p_maxs = c(max(plt[,1],na.rm = TRUE))
  p_vars = c(var(plt[,1],na.rm = TRUE))
  p_sds = c(sd(plt[,1],na.rm = TRUE))
  p_IQRs = c(IQR(plt[,1],na.rm = TRUE))
  p_miss = sum(is.na(plt[,1]))
  
  for (i in 2:(ncol(plt))){
    p_medians = cbind(p_medians,median(plt[,i],na.rm = TRUE))
    p_mins = cbind(p_mins,min(plt[,i],na.rm = TRUE))
    p_maxs = cbind(p_maxs,max(plt[,i],na.rm = TRUE))
    p_vars = cbind(p_vars,var(plt[,i],na.rm = TRUE))
    p_sds = cbind(p_sds,sd(plt[,i],na.rm = TRUE))
    p_IQRs = cbind(p_IQRs,IQR(plt[,i],na.rm = TRUE))
    p_miss= cbind(p_miss,sum(is.na(plt[,i])))
  }
  
  it = as.table(rbind(p_means,p_medians,p_mins,p_maxs,p_vars,p_sds,p_IQRs,p_miss))
  row.names(it) = c('mean','median','min','max','var','sd','IQR','n_NA')
  write.csv(it,file = 'Descriptive_Table.csv')
  return(it)
}
