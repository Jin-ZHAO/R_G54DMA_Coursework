# Used in 1-d #

################ Replace NA by O  ###############
NA_by_0 = function(plt){
  plt_NA_0 = plt
  plt_NA_0[is.na(plt_NA_0)] = 0
  return(plt_NA_0)
}

################ Replace NA by mean  ###############
NA_by_mean = function(plt){
  plt_NA_mean = plt
  for (i in 2:(ncol(plt)-1)){
    plt_NA_mean[,i][is.na(plt_NA_mean[,i])] = mean(plt_NA_mean[,i],na.rm = TRUE)
  }
  return(plt_NA_mean)
}


############### Replace NA by median ###############
NA_by_median = function(plt){
  plt_NA_median = plt
  for (i in 2:(ncol(plt)-1)){
    plt_NA_median[,i][is.na(plt_NA_median[,i])] = median(plt_NA_median[,i],na.rm = TRUE)
  }
  return(plt_NA_median)
}

