###############  1-e-1 Mean Centring #################################

# 1. Mean centring : subtract mean to move the mean value to zero  
# y = (y - mean)

mean_centre = function(plt){
  mean_centre = plt
  # step 2: substract the mean 
  for(i in 2:19){
    for (m in 1:nrow(plt)){
      mean_centre[m,i] = plt[m,i] - mean(plt[,i])
    }
  }
  return(mean_centre)
}


###############  1-e-2 Standardisation (Z-Score) #################################

# 2. Standardisation (Z-score normalization)
# y = (y - mean) / sd

standard_z = function(plt){
  standard_z = plt
  # step 2: substract the mean 
  for(i in 2:19){
    for (m in 1:nrow(plt)){
      # this 'if' part is for: when the value is equal to mean, the return will be 0 rather than NaN
      if (plt[m,i] - mean(plt[,i]) == 0 ){standard_z[m,i] = 0
      }else{
        standard_z[m,i] = (plt[m,i] - mean(plt[,i]))/sd(plt[,i])
      }
    }
  }
  return(standard_z)
}



###############  1-e-3 Normalizaton (Min-Max) #################################


# 3. Normalizaton (Min-Max normailization)
# y = (y - min) / (max - min)

normal_min_max = function(plt){
  normal_min_max = plt
  # step 2: substract the mean 
  for(i in 2:19){
    for (m in 1:nrow(plt)){
      # this 'if' part is for: when the value is equal to mean, the return will be 0 rather than NaN
      if (plt[m,i] - min(plt[,i]) == 0 ){normal_min_max[m,i] = 0
      }else{
        normal_min_max[m,i] = (plt[m,i] - min(plt[,i])) / (max(plt[,i])-min(plt[,i]))
      }
    }
  }
  return(normal_min_max)
}