enthropy<- function(points_in_region){
  enthropy_sum = 0
  for(i in seq(along=points_in_region)){
    ent<-points_in_region[i]*log(points_in_region[i]) + 
      (1- points_in_region[i])*log(1 - points_in_region[i])
    enthropy_sum = enthropy_sum - ent
  } 
  return(enthropy_sum)
}

hopkins<- function(dataset, num_of_points){
  #Higher values of H indicate highly clustered data.
  
  R = sample(dataset, num_of_points)
  S = runif(num_of_points, min(dataset), max(dataset))
  
  alfa = vector(mode = "list", length = num_of_points)
  beta = vector(mode = "list", length = num_of_points)
  
  for(i in seq(along=num_of_points)){
    alfa[i] = 
    
  }
  
}

