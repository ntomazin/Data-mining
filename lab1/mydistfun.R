mydistfun<- function(element1, element2, metricf, cov=NULL, p=1){
  # this function returns the distance between the element1 and element2
  # according to the metricf
  
  if (tolower(metricf)=="minkowski"){
    #MINOWSKI
    #for p<1 Minowski distance is not metric
    #if p==1 then the Minowski distance corresponds to the Manhattan distance
    #if p==2 then the Minowski distance corresponds to the Euclidian distance
    #if p->inf then the Minowski distance corresponds to the Chebyshev distance
    dist<-(sum((abs(element1-element2))^p))^(1/p)
  }
  
  if (tolower(metricf)=="manhattan"){
    mydistfun(element1, element2, "minkowski", p=1)
  }
  
  if (tolower(metricf)=="euclidian"){
    mydistfun(element1, element2, "minkowski", p=2)
  }
  
  if (tolower(metricf)=="canberra"){
    #CANBERRA
    #weighted version of Manhattan distance
    print(abs(element1 - element2) / (abs(element1) + abs(element2)))
    dist<-sum(abs(element1 - element2) / (abs(element1) + abs(element2)))
  }

  if (tolower(metricf)=="mahalanobis"){
    #MAHALANOBIS
    dist<-sqrt(t(element1 - element2) %*% solve(cov) %*% (element1 - element2))
  }
  
  return(dist)
}


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
  
  for(i in seq(along=R)){
    help_list_R = vector(mode = "list", length = length(dataset))
    help_list_S = vector(mode = "list", length = length(dataset))
    for(j in seq(along=dataset)){
      help_list_R[j] = mydistfun(R[i], dataset[j], "Minkowski", p=2)
      help_list_S[j] = mydistfun(S[i], dataset[j], "Minkowski", p=2)
    }
    alfa[i] = min(help_list_R)
    beta[i] = min(help_list_S)
  }
  
  for(i in seq(along=num_of_points)){
    H = sum(beta) / sum(alfa + beta)
  }
  return(H)
}
  