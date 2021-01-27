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
  
  if (tolower(metricf)=="euclidean"){
    mydistfun(element1, element2, "minkowski", p=2)
  }
  
  if (tolower(metricf)=="chebyshev"){
    mydistfun(element1, element2, "minkowski", p=Inf)
  }
  
  if (tolower(metricf)=="canberra"){
    #CANBERRA
    #weighted version of Manhattan distance
    dist<-sum(abs(element1 - element2) / (abs(element1) + abs(element2)))
  }

  if (tolower(metricf)=="mahalanobis"){
    #MAHALANOBIS
    dist<-sqrt(t(element1 - element2) %*% solve(cov) %*% (element1 - element2))
  }
  
  return(dist)
}


entropy<- function(dataset, num_of_regions){
  #Large values of entropy_sum indicate poor clustering behaviour
  #only getting regions through the first dimension
  region_size = (abs(max(dataset[,1]))+abs(min(dataset[,1])))/num_of_regions
  proportion_of_points = numeric(length = num_of_regions)
  for(i in 1:num_of_regions){
    for(j in seq(along=dataset[,1])){
      if(dataset[j,1] > region_size*(i-1-num_of_regions/2) && dataset[j,1] < region_size*(i-num_of_regions/2)){
        proportion_of_points[i] = proportion_of_points[i] + 1
      }
    }
  }
  
  proportion_of_points = proportion_of_points/length(dataset[,1])

  entropy_sum = 0
  for(i in 1:num_of_regions){
    if(proportion_of_points[i] != 0){
      ent<-proportion_of_points[i]*log(proportion_of_points[i]) + 
        (1- proportion_of_points[i])*log(1 - proportion_of_points[i])
      entropy_sum = entropy_sum - ent
    }
  } 
  return(entropy_sum)
}

hopkins<- function(dataset, num_of_points){
  #Higher values of H indicate highly clustered data.
  
  R = dataset[sample(1:length(dataset[,1]),num_of_points),]
  S = cbind(runif(num_of_points, min(dataset[,1]), max(dataset[,1])),
            runif(num_of_points, min(dataset[,2]), max(dataset[,2])))
  
  alfa = vector(mode = "list", length = num_of_points)
  beta = vector(mode = "list", length = num_of_points)
  
  for(i in seq(along=R[,1])){
    help_list_R = vector(mode = "list", length = length(dataset[,1]))
    help_list_S = vector(mode = "list", length = length(dataset[,1]))
    for(j in seq(along=dataset[,1])){
      help_list_R[j] = mydistfun(R[i,], dataset[j,], "Minkowski", p=2)
      help_list_S[j] = mydistfun(S[i,], dataset[j,], "Minkowski", p=2)
    }
    alfa[i] = min(unlist(help_list_R[help_list_R != 0]))
    beta[i] = min(unlist(help_list_S))
  }
  
  H=0
  H=sum(unlist(beta)) / sum(unlist(alfa) + unlist(beta))
  return(H)
}


k_medoid<-function(dataset, k, metricf="minkowski", cov=NULL, p=2, 
                   dissimilarity_matrix=NULL, iterations){
  #https://www.geeksforgeeks.org/ml-k-medoids-clustering-with-example/
  medoids = dataset[sample(1:length(dataset[,1]),k),]

  cluster_affilation = matrix(, nrow = length(dataset[,1]), ncol=1)
  total_cost = matrix(, nrow = length(dataset[,1]), ncol=1)
  dissimilarity_matrix = matrix(, nrow = length(dataset[,1]), ncol = k)
  dissimilarity_matrix_full = matrix(, nrow = length(dataset[,1]), ncol = length(dataset[,1]))
  
  last_cost = Inf
  best_medoid = medoids
  iteration_counter = 0
  
  #calculating the dissimilarity matrix, where dissimilarity is interpreted
  #as the distance from 2 points
  #smaller distance, smaller dissimilarity
  for(j in 1:length(dataset[,1])){
    for(i in 1:length(dataset[,1])){ 
      dissimilarity_matrix_full[j, i] = mydistfun(t(dataset[i,]), t(dataset[j,]), 
                                             metricf=metricf, cov=cov, p=p)
    }
  }
  
  while(TRUE){
    #getting only columns with the mediods
    dissimilarity_matrix = dissimilarity_matrix_full[,as.numeric(rownames(medoids))]
    
    #calculating cost with the current mediods
    #cost is calculated as the sum of all distances
    total_cost = apply(dissimilarity_matrix, 1, min)
    
    #calculating to which cluster does a point belong
    for(j in 1:length(dataset[,1])){
      cluster_affilation[j] = which(dissimilarity_matrix[j,] == min(min(dissimilarity_matrix[j,])))
    }
    current_dataset = cbind(dataset, cluster_affilation)
    
    #updating the cost and best_medoids if cost is lower than the last cost
    if(sum(total_cost) < last_cost){
      last_cost = sum(total_cost)
      iteration_counter = 0
      best_medoid = medoids
      #randomly chosen medoid
      medoids = dataset[sample(1:length(dataset[,1]),k),]
      next
    }
    
    iteration_counter = iteration_counter+1
    
    #stopping condition, if "iterations" iterations didnt find a better medoid
    if(iteration_counter >= iterations){
      return(list("data" = current_dataset, "best_medoids" = best_medoid,
                  "dissimilarity_matrix"=dissimilarity_matrix_full))
    }
    medoids = dataset[sample(1:length(dataset[,1]),k),]
    
    
  }
}
  
     
    


