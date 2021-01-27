mydistfun<- function(element1, element2, metricf, p=1){
  # this function returns the distance between the element1 and element2
  # according to the metricf
  
  dimensions=length(element1)
  sqd<-matrix(element1,dimensions,1)
  #print(dimensions)
  #print(element1)
  
  
  if (toLower(metricf)=="minkowski"){
    #MINOWSKI
    #for p<1 Minowski distance is not metric
    #if p==1 then the Minowski distance corresponds to the Manhattan distance
    #if p==2 then the Minowski distance corresponds to the Euclidian distance
    #if p->inf then the Minowski distance corresponds to the Chebyshev distance
    dist<-(sum((abs(element1-element2))^p))^(1/p)
  }
  
  if (toLower(metricf)=="manhattan"){
    mydistfun(element1, element2, "minkowski", p=1)
  }
  
  if (toLower(metricf)=="euclidian"){
    mydistfun(element1, element2, "minkowski", p=2)
  }
  
  if (toLower(metricf)=="canberra"){
    #CANBERRA
    #weighted version of Manhattan distance
    dist<-sum(abs(element1 - element2) / abs(element1) + abs(element2))
  }
  
  if (toLower(metricf)=="mahalanobis"){
    #MAHALANOBIS
    median_elem1 <- colMedians(element1)
    median_elem2 <- colMedians(element2)
    
    n_elem1 <- nrow(element1)
    n_elem2 <- nrow(element2)
    
    var_elem1 <- (cov(element1) * pi) / (2 * n_elem1)
    var_elem2 <- (cov(element2) * pi) / (2 * n_elem2)
    
    total_var <- ((n_elem1 - 1) * var_elem1 + 
                    (n_elem2 - 1) * var_elem2) / (n_elem1 + n_elem2 - 2)
    
    dist<-as.numeric(t(median_elem1 - median_elem2) %*% solve(total_var) %*% median_elem1 - median_elem2)
  }
  
  return(dist)
}
  