###################################################
###### TASK 1 #####################################
###################################################

FisherScore<- function(X, y){
  #The Fisher score is naturally designed for numeric attributes to measure the
  #ratio of the average interclass separation to the average intraclass separation.
  #The larger the Fisher score, the greater the discriminatory power of the
  #attribute.
  X_mean = mean(as.matrix(X));
  F_numerator = 0;
  F_denumerator = 0;
  
  classes = unique(as.matrix(y));
  
  for(i in classes){
    f_j = X[y==i,];
    p_j = nrow(f_j)/nrow(X);
    u_j = mean(as.matrix(f_j));
    F_numerator = F_numerator + p_j * (u_j - X_mean)^2;
    
    F_denumerator = F_denumerator + p_j * sd(as.matrix(f_j))^2;
  }
  return(F_numerator/F_denumerator);
}

gini.index<- function(X, y, feature_name){
  #Measures the discriminative power of a particular feature.
  #Typically, it is used for categorical variables, but it can be generalized
  #to numeric attributes by the process of discretization.
  
  #value-specific Gini index
  classes = unique(as.matrix(y));
  G.sum = 0;
  for(feature_value in as.matrix(unique(X[feature_name]))){
    G = 0;
    for(j in classes){
      f_j = X[y==j & X[feature_name]==feature_value,];
      G = G + (nrow(f_j)/(nrow(X[X[feature_name]==feature_value,])))^2;
    }
    G = 1 - G; 
    G.sum = G.sum + nrow(X[X[feature_name]==feature_value,]) / nrow(X) * G;
  }
  return(G.sum)
}



###################################################
###### TASK 2 #####################################
###################################################

decision.tree.train <- function(X, y, root){
  #if all decisions are the same
  if(length(unique(as.matrix(y)))==1){
    child = root$AddChild(unique(as.matrix(y)));
    SetEdgeStyle(child, label = root$variable);
  }
  else{
    min.gini = 1;
    feature = 0;
    #calculating the minimum gini index values
    for(i in 1:ncol(X)){
      gini = gini.index(X,y, names(X)[i]);
      if(gini < min.gini){
        min.gini = gini;
        feature = i;
      }
    }
  
    if(is.null(root)){
      child = Node$new(names(X)[feature]);
    }
    else{
      child = root$AddChild(names(X)[feature]);
    }
    for(i in unique(as.matrix(X[feature]))){
      child$variable = i;
      SetEdgeStyle(child, label = root$variable);
      decision = as.data.frame(y[rownames(X[X[feature] == i,]),]);
      rownames(decision) = rownames(X[X[feature] == i,])
      decision.tree.train(X[X[feature] == i,-feature],
                          decision,
                          child)
    }
  }
  if(is.null(root)){
    plot(child)
    return(child)
  }
}


###################################################
###### TASK 3 #####################################
###################################################

TimeSeries<- function(D, period){
  D <- data.frame(
    time = c(1:length(D)),
    data = D
  )
  
  plot(D$time, D$data, type="l", main="Initial graph", lty=1);
  
  D$basline = ma(D$data, period);
  #TODO napraviti CMA
  lines(D$time, D$basline, col="red",lty=1)  
  D$ST_IT = D$data/D$basline;
  D$ST = calculate_st(D$ST_IT, period);
  D$deseasonalized = D$data/D$ST;
  
  linearMod <- lm(D$deseasonalized ~ D$time, data=D);
  #summary(linearMod)
  D$trendline = linearMod$coefficients[1] + linearMod$coefficients[2] * D$time;
  lines(D$time, D$trendline, col="blue",lty=1)  
  
  D$forecast = D$ST * D$trendline;
  lines(D$time, D$forecast, col="yellow",lty=1) 
  
  legend("topleft", 
         legend = c("Initial data", "Baseline", "Trendline", "Forecast"),
         col = c("black", "red", "blue", "yellow"), lty = 1, lwd = 1, ncol = 2,
         cex = 0.5)
  
  return(list("D" = D, "model" = linearMod))
}

make_forecast <- function(linearMod, time_years, seasonality){
  time_in_quartals = c(1:(time_years*4));
  st = rep(c(seasonality), length(time_in_quartals)/4);
  trendline = linearMod$coefficients[1] + linearMod$coefficients[2] * time_in_quartals;
  forecast = st*trendline;
  plot(time_in_quartals, forecast,"l", main="Forecast");
}

ma <- function(D, n=4){
  #TODO POPRAVITI FJU
  res = D;
  for(i in n:(length(D))){
    res[i-n/2] = mean(D[(i-n):i])
  }
  res[n/2-1] = NA;
  res[length(D)] = NA;
  
  return(res);
}

calculate_st <- function(data, period){
  res = vector("list", period);
  for(i in 1:period){
    res[i] = mean(data[seq(i, length(data), period)], na.rm=TRUE);
  }
  return(rep(c(unlist(res)), length(data)/period));
}

get_p_value<- function(linearMod){
  modelSummary <- summary(linearMod)  # capture model summary as an object
  modelCoeffs <- modelSummary$coefficients  # model coefficients
  beta.estimate <- modelCoeffs["time", "Estimate"]  # get beta estimate for time
  std.error <- modelCoeffs["time", "Std. Error"]  # get std.error for time
  t_value <- beta.estimate/std.error  # calc t statistic
  p_value <- 2*pt(-abs(t_value), df=nrow(D)-ncol(D))  # calc p Value
  return(p_value)
}

get.daily.cases<- function(data){
  for(i in 1:(nrow(data)-1)){
    data$daily.cases[i] = data$SlucajeviHrvatska[i]-data$SlucajeviHrvatska[i+1];
  }
  data$daily.cases[nrow(data)] = data$SlucajeviHrvatska[nrow(data)];
  return(data$daily.cases)
}

TimeSeriesPoly<- function(D, period){
  D <- data.frame(
    time = c(1:length(D)),
    data = D
  )
  
  plot(D$time, D$data, type="l", main="Initial graph", lty=1);
  
  D$basline = ma(D$data, period);
  #TODO napraviti CMA
  lines(D$time, D$basline, col="red",lty=1)  
  D$ST_IT = D$data/D$basline;
  D$ST = calculate_st(D$ST_IT, period);
  D$deseasonalized = D$data/D$ST;
  
  polyMod <- lm(D$deseasonalized ~ poly(D$time,3), data=D);
  #summary(linearMod)
  D$trendline = polyMod$coefficients[1];
  for(i in 2:length(polyMod$coefficients)){
    D$trendline = D$trendline + polyMod$coefficients[i] * D$time^(i-1);
  }
  lines(D$time, D$trendline, col="blue",lty=1)  
  
  D$forecast = D$ST * D$trendline;
  lines(D$time, D$forecast, col="yellow",lty=1) 
  
  legend("topleft", 
         legend = c("Initial data", "Baseline", "Trendline", "Forecast"),
         col = c("black", "red", "blue", "yellow"), lty = 1, lwd = 1, ncol = 2,
         cex = 0.5)
  
  return(list("D" = D, "model" = polyMod))
}
  