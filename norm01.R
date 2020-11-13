
norm01.matrix <- function(data, with_back, convert_back = F){
  if(convert_back == F){
    mean_and_sd <- sapply(list(mean=mean,sd=sd), 
                          function(f){apply(data, 2, f)})
    data<-apply(data,1,function(x) (x-mean_and_sd[,1])/mean_and_sd[,2])
    data<-1/(1+exp(-data))
    if(with_back == F) return(t(data))
    else return(list(data = t(data),mean_and_sd = mean_and_sd))
  }
  else{
    data$data <- (-log(1/data$data-1))
    data$data <- apply(data$data, 1, 
                       function(x) x*data$mean_and_sd[,2]+data$mean_and_sd[,1])
    return(t(data$data))
  }
}

norm01.numeric <- function(data, with_back, convert_back = F){
  if(convert_back == F){
    dmean <- mean(data)
    dsd <- sd(data)
    data<- (data-dmean)/dsd
    data<-1/(1+exp(-data))
    if(with_back == F) return(data)
    else return(list(data = data,dmean = dmean,dsd = dsd))
  }
  else{
    data$data <- (-log(1/data$data-1))
    data$data <- data*data$dsd+data$dmean
    return(data$data)
  }
}

norm01.data.frame <- function(data, with_back, convert_back = F){
  if(convert_back == F){
    mean_and_sd <- sapply(list(mean=mean,sd=sd), 
                          function(f){apply(data, 2, f)})
    data<-apply(data,1,function(x) (x-mean_and_sd[,1])/mean_and_sd[,2])
    data<-1/(1+exp(-data))
    if(with_back == F) return(as.data.frame(t(data)))
    else return(list(data = as.data.frame(t(data)),mean_and_sd = mean_and_sd))
  }
  else{
    data$data <- (-log(1/data$data-1))
    data$data <- apply(data$data, 1, 
                       function(x) x*data$mean_and_sd[,2]+data$mean_and_sd[,1])
    return(t(data$data))
  }
}
 
norm01 <- function(data, with_back, convert_back = T) UseMethod("norm01")



