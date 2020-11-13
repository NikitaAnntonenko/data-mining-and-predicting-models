boxCox.norm.data.frame <- function(data, with.back = F, convert.back = F)
{
  library(car)
  if(convert.back == F){
    
    if(with.back == F)
    {
      norm.data <- lapply(data,function(X) 
      {
        lm.temp <- lm(X+1~1,y = T,qr = T)
        bc.null <- boxCox(lm.temp)
        lambda <- bc.null$x[which.max(bc.null$y)]
        ((X+1)^lambda - 1)/lambda
      })
      return(as.data.frame(norm.data))
    }
    else 
    {
      all.lambda <- sapply(data,function(X) 
      {
        lm.temp <- lm(X+1~1,y = T,qr = T)
        bc.null <- boxCox(lm.temp)
        bc.null$x[which.max(bc.null$y)]
      })
      norm.data <- sapply(1:length(all.lambda), function(i)
        {
        ((data[,i]+1)^all.lambda[i] - 1)/all.lambda[i]
      })
      norm.data <- as.data.frame(norm.data)
      names(norm.data) <- names(data)
      return(list(data = norm.data,all.lambda = all.lambda))
    }
  }
  else{
    ret.data <- lapply(1:length(data$all.lambda), function(i)
    {
      ((data$data[,i]*data$all.lambda[i] + 1)^(1/data$all.lambda[i])) - 1
    })
    ret.data <- as.data.frame(ret.data)
    names(ret.data) <- names(data$data)
    return(ret.data)
  }
}