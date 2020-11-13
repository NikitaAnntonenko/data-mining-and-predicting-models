KD <- function(x.data,y, step = 3)
{
  p <- (step - 1)/2
  
  Fx <- function(y) sapply(1:(length(y) - 1), function(i) {
    sum(y[(i-p):(i+p)])/step
  })
  
  myvar <- function(y) sum((y - mean(y))^2)/length(y)
  
  epsvar <- function(y) sum((y - Fx(y))^2)/length(y)
  
  ans<-apply(x.data,2, function(x)
    {
    y.sort <- sortedXyData(x,y)$y
    return(1 - (epsvar(y.sort)/myvar(y.sort)))
  })
  names(ans) <- names(x.data)
  return(ans)
}

#Example
KD(x.data,y)

