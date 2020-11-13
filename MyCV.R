My.cv <- function(lm.furmula, X, Y)
{
  Err <- sapply(1:length(X), function(i)
    {
    Yex <- Y[i]; Y <- Y[-i];
    Xex <- X[i]; X <- X[-i];
    lm.temp <- lm(as.formula(lm.furmula))
    Yprog <- predict(lm.temp,newdata = data.frame(X = Xex))
    return((Yex - Yprog)^2)
  })
  return(sqrt(mean(Err)))
}


