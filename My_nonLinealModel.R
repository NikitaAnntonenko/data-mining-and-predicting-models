NEML.MODEL.FIT <- function(f,X,Y) {
  lm(as.formula(f))$fit
}

AIC.data <- function(data)
{
  model <- lm(Y~.,data = data)
  return(extractAIC(model)[2])
}

my.nlm <- function(data, FormulList = c("Y~X","Y~I(1/X)","Y~X+I(X*X)","Y~I(1/X)+I(X*X)",
                                        "Y~X+I(X/X)","Y~I(1/X)+I(X/X)"))
{
  change <- rep(0,length(data) - 1)
  # перебір кожного регресора в моделі
  for(i in 1:(length(data) - 1)) 
  {
    # Підготовка
    eval <- AIC.data(data)
    Xi <- data[,i]
    
    # знаходження претендентів
    data.prog <- sapply(FormulList, function(f) NEML.MODEL.FIT(f,data[,i],data[,length(data)]))
    
    # оцінка претендентів
    all.eval <- apply(data.prog,2, function(X){data[,i] <- X; return(AIC.data(data))})
    
    # відбір кращого претендента 
    if(eval > max(all.eval)) data[,i] <- Xi
    else {eval <- max(all.eval); change[i] <- which.max(all.eval); 
          data[,i] <- data.prog[,change[i]] }
  }
  
  return(list(data = data, AIC = eval, change = change))
  
}