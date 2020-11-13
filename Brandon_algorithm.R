Brend.alg <- function(x.data,y,FormulList = 
                       c("Y~X", "Y~exp(X)", 
                         "Y~log(X)", "Y~sqrt(X)","Y~X^2")){
  #Оценка модели (Застаріла)
  nu <- function(y,y.prog){
    # Способ № 1
    # нормированная оценка от 0 до 1 (оценка направленная)
    return(1/(1+sum(abs(y - y.prog))))
    # Способ № 2
    #расчитаем среднеквадратическую ошибку по критерию регулярности :)
    #return(sum(abs(y - y.prog))/sum(y^2))
    
    # Способ № 3
    #return(1 - (sum((y-y.prog)^2)/sum((y - mean(y))^2)))^(1/2)
  }
  
  #Функция что возвращает лучшую модель для x и y, её оценку и прогноз y
  NEML <- function(X,Y){
    # Новий спосіб !!!!!!!
    #All.nu <- sapply(FormulList, function(f) My.cv(f,X,Y))
    #win.form <- which.min(All.nu)
    #return(list(form.num = win.form, nu = All.nu[win.form], 
                #y.prog = lm(as.formula(FormulList[win.form]))$fit))
    
    # Старий спосіб
    Ally.prog <- sapply(FormulList, function(form) 
      y.prog <- lm(as.formula(form))$fit)
    All.nu <- sapply(Ally.prog, function(y.prog) nu(Y,y.prog)) 
    win.form <- which.min(All.nu)
    return(list(form.num = win.form, nu = All.nu[win.form], 
                y.prog = Ally.prog[,win.form]))
  }
  
  #Небольшая подготовка
  y.iter <- y/mean(y)
  y.res <- mean(y)
  result <- matrix(ncol = 2, nrow = length(x.data))
  colnames(result) <- c("form.num","nu")
  rownames(result) <- colnames(x.data)
  #------------------------------------------------
  
  for (h in 1:length(x.data)) {
    mod.inf <- NEML(x.data[,h],y.iter)
    result[h,] <- c(mod.inf$form.num, mod.inf$nu)
    y.iter <- y.iter/mod.inf$y.prog
    y.res <- y.res*mod.inf$y.prog
  }
  
  return(list(res.table = result, nu = nu(y,y.res), y.prog = y.res))
}


# Example

FormulList <- c(
  "Y~X", "Y~exp(X)", "Y~log(X)", "Y~sqrt(X)",
  "Y~X^2")

(f<-Bend.alg(x.data = data[,13:17], y = data[,23], FormulList = FormulList))
cor(data[,23], f$y.prog)
plot(data[,23], f$y.prog)
f$nu
