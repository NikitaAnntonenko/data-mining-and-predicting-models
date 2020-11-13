
create.CV <- function(folds, cost, trim, eps.L){
  list(folds = folds,cost = cost, trim = trim, eps.L = eps.L)
}


gmdh <- function(x.data,y,FormulList,CV){
  library(cvTools)
  #--------------------------------------------------------------
  #функция что выбирает лучшую модель для Y,X1 и X2 из FormulList
  NEML <- function(X1,X2,Y){
    allAIC <- sapply(1:length(FormulList), function(k){
      AIC(lm(as.formula(FormulList[k])))
    })
    Iopt <- which.min(allAIC)
    Mopt <- lm(as.formula(FormulList[Iopt]))
    cvFitLm <- cvLm(Mopt, cost = CV$cost,folds = CV$folds, trim = CV$trim)
     
    #Возвращает оценку и номер лучшей модели для Y,X1 и X2
    return(c(Iopt, cvFitLm$cv))
  }
  #-----------------------------------------------------------
  #Функция, что возвращает модель для Y,X1 и X2 по номеру Iopt
  PRED.NEML <- function(Y,X1,X2,Iopt) {
    Mopt <- lm(as.formula(FormulList[Iopt]))
    return(Mopt)
  }
  #-----------------------------------------------------------
  #Небольшая подготовка
  M <- ncol(x.data)
  AllModel <- list()
  result <- matrix(ncol = 6, nrow = 0)
  h <- 0;
  #------------------------
  #Создание и заполнение начальной матрици для ряда селекции
  #Интерпретация не очень, можно ил переписать???????!!!!!!!!!!
  res1 <- sapply(1:(M-1),function(i) {
    sapply((i+1):M,function(j) c(0,0,i,j,0,Inf))
    })
  res1<-t(do.call(cbind,res1))
  colnames(res1) <- c("H","пп","I","J","Nmod","CVcrit")
  #-----------------------------------------------------------
  #Наращиваем ряды селекции
  repeat{
    h <- h + 1
    CVglob <- res1[1,6]
    #перебр всех пар предикторов
    res1[,5:6]<- t(apply(res1, 1, 
          function(k) NEML(x.data[,k[3]], x.data[,k[4]],y)))
    #сортируем все строки по величине критерия самоотганизации
    res1 <- res1[order(res1[,6]),]
    res1[,1] <- h
    res1[,2] <- 1:nrow(res1)
    #Сохраняем лучшие M моделей
    result <- rbind(result, res1[1:M,])
    #Заново расчитываем и сохраняем M лучших моделей
    listMod1 <- lapply(1:M, function(k) 
      PRED.NEML(y,x.data[,res1[k,3]], x.data[,res1[k,4]],res1[k,5]))
    listCoef1 <- lapply(listMod1, function(fit) fit$coef)
    XP <- sapply(listMod1, function(fit) fit$fit)
    AllModel <- c(AllModel, listCoef1)
    # Заменяем исходную таблицу на подогнанные значенияY (!!!)
    x.data[,1:M] <- XP
    #Проверяем условие окончания построения рядов селекции
    if((CVglob - res1[1,6])<CV$eps.L) break
  }
  return(list(result = result, AllModel = AllModel, x.data = x.data))
}

#example

CV <- create.CV(folds = cvFolds(nrow(x.data), K = 10, R = 5),
                cost = "rtmspe", trim = 0.1,eps.L = 0.001)
FormulList <- c(
  "Y~X1","Y~X2", "Y~X1+X2", "Y~X1+X2+I(X1*X2)",
  "Y~X1+X2+I(X1*X1)","Y~X1+X2+I(X2*X2)",
  "Y~X1+X2+I(X1*X2)+I(X1*X1)","Y~X1+X2+I(X1*X2)+I(X2*X2)",
  "Y~X1+X2+I(X1*X2)+I(X1*X1)+I(X2*X2)")


#gmdh(x.data = X[,1:7],y = X[,8],FormulList = FormulList,CV = CV)$result

ans<-gmdh(x.data = x.data,y = y,FormulList = FormulList,CV = CV)
ans$AllModel[[18]]
