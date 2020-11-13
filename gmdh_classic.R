
create.CV <- function(folds, cost, trim, eps.L){
  list(folds = folds,cost = cost, trim = trim, eps.L = eps.L)
}


gmdh.classic <- function(x.data,y,CV){
  library(cvTools)
  FormulList <- c("Y~X1","Y~X2","Y~dX1","Y~dX2",
                  "Y~X1.X1","Y~X2.X2","Y~X1dX2","Y~X2dX1","Y~X1.X2","Y~dX1.X2","Y~dX2.X2","Y~dX1.X1",
                  "Y~X1+X2","Y~dX1+X2","Y~X1+dX2","Y~dX1+dX2",
                  "Y~X1+X2+X1.X1",
                  "Y~dX1+X2+X1.X1","Y~X1+dX2+X1.X1","Y~dX1+dX2+X1.X1",
                  "Y~X1+X2+dX1.X1",
                  "Y~dX1+X2+dX1.X1","Y~X1+dX2+dX1.X1","Y~dX1+dX2+dX1.X1",
                  "Y~X1+X2+X2.X2",
                  "Y~dX1+X2+X2.X2","Y~X1+dX2+X2.X2","Y~dX1+dX2+X2.X2",
                  "Y~X1+X2+dX2.X2",
                  "Y~dX1+X2+dX2.X2","Y~X1+dX2+dX2.X2","Y~dX1+dX2+dX2.X2",
                  "Y~X1+X2+X1.X2",
                  "Y~dX1+X2+X1.X2","Y~X1+dX2+X1.X2","Y~dX1+dX2+X1.X2",
                  "Y~X1+X2+X1dX2",
                  "Y~dX1+X2+X1dX2","Y~X1+dX2+X1dX2","Y~dX1+dX2+X1dX2",
                  "Y~X1+X2+X2dX1",
                  "Y~dX1+X2+X2dX1","Y~X1+dX2+X2dX1","Y~dX1+dX2+X2dX1",
                  "Y~X1+X2+dX1.X2",
                  "Y~dX1+X2+dX1.X2","Y~X1+dX2+dX1.X2","Y~dX1+dX2+dX1.X2"
  )
  #--------------------------------------------------------------
  #функция что выбирает лучшую модель для Y,X1 и X2 из FormulList
  NEML <- function(X1,X2,Y){
    
    
    # Сортування за виличиною дисперсій
    #XY <- data.frame(X1 = X1,X2 = X2, Y = Y)
    XY <- Sort.var.data.frame(data.frame(X1 = X1,X2 = X2, Y = Y))
    # Поділ вибірки на навчальну та єкзаменаційне
    exam.n <- round(length(X1)/100*60)
    X1ex <- XY$X1[-(1:exam.n)]; X1 <- XY$X1[1:exam.n];
    X2ex <- XY$X2[-(1:exam.n)]; X2 <- XY$X2[1:exam.n];
    Yex <- XY$Y[-(1:exam.n)]; Y <- XY$Y[1:exam.n];
    rm(list = "XY")
    
    # Маразм (маленький)
    dX1 <- 1/X1; dX2 <- 1/X2; X1.X2 <- X1*X2; X1.X1 <- X1*X1; 
    X2.X2 <- X2*X2; X1dX2 <- X1/X2; X2dX1 <- X2/X1; dX1.X2 <- 1/(X1*X2);
    dX2.X2 <- 1/(X2*X2); dX1.X1 <- 1/(X1*X1);
    
    # Єкзаменаційна таблиця
    ex.data <- data.frame(X1 = X1ex,X2 = X2ex, dX1 = 1/X1ex, dX2 = 1/X2ex, 
                          X1.X2 = X1ex*X2ex, X1.X1 = X1ex*X1ex, X2.X2 = X2ex*X2ex, 
                          X1dX2 = X1ex/X2ex, X2dX1 = X2ex/X1ex, dX1.X2 = 1/(X1ex*X2ex),
                          dX2.X2 = 1/(X2ex*X2ex), dX1.X1 = 1/(X1ex*X1ex))
    
    
    # Расчитиваем оценки для всех моделей
    allScores <- sapply(FormulList, function(f) 
      {
      lm.temp <- lm(as.formula(f))
      Yprog <- predict(lm.temp, newdata = ex.data)
      #score <- sum(abs(Yex - Yprog))/sum(Yex^2)
      score <- sqrt(sum((Yex - Yprog)^2)/length(Yex))
      return(score)
    })
    
    #Находим и возвращаем оценку и номер лучшей модели для Y,X1 и X2
    Iopt <- which.min(allScores)
    
    return(c(Iopt,allScores[Iopt]))
  }
  #-----------------------------------------------------------
  #Функция, что возвращает модель для Y,X1 и X2 по номеру Iopt
  PRED.NEML <- function(Y,X1,X2,Iopt) {
    
    # Маразм (маленький)
    dX1 <- 1/X1; dX2 <- 1/X2; X1.X2 <- X1*X2; X1.X1 <- X1*X1; 
    X2.X2 <- X2*X2; X1dX2 <- X1/X2; X2dX1 <- X2/X1; dX1.X2 <- 1/(X1*X2);
    dX2.X2 <- 1/(X2*X2); dX1.X1 <- 1/(X1*X1);
    
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


# FormulList <- c("Y~X1","Y~X2","Y~I(1/X1)","Y~I(1/X2)",
#                 "Y~I(X1*X1)","Y~I(X2*X2)","Y~I(X1/X2)","Y~I(X2/X1)","Y~I(X1*X2)","Y~I(1/(X1*X2))","Y~I(1/(X2*X2))","Y~I(1/(X1*X1))",
#                 "Y~X1+X2","Y~I(1/X1)+X2","Y~X1+I(1/X2)","Y~I(1/X1)+I(1/X2)",
#                 "Y~X1+X2+I(X1*X1)",
#                 "Y~I(1/X1)+X2+I(X1*X1)","Y~X1+I(1/X2)+I(X1*X1)","Y~I(1/X1)+I(1/X2)+I(X1*X1)",
#                 "Y~X1+X2+I(1/(X1*X1))",
#                 "Y~I(1/X1)+X2+I(1/(X1*X1))","Y~X1+I(1/X2)+I(1/(X1*X1))","Y~I(1/X1)+I(1/X2)+I(1/(X1*X1))",
#                 "Y~X1+X2+I(X2*X2)",
#                 "Y~I(1/X1)+X2+I(X2*X2)","Y~X1+I(1/X2)+I(X2*X2)","Y~I(1/X1)+I(1/X2)+I(X2*X2)",
#                 "Y~X1+X2+I(1/(X2*X2))",
#                 "Y~I(1/X1)+X2+I(1/(X2*X2))","Y~X1+I(1/X2)+I(1/(X2*X2))","Y~I(1/X1)+I(1/X2)+I(1/(X2*X2))",
#                 "Y~X1+X2+I(X1*X2)",
#                 "Y~I(1/X1)+X2+I(X1*X2)","Y~X1+I(1/X2)+I(X1*X2)","Y~I(1/X1)+I(1/X2)+I(X1*X2)",
#                 "Y~X1+X2+I(X1/X2)",
#                 "Y~I(1/X1)+X2+I(X1/X2)","Y~X1+I(1/X2)+I(X1/X2)","Y~I(1/X1)+I(1/X2)+I(X1/X2)",
#                 "Y~X1+X2+I(X2/X1)",
#                 "Y~I(1/X1)+X2+I(X2/X1)","Y~X1+I(1/X2)+I(X2/X1)","Y~I(1/X1)+I(1/X2)+I(X2/X1)",
#                 "Y~X1+X2+I(1/(X1*X2))",
#                 "Y~I(1/X1)+X2+I(1/(X1*X2))","Y~X1+I(1/X2)+I(1/(X1*X2))","Y~I(1/X1)+I(1/X2)+I(1/(X1*X2))"
# )
