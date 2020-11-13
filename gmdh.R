rm(list = ls())

X <- state.x77[,c(1:3,5:8,4)]
colnames(X)[8] = "Life.Exp"
colnames(X)[5] = "HS.Grad"
M <- ncol(X) ; N <- nrow(X)

FormulList <- c(
  "Y~X1","Y~X2", "Y~X1+X2", "Y~X1+X2+I(X1*X2)",
  "Y~X1+X2+I(X1*X1)","Y~X1+X2+I(X2*X2)",
  "Y~X1+X2+I(X1*X2)+I(X1*X1)","Y~X1+X2+I(X1*X2)+I(X2*X2)",
  "Y~X1+X2+I(X1*X2)+I(X1*X1)+I(X2*X2)")

library(cvTools)
set.seed(1234)

folds <- cvFolds(N, K = 10, R = 5)

NEML <- function(Y,X1,X2){
  allAIC <- sapply(1:length(FormulList), function(k){
    AIC(lm(as.formula(FormulList[k])))
  })
  Iopt <- which.min(allAIC)
  Mopt <- lm(as.formula(FormulList[Iopt]))
  cvFitLm <- cvLm(Mopt, cost = "rtmspe",folds = folds, trim = 0.1)
  return(c(Iopt, cvFitLm$cv))
}

PRED.NEML <- function(Y,X1,X2,Iopt) {
  Mopt <- lm(as.formula(FormulList[Iopt]))
  return(Mopt)
}

eps <- 0.001 #  Малое число для оценки стабилизацииL
AllModel <- list() #  Список для складирования коэффициентов
for(h in 1:(M-1)) { #  Наращивание рядов селекции
  res1 <- matrix(0, ncol=6, nrow=(M-1)*(M-2)/2)
  colnames(res1) <- c("H","пп","I", "J", "Nmod", "CVcrit")
  a <- 0;
  for (i in 1:(M-2)) { #  Перебор всех пар предикторов
    for(j in (i+1):(M-1)) {
      a <- a + 1
      res1[a,1:4] = c(h,0,i,j)
      res1[a,5:6] = NEML(X[,M], X[,i], X[,j])
    }
  }
  #  Получили таблицу частных описаний для одного ряда селекции
  #  Сортируем ее по величине критерия самоорганизации
  res1 <- res1[order(res1[,6]),]
  res1[,2] <- 1:nrow(res1)
  if(h==1) { #  Первый шаг селекции
    CVglob <- res1[1,6] #  Лучшая модель в ряду
    result <- res1[1:(M-1),]
  } else{
    #  Если шаг селекции не первый, проверяем сходимость процесса
    if((CVglob-res1[1,6])<eps) break
    CVglob <- res1[1,6]
    result <- rbind(result, res1[1:(M-1),])
  }
  listMod1 <- lapply(1:(M-1), function(k) PRED.NEML(X[,M],
                                                    X[,res1[k,3]], X[,res1[k,4]],res1[k,5]))
  # Для(M-1)-лучших моделей извлекаем коэффициенты и прогнозы
  listCoef1 <- lapply(listMod1, function(fit) fit$coef)
  XP <- sapply(listMod1, function(fit) fit$fit)
  AllModel <- c(AllModel, listCoef1)
  # Заменяем исходную таблицу на подогнанные значенияY (!!!)
  X[,1:(M-1)] <- XP
}

result


