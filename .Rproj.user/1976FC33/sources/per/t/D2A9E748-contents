# Librarys -------------------------------------------------
library(outliers) # grubbs.test
library(NbClust) # klast
library(flexclust) # k-means with manchetten
library(corrplot)
library(car) # vif + leveneTest + outlierTest
library(ggplot2) # Графіка
library(cvTools) # cross-validation
library(FWDselect) # selection
library(dplyr) # робота з тиблицями
library(lmtest) # gqtest на гетероскедастичность, resettest тест Рамсея
library(stringr) # Робота з рядками
library(psych) # look at data
library(randomForest) # For random forest
library(rpart) # For classificarion tree
library(rpart.plot) # For classificarion tree visualization
library(plotly) # 3D Visualization
library(neuralnet) # For neural networks
# For XGBoost ---
library(xgboost)
library(methods)
library(data.table)

rm(list = ls())
# ПРЕДОБРАБОТКА -----------------------------

# 1. Зчитуванна данних -------------------------------------------------------

data <- read.table("data.txt",F,'\t',dec = ',',na.strings = "NULL")
names(data) <- paste0("X",1:26)
names(data)[27] <- "Y"

#data$X23 <- as.numeric(data$X23)
#data$X24 <- as.numeric(data$X24)
 # ok # ok
# 2. Логічне видалення -------------------------------------------------------

data$X5 <- NULL # == X6
data$X2 <- NULL # == X7
data$X8 <- NULL # == X3
data$X9 <- NULL # == X4
data$X12 <- NULL # == X11
data$X17 <- NULL # == X16
data$X19 <- NULL # == X18
data$X22 <- NULL # == X26

data$X15 <- NULL # very little data
data$X20 <- NULL # the same (as with X15)
 
# 3. Нормальність розподілу Y -------------------------------------------------------

# ПРИВИДЕНИЕ Y к нормальному закону распределения - 
# єто подразумевает метод lm()
# 1. Проверка на нармальность распределения
hist(data$Y, breaks = 10, col = "lightgreen", xlab = "Y", freq = F,
     main = "Гістограма щільності розподілу ймовірності")
lines(density(data$Y), col = "red", lwd = 2)
shapiro.test(data$Y)
# 2. Нахождение виброса
grubbs.test(data$Y)
# 3. Удаление виброса
data <- data[-which(data$Y == 3266),]
data <- data[-which(data$Y == 3240),]
# 4. Перевірка
shapiro.test(data$Y)

# Висновок: після видалення викида, розподіл Y не став нормальним, але 
# по результатам "4. Перевірка" розподіл незначним
# чином відрізняється від нормального, це допускає метод lm()
 
# 4. box-counting -------------------------------------------------------

# The Closer to 0 the better
pairs(data[,c(13,14,15)])
box_counting.matrix(data[,c(1:12,16)],data$Y, eps = 0.1)
 
# 5. Дисперсійний аналіз --------------------------------------------------------------------

data.disp <- data.frame(X23 = data$X23, X24 = data$X24, X25 = data$X25, Y = data$Y)

# Побудова моделі з використанням контрастів комбінації умов

# --- Модель 1 ---
model.disp.X23 <- lm(formula = Y ~ X23, data = data.disp)
summary(model.disp.X23)
confint(model.disp.X23)
# Let's create a "весна/осінь/зима" instead "весна/осінь" and "зима" levels
levels(data.disp$X23)[2] <- "весна/осінь/зима"
levels(data.disp$X23)[1] <- "весна/осінь/зима"
model.disp.X23 <- lm(formula = Y ~ X23, data = data.disp)
summary(model.disp.X23)
# Перевірка умови нормальності (виконується)
hist(model.disp.X23$residuals,breaks = 15, col = "lightblue", xlab = "Residuals",
     main = "Гістограма частоти розподілу залишків")
shapiro.test(model.disp.X23$residuals)
# Перевірка умови гомоскедастичності (виконується)
plot(model.disp.X23)
leveneTest(data.disp$Y, data.disp$X23)
# Substitution
data$X23 <- model.disp.X23$fitted.values

# --- Модель 2 ---
model.disp.X24 <- lm(formula = Y ~ X24, data = data.disp)
summary(model.disp.X24)
# Let's create a "Далмонт/Ингкоу/Майертон" instead ...
levels(data.disp$X24)[c(1,3,4)] <- "Далмонт/Ингкоу/Майертон"
model.disp.X24 <- lm(formula = Y ~ X24, data = data.disp)
summary(model.disp.X24)
confint(model.disp.X24)
# Перевірка умови нормальності (не виконується)
hist(model.disp.X24$residuals,breaks = 15, col = "lightblue", xlab = "Residuals",
     main = "Гістограма частоти розподілу залишків")
shapiro.test(model.disp.X24$residuals)
# Заходи
model.disp.X24 <- lm(formula = log(Y) ~ X24, data = data.disp)
summary(model.disp.X24)
# Перевірка умови нормальності (виконується)
hist(model.disp.X24$residuals,breaks = 15, col = "lightblue", xlab = "Residuals",
     main = "Гістограма частоти розподілу залишків")
shapiro.test(model.disp.X24$residuals)
# Перевірка умови гомоскедастичності (виконується)
plot(model.disp.X24)
leveneTest(data.disp$Y, data.disp$X24)
# Substitution
data$X24 <- exp(model.disp.X24$fitted.values)

# --- Модель 3 ---
model.disp.X25 <- lm(formula = Y ~ X25, data = data.disp)
summary(model.disp.X25)
confint(model.disp.X25)
# Senselessly!!!
data$X25 <- NULL

rm(list = c("data.disp","model.disp.X23","model.disp.X24","model.disp.X25"))
 
# 6. Перевірка на мультиколінеарність -------------------------------------------------------


col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow",
                           "#7FFF7F", "cyan", "#007FFF", "blue","#00007F"))
corrplot(cor(data.new,method = "spearman"), method="color", col=col4(20), cl.length=21,
         order = "AOE", addCoef.col="green")

#data$X6 <- NULL
vif(lm(Y~.,data = data))

#data$X13 <- NULL
vif(lm(Y~.,data = data[,-8]))

data$X6 <- NULL # corelation with X13!
data$X26 <- NULL # corelation with X18!
# Try again
corrplot(cor(data[-14],method = "spearman"), method="color", col=col4(20), cl.length=21,
         order = "AOE", addCoef.col="green")

rm(list = "col4")

# 7. Додатковий розділ про X18 -------------------------------------------------------
# Plot
colors <- rep(100,89)
colors[c(8,73)] <- 20
colors[68] <- 0

qplot(data = data, X18, Y,
      xlab = "X18",
      ylab = "Y", col = colors) + 
  stat_smooth(method = "lm")
# With 68 row
model.X18 <- lm(Y ~ X18, data = data.new)
plot(model.X18)
hist(model.X18$residuals,breaks = 15, xlab = "Residuals", 
     main = "Гістограма частоти розподілу залишків", col = 7)
shapiro.test(model.X18$residuals)
# Without 68 row
model.X18 <- lm(Y ~ X18, data = data[-68,])
plot(model.X18)
hist(model.X18$residuals,breaks = 15, xlab = "Residuals", 
     main = "Гістограма частоти розподілу залишків", col = 7)
shapiro.test(model.X18$residuals)
# Заходи: 
data <- data[c(-68),]

rm(list = c("colors","model.X18"))
  
# 8. Нормування даних -----------------------------------------------------------------------

data.scale <- select(data, -X6, -X24Fact)
data.scale <- as.data.frame(scale(data.scale, center = sapply(data.scale, min), 
                                  scale = sapply(data.scale, max) - sapply(data.scale, min)))
 
# 9. Перевірка на викиди -------------------------------------------------------

boxplot(select(data.scale.for.outliers, -Clust, -X23, -X24), col = "coral")

grubbs.test(data$X1,type = 10)
grubbs.test(data$X3,type = 10); boxplot(data$X3);
grubbs.test(data$X4,type = 10)
grubbs.test(data$X7,type = 10)
grubbs.test(data$X10,type = 10)
grubbs.test(data$X11,type = 10)
grubbs.test(data$X13,type = 10)
grubbs.test(data$X14,type = 10)
grubbs.test(data$X16,type = 10)
grubbs.test(data$X18,type = 10); boxplot(data$X18); which.max(data$X18); 
grubbs.test(data$X21,type = 10)
grubbs.test(data$Y,type = 10)
 
# 10. PCA -----------------------------------------------------------------------------

#Знаходження головних компонент
data.pca <- prcomp(data[,c(-15, -16)], scale = TRUE)
summary(data.pca)
# Побудова графіку
biplot(data.pca, 
main = "Відображення всіх данних в координатах першої 
та другої головних компонент")

# 11. Clustering ----------------------------
# 11.1. Hierarchical cluster analysis
data.clustering <- hclust(dist(data.scale, method = "manhattan"), "ward.D")
plot(data.clustering)
rect.hclust(data.clustering, k=2, border="blue")
data.clustering$groups <- cutree(data.clustering, k=2)
# Concusion - 2 clusres the best! --- 

# 11.2. K meens
#  Определение числа кластеров (2 the best!!!) ---
NbClust <- NbClust(data.scale, distance = "manhattan", 
                   min.nc = 2, max.nc = 15, method = "ward.D", index = "alllong")
data.clustering.km <- kcca(data.scale, 2,  family=kccaFamily("kmedians")) # for manhattan!!!

data$Clust <- (data.clustering$cluster - 1)
# 2D Visualization
plot(data.pca$x[,1], data.pca$x[,2], type = "n", xlab = "Перша головна компонента", 
     ylab = "Друга головна компонента")
for (i in 1:88) { text(data.pca$x[i,1], data.pca$x[i,2], label = as.character(i), 
                       col = c("blue", "red")[data$Clust + 1][i]) }
# 3D Visualization
threeComp <- data.frame(z = data.pca$x[,3], y = data.pca$x[,2], x = data.pca$x[,1])
plot_ly(threeComp,x=~x,y=~y,z=~z, color = ~data$Clust)

# 11.3 Знаходження загальних ознак у кластерів ---
sort(abs(data.pca$rotation[,1])) # X18 X16 X24 X13 X11 X1 X4 - have a heavy weight
# 1. X18 BIGEST in second clucter
tapply(data$X18, data$Clust, mean)[2]*100 / tapply(data$X18, data$Clust, mean)[1]
# 2. X16 BIGEST in second clucter
tapply(data$X16, data$Clust, mean)[2]*100 / tapply(data$X16, data$Clust, mean)[1]
# 3. X24 BIGEST in second clucter
tapply(data$X24, data$Clust, mean)[2]*100 / tapply(data$X24, data$Clust, mean)[1]
# 4. X13 BIGEST in second clucter
tapply(data$X13, data$Clust, mean)[2]*100 / tapply(data$X13, data$Clust, mean)[1]
# 5. X11 LOWER in second clucter
tapply(data$X11, data$Clust, mean)[2]*100 / tapply(data$X11, data$Clust, mean)[1]
# 6. X1 BIGEST in second clucter
tapply(data$X1, data$Clust, mean)[2]*100 / tapply(data$X1, data$Clust, mean)[1]
# 7. X4 BIGEST A LITTLE in second clucter
tapply(data$X4, data$Clust, mean)[2]*100 / tapply(data$X4, data$Clust, mean)[1]
# 8. X3 BIGEST A LITTLE in second clucter
tapply(data$X3, data$Clust, mean)[2]*100 / tapply(data$X3, data$Clust, mean)[1]

# Split data for lm!
data$Clust <- data.clustering$groups
data.c1 <- select(data[data$Clust == 0,], -Clust)
data.c2 <- select(data[data$Clust == 1,], -Clust)

# 1. Sirka
# Without clustering
qplot(data = data, X4, Y,
      xlab = "X4",
      ylab = "Y", col) + 
  stat_smooth(method = "lm")
# For first cluster
qplot(data = data[data$Clust == 0,], X4, Y,
      xlab = "X4",
      ylab = "Y") + 
  stat_smooth(method = "lm")
# For second cluster
qplot(data = data[data$Clust == 1,], X4, Y,
      xlab = "X4",
      ylab = "Y", col) + 
  stat_smooth(method = "lm")

# 2. X18 norm
# Without clustering
hist(data$X18,breaks = 15, col = "lightblue", xlab = "X18",
     main = "Гістограма частоти розподілу")
# For first cluster
hist(data$X18[data$Clust == 0][-9],breaks = 10, col = "lightblue", xlab = "X18",
     main = "Гістограма частоти розподілу")
# For second cluster
hist(data$X18[data$Clust == 1],breaks = 10, col = "lightblue", xlab = "X18",
     main = "Гістограма частоти розподілу")

rm(list = c("NbClust", "i", "data.pca","data.clustering","threeComp","data.scale","data.clustering.km"))

# 10. Normalization clustrs Y -----------------------------------------------------------------------------

hist(data[data$Clust == 0,]$Y, breaks = 20)
length(data[data$Clust == 0,]$Y)
shapiro.test(data[data$Clust == 0,]$Y)
hist(data[data$Clust == 1,]$Y, breaks = 20)
shapiro.test(data[data$Clust == 1,]$Y)

# -------------------------------------------
# ПОБУДОВА МОДЕЛЕЙ---------------------------

index <- Sort.by.pca(select(data,-X24Fact),5)
data.train <- data[-index,]
data.test <- data[index,]
data.train.scale <- data.scale[-index,]
data.test.scale <- data.scale[index,]

# Лінійна модель -------------------------------------------------------
data$X6 <- data.new$X6 #!!!!! :)
# відбір "AddDel"
{
# data$prev <- c(0,data$Y[1:87]) do not work
nlm.model <- step(lm(data = select(data, -Clust, -X24Fact), Y ~ .),direction = "both")
# data.temp$X16[which.max(data.temp$X16)] <- 600
# nlm.model <- lm(data = data, Y ~ 0 + X3 + X11 + X13 + X18 + X24)
nlm.model.c1 <- step(lm(data = select(data[data$Clust == 0,], -Clust, -X24Fact), Y~.),direction = "both")
# nlm.model.c1 <- lm(Y ~ X4 + X18 + X21 + X24, data = data[data$Clust == 0,])
nlm.model.c2 <- step(lm(data = select(data[data$Clust == 1,], -Clust, -X24Fact), Y~.),direction = "both")
# nlm.model.c2 <- lm(Y ~ X1 + X3 + X10 + X13 + X16 + X18, data = data[data$Clust == 1,])

# Перевірка моделі 0
summary(nlm.model)
confint(nlm.model)
plot(nlm.model)
hist(nlm.model$residuals,  col = "green")
shapiro.test(nlm.model$residuals)
outlierTest(nlm.model) # ?????????????????????????????!!!!!!!!!!
acf(nlm.model$residuals)
# Перевірка моделі 1
summary(nlm.model.c1)
plot(nlm.model.c1)
hist(nlm.model.c1$residuals,  col = "lightblue")
shapiro.test(nlm.model.c1$residuals)
outlierTest(nlm.model.c1) # ?????????????????????????????!!!!!!!!!!
acf(nlm.model.c1$residuals)
# set.seed(1234)
# cvLm(nlm.model)

# Перевірка моделі 2
summary(nlm.model.c2)
plot(nlm.model.c2)
hist(nlm.model.c2$residuals,  col = "lightblue")
shapiro.test(nlm.model.c2$residuals)
outlierTest(nlm.model.c2) # ?????????????????????????????!!!!!!!!!!
acf(nlm.model.c2$residuals)

# Висновок: відібрані:
# X3 X11 X13 X16 X18 X21 X24 (7)!!!
# AIC = -243.58 знизився лише на 3.3% !!!
# Усі тести пройшов успішно !!!!!
}


# Класичний МГУА -------------------------------------------------------

# Побудова моделі
CV <- create.CV(folds = cvFolds(nrow(data), K = 10, R = 9),
                cost = "rtmspe", trim = 0.1,eps.L = 0.000001)
set.seed(1234)
ans1 <- gmdh.classic(x.data = data[,-length(data)],y = data[,length(data)],CV = CV)
ans1$result

# Результат gmdh.cl.model

{
prog.data <- data.frame(
  U1 = lm(data = data, Y~X16*X18)$fit,
  U2 = lm(data = data, Y~I(1/X3)+X13+I(X3/X13))$fit,
  Y = data$Y
)
    
gmdh.cl.model <- lm(data = prog.data, Y~U1+I(1/U2)+I(U2*U2))

# Очистка
rm(list = c("prog.data"))
}

# Змінений МГУА -------------------------------------------------------

# Побудова моделі
CV <- create.CV(folds = cvFolds(nrow(data), K = 10, R = 9),
                cost = "rtmspe", trim = 0.1,eps.L = 0.001)
set.seed(1234)
ans2 <- gmdh(x.data = data[,-c(15,length(data))],y = data[,length(data)],CV = CV)
ans2$result


# Результат gmdh.new.model
qplot(data = data,X3,Y)

{
  prog.data <- data.frame(
    U1 = lm(data = data, Y~I(1/X7)+X18+I(X7*X18))$fit,
    U2 = lm(data = data, Y~X3+X18+I(X3*X18))$fit,
    U3 = lm(data = data, Y~X6+X18+I(X18*X18))$fit,
    U4 = lm(data = data, Y~X11+X18+I(X18*X18))$fit,
    Y = data$Y
  )
  
  prog.data2 <- data.frame(
    V1 = lm(data = prog.data, Y~U1+U2)$fit,
    V2 = lm(data = prog.data, Y~U3+I(1/U4)+I(1/(U3*U3)))$fit,
    Y = data$Y
  )
  
  gmdh.new.model <- lm(data = prog.data2, Y~V1+V2+I(V2/V1))
  rm(list = c("prog.data","prog.data2","CV"))
}

# Neural networks --------------------------

# 1) Prepare ---
num.nets <- 100 # number of networks
error.best <- Inf # start error
error.best.train <- Inf # start error of train data
hidden.max <- 7 # max neirons in hidden layer
seed.start <- 0
seed.current <- seed.start
err.vect <- matrix(data = rep(Inf,num.nets*hidden.max), nrow = hidden.max, ncol = num.nets)
err.vect.test <- matrix(data = rep(Inf,num.nets*hidden.max), nrow = hidden.max, ncol = num.nets)

# 2) Building network --- 
for (hidden.n in 1 : hidden.max) {
  
  seed.current  <- seed.start
  
  for ( i in 1 : num.nets ){ 
    
    # 2.1) A network builds
    set.seed(seed.current)
    nn.temp <- neuralnet( 
      Y ~ X1 + X3 + X4 + X7 + X10 + X11 + X13 + X14 + X16 + X18 + X21 + X23 + X24 + Clust , 
      data = data.train.scale ,hidden = hidden.n, linear.output=F)
    
    # 2.2) Get error on data.train
    forecast.temp     <- compute(nn.temp, select(data.test.scale,-Y) )  # get forecast
    error.temp        <- sum( ( forecast.temp$net.result - data.test.scale$Y )^2 ) # get error 
    error.tepm.train <- nn.temp$result.matrix[1,1]
    
    # 2.3) save errors
    err.vect[hidden.n,i] <- error.tepm.train
    err.vect.test[hidden.n,i] <- error.temp
    
    #  2.4) if error lower than previous
    if ( ( error.temp + error.tepm.train ) < ( error.best + error.best.train ) )  {
      nn.best     <- nn.temp
      error.best  <- error.temp
      error.best.train <- error.tepm.train
      hidden.best <- hidden.n
    }
    
    # 2.5) Set next seed
    seed.current<-seed.current+1
    
  }
  
}

# 3)  Visualization the best network
plot(nn.best)
predict.net <- compute(nn.best, select(data.test.scale, -Y))$net.result[,1]
cor(predict.net, data.test.scale$Y)^2
cor(nn.best$net.result[[1]], data.train.scale$Y)^2

plot(1:88, data.scale$Y, type = "l")
lines(1:88,neuralnet::compute(nn.best,select(data.scale,-Y))$net.result, col = "red")

rm(list = c("err.vect","err.vect.test","forecast.temp","nn.best.train","nn.temp","error.best",
            "error.best.train","error.temp","error.tepm.train","hidden.best","hidden.max",
            "hidden.n","i","index","num.nets","seed.current","seed.start"))

# Classter tree -----------------------------

# Without clusters --- 
tree.model <- rpart(Y~.,data = select(data,-Clust,-X18, -X24),method = "anova",
                    control = rpart.control(minsplit = 10, minbucket = 5, maxdepth = 6))
cor(data$Y, predict(tree.model, select(data, -Clust, -Y, -X18), type = "vector"))^2

plotcp(tree.model)
printcp(tree.model)
rpart.plot(tree.model)
# tree.model <- prune()

plot(1:88, data$Y, type = "l")
lines(1:88,predict(tree.model, select(data, -Clust, -X18, -Y), type = "vector"), col = "red")

# With Clusters ---
tree.model.clust <- rpart(Y~.,data = select(data, -X18, -X24, -X6) ,method = "anova",
                    control = rpart.control(minsplit = 10, minbucket = 5, maxdepth = 6))
cor(data$Y, predict(tree.model.clust, select(data, -Y), type = "vector"))^2

plotcp(tree.model.clust)
printcp(tree.model.clust)
rpart.plot(tree.model.clust)
# tree.model <- prune()

plot(1:88, data$Y, type = "l")
lines(1:88,predict(tree.model.clust, select(data, -X18, -Y), type = "vector"), col = "red")


rm(list = c("index","i","next.r2","next.train.r2","start.r2","start.seed"))

# Random Forest -----------------------------

set.seed(3217)
rf.model <- randomForest(x = as.data.frame(cbind(select(data.train, -Y), Nlm = nlm.model$fitted.values[-index])), y = data.train$Y, 
                         mtry = floor(sqrt(ncol(data.train))), ntree = 1000, nodesize = 1,
                         do.trace = 1000/20, keep.forest = T, replace = F, importance = T,
                         localImp = F, proximity = F, norm.votes = T, corr.bias = F, keep.inbag = F)
predict.rf <- predict(rf.model, newdata = as.data.frame(cbind(select(data.test, -Y), Nlm = nlm.model$fitted.values[index])))

cor(rf.model$predicted, data.train$Y)^2
cor(predict.rf, data.test$Y)^2

plot(1:88, data$Y, type = "l")
lines(1:88,predict(rf.model, newdata = as.data.frame(cbind(select(data, -Y, -X6, -X24Fact), 
                                                           Nlm = nlm.model$fitted.values))), col = "red")

varImpPlot(rf.model)
varUsed(rf.model, by.tree = F, count = T)

# XGBoost -----------------------------------

xgboost.model <- xgboost(data = cbind(as.matrix(select(data.train,-Y)),
                                      nlm.model$fitted.values[-index]), label = data.train$Y,
                         objective = "reg:linear", eval_metric = "rmse", max.depth = 1,
                         eta = 0.0005, nrounds = 10000, subsample = 1, colsample_bytree = 0.7, 
                         nthread = 3)
predict.xgboost <- predict(xgboost.model, cbind(as.matrix(select(data.test,-Y)), 
                                                nlm.model$fitted.values[index]))
cor(predict.xgboost, data.test$Y)^2
cor(predict(xgboost.model,cbind(as.matrix(select(data.train,-Y)), 
            nlm.model$fitted.values[-index])), data.train$Y)^2

plot(1:88, data$Y, type = "l")
lines(1:88,predict(xgboost.model, newdata = cbind(as.matrix(select(data,-Y,-X24Fact,-X6)), 
                                                  nlm.model$fitted.values)), col = "red")

# -------------------------------------------
# ПЕРЕВІРКА МОДЕЛІ---------------------------
# Довірчі інтервали -------------------------------------------------------

confint(nlm.model)
confint(gmdh.cl.model)
confint(gmdh.new.model)

# Специфікація моделі та Гомоскадастичність -------------------------------------------------------

# --- Модель 1 ---
# 1) ---
summary(nlm.model)
plot(nlm.model, 
     main = "Bonferonni p = 1. Point '69'")
outlierTest(nlm.model) # > 1 !!!!!
# 2) ---
summary(nlm.model.c1)
plot(nlm.model.c1, 
     main = str_c("Bonferonni p = ", 
                  as.character(outlierTest(nlm.model.c1)$bonf.p),
                  ". Point '",names(outlierTest(nlm.model.c1)$bonf.p),"'"))
# 3) ---
summary(nlm.model.c2)
plot(nlm.model.c2, 
     main = str_c("Bonferonni p = ", 
                  as.character(outlierTest(nlm.model.c2)$bonf.p),
                  ". Point '",names(outlierTest(nlm.model.c2)$bonf.p),"'"))

# --- Модель 2 ---

summary(gmdh.cl.model)
plot(gmdh.cl.model, 
     main = str_c("Bonferonni p = ", 
                  as.character(outlierTest(gmdh.cl.model)$bonf.p),
                  ". Point '",names(outlierTest(gmdh.cl.model)$bonf.p),"'"))
# --- Модель 3 ---

summary(gmdh.new.model)
plot(gmdh.new.model,
     main = str_c("Bonferonni p = ", 
    as.character(outlierTest(gmdh.new.model)$bonf.p),
    ". Point '",names(outlierTest(gmdh.new.model)$bonf.p),"'"))

# --- Model 4 (Neural network) ---

qplot(compute(nn.best, select(data.scale, -Y))$net.result, 
      compute(nn.best, select(data.scale, -Y))$net.result - data.scale$Y)
var.test(compute(nn.best, select(data.scale, -Y))$net.result, 
         compute(nn.best, select(data.scale, -Y))$net.result - data.scale$Y)

# --- Model 5 (Tree) ---

# 1) ---
qplot(predict(tree.model, select(data, -Clust, -Y, -X18, -X6), type = "vector"), 
      predict(tree.model, select(data, -Clust, -Y, -X18), type = "vector") - data$Y,
      xlab = "Predicted Values", ylab = "Residuals") + stat_smooth(method = "lm")

# 2) ---
qplot(predict(tree.model.clust, select(data, -Y, -X18), type = "vector"), 
      predict(tree.model.clust, select(data, -Y, -X18), type = "vector") - data$Y)
var.test(predict(tree.model.clust, select(data.train, -Y), type = "vector"), 
         predict(tree.model.clust, select(data.train, -Y), type = "vector") - data$Y)

# --- Model 6 (RF) ---

qplot(rf.model$predicted, rf.model$predicted - data.train$Y)
var.test(rf.model$predicted, (rf.model$predicted - data.train$Y))

#  --- Model 7 (XGBoost) ---

qplot(predict(xgboost.model, cbind(as.matrix(select(data.train,-Y)), 
                                   nlm.model$fitted.values[-index])), 
              predict(xgboost.model, cbind(as.matrix(select(data.train,-Y)), 
                                           nlm.model$fitted.values[-index])) - data.train$Y)
var.test(predict(xgboost.model, cbind(as.matrix(select(data.train,-Y)), 
                                   nlm.model$fitted.values[-index])), 
      predict(xgboost.model, cbind(as.matrix(select(data.train,-Y)), 
                                   nlm.model$fitted.values[-index])) - data.train$Y)
# Тест на нормальний розподіл залишків -------------------------------------------------------

# --- Модель 1 ---

hist(nlm.model$residuals,breaks = 10, col = "lightgreen",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(nlm.model$residuals)[2])))
lines(density(nlm.model$residuals), col = "red", lwd = 2)


# --- Модель 2 ---

hist(gmdh.cl.model$residuals,breaks = 20, col = "lightgreen",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(gmdh.cl.model$residuals)[2])))
lines(density(gmdh.cl.model$residuals), col = "red", lwd = 2)


# --- Модель 3 ---

hist(gmdh.new.model$residuals,breaks = 20, col = "lightgreen",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(gmdh.new.model$residuals)[2])))
lines(density(gmdh.new.model$residuals), col = "red", lwd = 2)

# --- Model 4 (neural Network) ---

res.net <- compute(nn.best, select(data.scale, -Y))$net.result - data.scale$Y

hist(res.net,breaks = 20, col = "lightgreen",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(res.net)[2])))
lines(density(res.net), col = "red", lwd = 2)

rm(list = "res.net")

# --- Model 5 (Tree) ---

residuals.tree <- predict(tree.model, select(data, -Clust, -Y, -X6), type = "vector") - data$Y

hist(residuals.tree, breaks = 10, col = "yellow",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(residuals.tree)[2])))
lines(density(residuals.tree), col = "red", lwd = 2)

rm(list = c("residuals.tree"))

# --- Model 6 (RF) ---

hist(rf.model$predicted - data.train$Y,breaks = 20, col = "lightgreen",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(rf.model$predicted - data.train$Y)[2])))
lines(density(rf.model$predicted - data.train$Y), col = "red", lwd = 2)

hist(predict.rf - data.test$Y,breaks = 20, col = "lightgreen",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(predict.rf - data.test$Y)[2])))
lines(density(predict.rf - data.test$Y), col = "red", lwd = 2)

# --- Model 7 (XGBoost) ---

predict.temp <- predict(xgboost.model, cbind(as.matrix(select(data.train,-Y)), 
                                             nlm.model$fitted.values[-index])) - data.train$Y
hist(predict.temp,breaks = 20, col = "lightgreen",
     xlab = "Residuals", freq = F,
     main = str_c("Shapiro test, p-value = ",
                  as.character(shapiro.test(predict.temp)[2])))
lines(density(predict.temp), col = "red", lwd = 2)

rm(list = "predict.temp")

# Autocotelation ---------------------------

acf(nlm.model$residuals)
acf(nlm.model.c1$residuals)
acf(nlm.model.c2$residuals)
acf(predict(tree.model, select(data, -Clust, -X18, -Y, -X6), type = "vector") - data$Y, 
    main = "Autocrelation function")
acf(predict(tree.model.clust, select(data.train, -X18, -Y), type = "vector") - data.train$Y)

acf(nn.best$net.result[[1]] - data.train.scale$Y)
acf(rf.model$predicted - data.train$Y)


# Тест Рамсея -------------------------------------------------------

resettest(nlm.model)
resettest(gmdh.cl.model)
resettest(gmdh.new.model)

# AIC критерій ------------------------------------------------------
extractAIC(nlm.model)
extractAIC(gmdh.cl.model)
extractAIC(gmdh.new.model)
# Середня квадратична похибка апроксимації --------------------------
sum((nlm.model$residuals - mean(nlm.model$residuals))^2)/
  (length(nlm.model$residuals) - 1)

sum((gmdh.cl.model$res - mean(gmdh.cl.model$res))^2)/
  (length(gmdh.cl.model$res) - 1)

sum((gmdh.new.model$res - mean(gmdh.new.model$res))^2)/
  (length(gmdh.new.model$res) - 1)

var(nlm.model$res)
var(gmdh.cl.model$res)
var(gmdh.new.model$res)















x <- data$Y / data$X22
y <- data$Y
plot(x, y, type = 'n')
for (i in 1:91) { text( x[i], y[i], label = as.character(i)) }

x <- data$X22
plot(x, y, type = 'n')
for (i in 1:91) { text( x[i], y[i], label = as.character(i)) }





