rm(list = ls())
#--------------------PCA----------------------------
library("psych")
library("dplyr")
#data prepare-------- 
data<-read.table("data.txt",T,'\t',dec = ',')
y <- data$y
data <- select(data, -x2,-x15,-x20,-x22,-y)
y <- as.character(y)
y[49] <- "1127"
y[86] <- "2091"

row.names(data) <- y
rm(list = "y")
#-------------------

data.pca <- prcomp(data, scale = TRUE)
#---------------------------------------------------------------

# Убрать данные со стойкостья футеровки < 2000
data.pca$x <- data.pca$x[as.numeric(row.names(data.pca$x)) > 2000,]
biplot(data.pca, 
       main = "Відображення всіх данних в координатах першої 
       та другої головних компонент")

v1 <- data.pca$rotation[,1]
v1[v1>0.333]

v2 <- data.pca$rotation[,2]
v2[v2<(-0.3)]

summary(data.pca)

plot(data.pca)

