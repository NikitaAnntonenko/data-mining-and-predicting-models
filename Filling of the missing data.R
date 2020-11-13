rm(list = ls())

data(sleep,package = "VIM")

#список строк, в которых нет пропусков
sleep[complete.cases(sleep),]
#список строк, в которых хотя бы одно пропущенное значение
sleep[!complete.cases(sleep),]

library(mice)

#матрица пропусков
#первый столбец ко-во таких случаев, последний ко-во пропусков в случае
#нижняя строка - это ко-во пропусков у каждого свойства
md.pattern(sleep)

#график пропусков
library(VIM)
matrixplot(sleep)

#проверим коеффициент кореляции у между пропусками
x<-as.data.frame(abs(is.na(sleep)))
y<-x[,which(colSums(x)>0)]
print(cor(y),4) #Как??? plot(y)

#???
cor(sleep,y,use = "pairwise.complete.obs")


#Процедура Гиббcа
imp <- mice(sleep,seed = 1234)
head(complete(imp))
head(sleep)
