Sort.var.data.frame <- function(data)
{
  # создание столбца дисперсий (var)
  Xvar <- scale(data,scale = F)
  Xvar <- Xvar^2
  Xvar <- apply(Xvar, 1, sum)/(length(data) - 1)
  # связивание данних
  data$Xvar <- Xvar
  # сортировка
  library(dplyr)
  data <- arrange(data,-Xvar)
  # удаление столбца дисперсий и возврат результата
  data$Xvar <- NULL
  return(data)
}


Sort.by.pca <- function(data, subsetCount) {
  data.pca <- prcomp(data, scale = TRUE)
  data.sort <- data.frame(pca = data.pca$x[,1], numbers = 1:length(data.pca$x[,1]))
  # сортировка
  library(dplyr)
  data.sort <- arrange(data.sort, pca)
  acf(data.sort$pca)
  numbers <- data.sort$numbers[seq(from = round(subsetCount / 2), to = length(data.sort$numbers), by = subsetCount)]
  return(numbers)
}

Sort.by.me <- function(data, subsetCount) {
  
  raiting <- scale(data, center = sapply(data, min), 
                scale = sapply(data, max) - sapply(data, min))
  raiting <- apply(raiting, 1, mean)
  raiting <- data.frame(raiting = raiting, numbers = 1:length(raiting))
  # сортировка
  library(dplyr)
  raiting <- arrange(raiting, raiting)
  numbers <- raiting$numbers[seq(from = round(subsetCount / 2), to = length(raiting$numbers), by = subsetCount)]
  return(numbers)
}


