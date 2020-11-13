box_counting.matrix <- function(xdata,y, eps = 0.01){
  # Нормализация
  xdata<-scale(xdata)
  y<-scale(y)
  # Расчитаем значение каждого столбца xdata и y
  ans<-apply(xdata,2,
             
             function(x) {
               # Расчёт Nxy
               # index - это маска для y
               index<-tapply(1:length(x), cut(x,seq(0,1,by = eps)),function(x)x)
               matrix_hit <- sapply(index, 
                                    function(x) {
                                      as.numeric(table(cut(y[x],seq(0,1,by = eps))) != 0)
                                    })
               Nxy <- sum(matrix_hit)
               # Расчёт Nx
               Nx <- sum(as.numeric(colSums(matrix_hit) != 0))
               # Расчёт Ny
               Ny <- sum(as.numeric(rowSums(matrix_hit) != 0))
               # Ответ
               return(log( (Nx*Ny) / Nxy,2))
             }
             
  )
  return(sort(ans,decreasing = T))
}