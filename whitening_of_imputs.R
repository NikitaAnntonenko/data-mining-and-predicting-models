
whitening_of_imputs<-function(data, convert = T){
  if(convert == T){
    name <- names(data)
    # Вычисление ковариационной матрицы
    y <- cov(data)
    # v_and_d$vectors –матрица, состоящая из собственных вектров; 
    #v_and_d$values –вектор собственных чисел
    v_and_d <- eigen(y);
    # Вычисляем матрицу Xn
    dmean = apply(data,2,mean)
    data <- scale(data, scale = F)
    # Вычисляем матрицу ~X
    data <- data%*%v_and_d$vectors
    data <- t(apply(data, 1, function(x) x*v_and_d$values^(-1/2)))
    data <- as.data.frame(data)
    names(data) <- name
    return(list(data = data,v_and_d = v_and_d,dmean = dmean))
  }
  else{
    ans <- t(apply(data$data,1,function(x) x*data$v_and_d$values^(1/2)))
    ans <- ans%*%ginv(data$v_and_d$vectors)
    ans <- t(apply(ans,1,function(x) x+data$dmean))
    return(ans)
  }
}