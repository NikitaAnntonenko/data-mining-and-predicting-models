Rand.search <- function(x.data, y, N, n.selection, n.pool)
{
  # Генерация нового фактора
  rand.vector_of_factors <- function(vect.score, N)
  {
    which.rand <- vector()
    repeat
    {
      r <- runif(N)
      which.rand <- c(which.rand,which(table(cut(r,vect.score)) != 0))
      which.rand <- unique(which.rand)
      if(length(which.rand) >= N) return(which.rand[1:N])
    }
  }
  
  # Поощерение победителей и наказание проиграфшим
  change.score <- function(vect.score, win, lost, d)
  {
    scores <- vect.score[2:length(vect.score)] - 
      vect.score[1:length(vect.score)-1]
    scores[win] <- scores[win] + d
    scores[lost] <- scores[lost] - d
    #Предохранитеть (не дает умирать факторам)
    scores[scores <= 0] <- 0.1^(10)
    #------------------------
    ans <- sapply(1:length(scores), function(i){sum(scores[1:i])})
    return(c(0,ans))
  }
  
  # Функция, что 
  
  # Подготовка к началу
  vect.score <- seq(0,1,length.out = length(x.data) + 1)
  d <- vect.score[2]/10
  #------------------------------------------------------------
  h <- 1
  while(h<=n.selection)
  {
    pool <- sapply(1:n.pool, function(i)
      {
      indiv <- rand.vector_of_factors(vect.score, N)
      score <- Brend.alg(x.data[,indiv],y)$nu
      c(score,indiv)
    })
    # разделим оценки с особями
    pool.score <- pool[1,]
    pool.indiv <- pool[2:(N+1),]
    # найдём лучшую и худшую особь
    k.min <- which.min(pool.score)
    k.max <- which.max(pool.score)
    # проведём оценивение
    vect.score <- change.score(vect.score, win = pool.indiv[,k.max],
                               lost = pool.indiv[,k.min],d)
    
    
    h <- h + 1
  }
  
  ans.score <- vect.score[2:length(vect.score)] - 
    vect.score[1:length(vect.score)-1]
  names(ans.score) <- names(x.data)
  return(sort(ans.score,T))
}


# Example № 1
vect.score <- seq(0,1,length.out = 11)
win <- c(1,2,3,4,5)
lost <- c(6,7,8,9,10)
vect.score<-change.score(vect.score,win,lost,0.07)
rand.vector_of_factors(vect.score,3)

# Example № 2
(scores<-Rand.search(x.data, y, 5, 20,40))
sum(scores)

