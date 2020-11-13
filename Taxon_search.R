data.02 <- as.data.frame(t(x.data))

summ.1 <- kmeans(data.02,centers = 10)

sort(summ.1$cluster)

options(digits=4)

# Кластер 3
dist(rbind(summ.1$centers[3,], data.02[c(12,15),]), method =  "maximum")

# Кластер 5
dist(rbind(summ.1$centers[5,], data.02[c(9,16),]))

# Кластер 7
dist(rbind(summ.1$centers[7,], data.02[c(1,5,6,10),]))

# Кластер 9
dist(rbind(summ.1$centers[9,], data.02[c(3,7,11),]))

# Кластер 10
dist(rbind(summ.1$centers[10,], data.02[c(13,14),]))

tax.data <- select(x.data, x3,x6,x8,x9,x10,x14,x18,x22,x23,x24)
(scores<-Rand.search(tax.data, y, 5, 30,60))
sum(scores)
