rm(list = ls())

options(scipen=100, digits=4)

library("ggplot2")
library("stringr")
library("psych")
library("dplyr")

data<-read.table("data.txt",F,'\t',dec = ',')
names(data) <- paste0("X",1:24)
names(data)[25] <- "Y"

data$x23 <- as.numeric(data$x23)
data$x24 <- as.numeric(data$x24)

data$x15 <- NULL
data$x20 <- NULL
data$x2 <- NULL
data$x5 <- NULL
data$x19 <- NULL
data$x17 <- NULL


data <- norm01(data,F)

x.data <- data[,1:20]
y <- data[,21]

sdata <- data
rownames(sdata) <- NULL
write.table(sdata,file = "data3.txt",row.names = F,dec = ",",sep = "\t")
