library(ggplot2)
library(stats)

setwd('C:/Users/duli/Documents/20210604')
cc_data = read.csv('CC GENERAL.csv')

cc_data <- na.omit(cc_data)
cc_data<- cc_data[,c(-1)]

cc_data <- data.frame(scale(cc_data))




one_dim <- cc_data[,c(1,5)]
 
m2h = hclust(dist(cc_data), method="complete")
plot(m2h)

plot(one_dim$INSTALLMENTS_PURCHASES~cc_data$BALANCE, col=(cutree(m2h, k=2)),xlab="BALANCE",ylab="INSTALLMENTS_PURCHASES")

plot(one_dim$INSTALLMENTS_PURCHASES~one_dim$BALANCE, col=(cutree(m2h, k=3)),xlab="BALANCE",ylab="INSTALLMENTS_PURCHASES")

plot(one_dim$INSTALLMENTS_PURCHASES~one_dim$BALANCE, col=(cutree(m2h, k=4)),xlab="BALANCE",ylab="INSTALLMENTS_PURCHASES")

plot(one_dim$INSTALLMENTS_PURCHASES~one_dim$BALANCE, col=(cutree(m2h, k=5)),xlab="BALANCE",ylab="INSTALLMENTS_PURCHASES")


one_dim <- data.frame(scale(cc_data[,c(5,14)]))

m2h = hclust(dist(one_dim), method="complete")
plot(m2h)

plot(one_dim$CREDIT_LIMIT~one_dim$PAYMENTS, col=(cutree(m2h, k=2)),xlab="PAYMENTS",ylab="CREDIT_LIMIT")

plot(one_dim$CREDIT_LIMIT~one_dim$PAYMENTS, col=(cutree(m2h, k=3)),xlab="PAYMENTS",ylab="CREDIT_LIMIT")

plot(one_dim$CREDIT_LIMIT~one_dim$PAYMENTS, col=(cutree(m2h, k=4)),xlab="PAYMENTS",ylab="CREDIT_LIMIT")

plot(one_dim$CREDIT_LIMIT~one_dim$PAYMENTS, col=(cutree(m2h, k=5)),xlab="PAYMENTS",ylab="CREDIT_LIMIT")


data <- data.frame(scale(cc_data[,c(1,4,5,6,13,14,15)]))

