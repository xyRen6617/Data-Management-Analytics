library(ggplot2)
library(stats)





setwd('C:/Users/duli/Documents/20210604')
cc_data = read.csv('CC GENERAL.csv')

cc_data <- na.omit(cc_data)

one_dim <- cc_data[,c(2,6)]


m2h = hclust(dist(one_dim), method="complete")
plot(m2h)

plot(one_dim$BALANCE~one_dim$INSTALLMENTS_PURCHASES, col=(cutree(m2h, k=2)))

plot(one_dim$BALANCE~one_dim$INSTALLMENTS_PURCHASES, col=(cutree(m2h, k=3)))

plot(one_dim$BALANCE~one_dim$INSTALLMENTS_PURCHASES, col=(cutree(m2h, k=4)))


m2h = hclust(dist(cc_data), method="complete")
plot(m2h)

num = 2

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$BALANCE)

plot(cutree(m2h, k=num),cc_data$BALANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$PURCHASES)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$INSTALLMENTS_PURCHASES)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE)

plot(cutree(m2h, k=num),cc_data$PURCHASES_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES_FREQUENCY)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_INSTALLMENTS_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_TRX)

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

plot(cutree(m2h, k=num),cc_data$CREDIT_LIMIT)

plot(cutree(m2h, k=num),cc_data$PAYMENTS)

plot(cutree(m2h, k=num),cc_data$MINIMUM_PAYMENTS)

par(mfrow=c(1,2))

plot(cutree(m2h, k=num),cc_data$PRC_FULL_PAYMENT)

plot(cutree(m2h, k=num),cc_data$TENURE)



num = 3

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$BALANCE)

plot(cutree(m2h, k=num),cc_data$BALANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$PURCHASES)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$INSTALLMENTS_PURCHASES)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE)

plot(cutree(m2h, k=num),cc_data$PURCHASES_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES_FREQUENCY)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_INSTALLMENTS_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_TRX)

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

plot(cutree(m2h, k=num),cc_data$CREDIT_LIMIT)

plot(cutree(m2h, k=num),cc_data$PAYMENTS)

plot(cutree(m2h, k=num),cc_data$MINIMUM_PAYMENTS)

par(mfrow=c(1,2))

plot(cutree(m2h, k=num),cc_data$PRC_FULL_PAYMENT)

plot(cutree(m2h, k=num),cc_data$TENURE)


m2h = hclust(dist(data.frame(scale(cc_data[,2:18]))), method="complete")


num = 2

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$BALANCE)

plot(cutree(m2h, k=num),cc_data$BALANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$PURCHASES)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$INSTALLMENTS_PURCHASES)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE)

plot(cutree(m2h, k=num),cc_data$PURCHASES_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES_FREQUENCY)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_INSTALLMENTS_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_TRX)

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

plot(cutree(m2h, k=num),cc_data$CREDIT_LIMIT)

plot(cutree(m2h, k=num),cc_data$PAYMENTS)

plot(cutree(m2h, k=num),cc_data$MINIMUM_PAYMENTS)

par(mfrow=c(1,2))

plot(cutree(m2h, k=num),cc_data$PRC_FULL_PAYMENT)

plot(cutree(m2h, k=num),cc_data$TENURE)


num = 3

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$BALANCE)

plot(cutree(m2h, k=num),cc_data$BALANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$PURCHASES)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$INSTALLMENTS_PURCHASES)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE)

plot(cutree(m2h, k=num),cc_data$PURCHASES_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES_FREQUENCY)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_INSTALLMENTS_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_FREQUENCY)

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE_TRX)

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$PURCHASES_TRX)

plot(cutree(m2h, k=num),cc_data$CREDIT_LIMIT)

plot(cutree(m2h, k=num),cc_data$PAYMENTS)

plot(cutree(m2h, k=num),cc_data$MINIMUM_PAYMENTS)

par(mfrow=c(1,2))

plot(cutree(m2h, k=num),cc_data$PRC_FULL_PAYMENT)

plot(cutree(m2h, k=num),cc_data$TENURE)


one_dim_scaled <-data.frame(scale(cc_data[,c(2,6)]))

m2h = hclust(dist(one_dim_scaled), method="complete")
plot(m2h)

plot(one_dim_scaled$BALANCE~one_dim$INSTALLMENTS_PURCHASES, col=(cutree(m2h, k=2)))

plot(one_dim_scaled$BALANCE~one_dim$INSTALLMENTS_PURCHASES, col=(cutree(m2h, k=3)))

plot(one_dim_scaled$BALANCE~one_dim$INSTALLMENTS_PURCHASES, col=(cutree(m2h, k=4)))


plot(cutree(m2h, k=3),one_dim_scaled$BALANCE)





cc_data <- cc_data[,c(-1)]
cc_data <- data.frame(scale(cc_data))

m2h = hclust(dist(cc_data), method="complete")


num = 5

#par(mfrow=c(2,2))

plot(cutree(m2h, k=num),cc_data$BALANCE,ylab="BALANCE")

plot(cutree(m2h, k=num),cc_data$ONEOFF_PURCHASES,ylab="ONEOFF_PURCHASES")

plot(cutree(m2h, k=num),cc_data$INSTALLMENTS_PURCHASES,ylab="INSTALLMENTS_PURCHASES")

plot(cutree(m2h, k=num),cc_data$CASH_ADVANCE,ylab="CASH_ADVANCE")




#par(mfrow=c(2,2))



plot(cutree(m2h, k=num),cc_data$CREDIT_LIMIT,ylab="CREDIT_LIMIT")

plot(cutree(m2h, k=num),cc_data$PAYMENTS,ylab="PAYMENTS")

plot(cutree(m2h, k=num),cc_data$MINIMUM_PAYMENTS,ylab="MINIMUM_PAYMENTS")




