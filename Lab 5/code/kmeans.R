library(ggplot2)
library(DataExplorer)
library(stats)
library(GGally)
library(factoextra)

setwd('C:/Users/duli/Documents/20210604')
cc_data = read.csv('CC GENERAL.csv')

plot_missing(cc_data)

cc_data <- na.omit(cc_data)



pairs(scale(data.frame(cc_data[,2:18])))

ss = c()

for (i in (3:18)){
  temp <- cc_data[,c(2,i)]
  m1 <- kmeans(temp,2,nstart = 16)
  print(m1$betweenss / m1$totss)
}

for (i in (2:18)){
  temp <- cc_data[,c(15,i)]
  m1 <- kmeans(temp,2,nstart = 16)
  print(m1$betweenss / m1$totss)
}
one_dim <- cc_data[,c(2,6)]

m1 <-kmeans(one_dim,2,nstart = 16)
m1
m1$centers
ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m1$cluster)))+ geom_point(size=15)+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")


one_dim <- cc_data[,c(2,18)]

m1 <- kmeans(one_dim,2,nstart = 16)
m1
m1$centers
ggplot(data=one_dim, aes(x=BALANCE, y=TENURE , colour = factor(m1$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Tenure of credit card service for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")

one_dim <- cc_data[,c(2,6)]

m3 = kmeans(one_dim, 3, nstart =100)
ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m3$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")
m4 = kmeans(one_dim, 4, nstart =100)
ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m4$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")
m5 = kmeans(one_dim, 5, nstart =100)
ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m5$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")

one_dim <- data.frame(scale(cc_data[,c(2,6)]))


m2 <- kmeans(one_dim,2,nstart = 100)

ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m2$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")


m3 = kmeans(one_dim, 3, nstart =100)
ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m3$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")
m4 = kmeans(one_dim, 4, nstart =100)
ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m4$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")
m5 = kmeans(one_dim, 5, nstart =100)
ggplot(data=one_dim, aes(x=BALANCE, y=INSTALLMENTS_PURCHASES , colour = factor(m5$cluster)))+ geom_point()+ xlab("Balance amount left in their account to make purchases")+
  ylab("Amount of purchase done in installment")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")





one_dim <- cc_data[,c(14,15)]

m3 = kmeans(one_dim, 2, nstart =100)
ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m3$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")

m3 = kmeans(one_dim, 3, nstart =100)
ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m3$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")
m4 = kmeans(one_dim, 4, nstart =100)
ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m4$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")
m5 = kmeans(one_dim, 5, nstart =100)
ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m5$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters")

one_dim <- data.frame(scale(cc_data[,c(14,15)]))


m2 <- kmeans(one_dim,2,nstart = 100)

ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m2$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters on Scaled Data")


m3 = kmeans(one_dim, 3, nstart =100)
ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m3$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters on Scaled Data")
m4 = kmeans(one_dim, 4, nstart =100)
ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m4$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters on Scaled Data")
m5 = kmeans(one_dim, 5, nstart =100)
ggplot(data=one_dim, aes(x=PAYMENTS, y=CREDIT_LIMIT , colour = factor(m5$cluster)))+ geom_point()+ xlab("PAYMENTS")+
  ylab("Limit of Credit Card for user")+ labs(colour = "Cluster Number")+ ggtitle("Exploring Clusters on Scaled Data")







one <-data.frame(cc_data[,2])
m1 <-kmeans(one,2,nstart = 16)

ggplot(data = one, aes(x = one$cc_data...2., fill = factor(m1$cluster)))+labs(colour = "Cluster Number")+
  geom_histogram()+ xlab("Balance amount left in their account to make purchases")+ ggtitle("Exploring Clusters")

m1 <-kmeans(one,3,nstart = 16)

ggplot(data = one, aes(x = one$cc_data...2., fill = factor(m1$cluster)))+labs(colour = "Cluster Number")+
  geom_histogram()+ xlab("Balance amount left in their account to make purchases")+ ggtitle("Exploring Clusters")

m1 <-kmeans(one,4,nstart = 16)

ggplot(data = one, aes(x = one$cc_data...2., fill = factor(m1$cluster)))+labs(colour = "Cluster Number")+
  geom_histogram()+ xlab("Balance amount left in their account to make purchases")+ ggtitle("Exploring Clusters")


one <-data.frame(scale(cc_data[,2]))
m1 <-kmeans(one,2,nstart = 16)

ggplot(data = one, aes(x = one$scale.cc_data...2.., fill = factor(m1$cluster)))+labs(colour = "Cluster Number")+
  geom_histogram()+ xlab("Balance amount left in their account to make purchases")+ ggtitle("Exploring Clusters on Scaled Data")


m1 <-kmeans(one,3,nstart = 16)

ggplot(data = one, aes(x = one$scale.cc_data...2.., fill = factor(m1$cluster)))+labs(colour = "Cluster Number")+
  geom_histogram()+ xlab("Balance amount left in their account to make purchases")+ ggtitle("Exploring Clusters on Scaled Data")

m1 <-kmeans(one,4,nstart = 16)

ggplot(data = one, aes(x = one$scale.cc_data...2.., fill = factor(m1$cluster)))+labs(colour = "Cluster Number")+
  geom_histogram()+ xlab("Balance amount left in their account to make purchases")+ ggtitle("Exploring Clusters on Scaled Data")



cc_pc = prcomp(scale(cc_data))


fviz_nbclust(scale(cc_data), kmeans, method = "wss", k.max = 10)

fit_km = kmeans(scale(cc_data), centers = 7)

fviz_pca_ind(cc_pc, habillage = fit_km$cluster)



one_dim <- data.frame(scale(cc_data[,c(1,5)]))

m5 = kmeans(one_dim, 5, nstart =1)
print(paste("the value of withinss when nstart = 1 :", m5$withinss))
print(paste("the value of tot.withinss when nstart = 1 :", m5$tot.withinss))

m5 = kmeans(one_dim, 5, nstart =100)
print(paste("the value of withinss when nstart = 100 :", m5$withinss))
print(paste("the value of tot.withinss when nstart = 100 :", m5$tot.withinss))

m5 = kmeans(one_dim, 5, nstart =500)
print(paste("the value of withinss when nstart = 500 :", m5$withinss))
print(paste("the value of tot.withinss when nstart = 500 :", m5$tot.withinss))




one_dim <- data.frame(scale(cc_data[,c(13,14)]))

m5 = kmeans(one_dim, 5, nstart =1)
print(paste("the value of withinss when nstart = 1 :", m5$withinss))
print(paste("the value of tot.withinss when nstart = 1 :", m5$tot.withinss))

m5 = kmeans(one_dim, 5, nstart =100)
print(paste("the value of withinss when nstart = 100 :", m5$withinss))
print(paste("the value of tot.withinss when nstart = 100 :", m5$tot.withinss))

m5 = kmeans(one_dim, 5, nstart =500)
print(paste("the value of withinss when nstart = 500 :", m5$withinss))
print(paste("the value of tot.withinss when nstart = 500 :", m5$tot.withinss))
