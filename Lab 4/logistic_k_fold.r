library(tidyverse )
library(MASS)
library(corrplot)

setwd("C:/Users/duli/Documents/20210520")

bike_data <- read.csv("bike_buyers_clean.csv")
t = model.matrix(~Commute.Distance-1,bike_data) %>% as.data.frame()
s = as.data.frame(model.matrix(~Commute.Distance-1,bike_data))
bike_data = cbind(bike_data,as.data.frame(model.matrix(~Commute.Distance-1,bike_data)))
bike_data = cbind(bike_data,as.data.frame(model.matrix(~Education-1,bike_data)))
bike_data = cbind(bike_data,as.data.frame(model.matrix(~Occupation-1,bike_data)))
bike_data = cbind(bike_data,as.data.frame(model.matrix(~Region-1,bike_data)))


bike_data <- bike_data[,-c(1)]
bike_data <- bike_data[,-c(5)]
bike_data <- bike_data[,-c(5)]
bike_data <- bike_data[,-c(7)]
bike_data <- bike_data[,-c(7)]

factors <- factor(bike_data$Marital.Status)
bike_data$Marital.Status <- as.numeric(factors)

factors <- factor(bike_data$Gender)
bike_data$Gender <- as.numeric(factors)

factors <- factor(bike_data$Home.Owner)
bike_data$Home.Owner <- as.numeric(factors)

numRows = nrow(bike_data) #there are 272 in this set 
id = seq(1, numRows, by =1)
bikeShuffle = slice(bike_data, sample(1:n()))
bikeShuffle = mutate(bikeShuffle, id)
k = 5 #5-fold validation
errors = matrix( nrow = 4, ncol = 5)
errors[1,2] = 0
View(errors)
for(j in 1:4){
  for(i in 1:5){
    errors[j,i] = 0
  } }
totalError = 0

for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model1 <- glm(as.factor(Purchased.Bike)~Marital.Status+ Children+ Income  + Cars  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[1,i] = (f[1,1] +　f[2,2])/nrow(test)
}



for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model2 <- glm(as.factor(Purchased.Bike)~Marital.Status + poly(Children,2)+ Income  + Cars  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model2)
  prob<-predict(object =model2,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[2,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model3 <- glm(as.factor(Purchased.Bike)~Marital.Status+ Children+ poly(Income,2)  + Cars  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model3)
  prob<-predict(object =model3,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[3,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model4 <- glm(as.factor(Purchased.Bike)~Marital.Status+ Children+ Income  + poly(Cars,2)  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model4)
  prob<-predict(object =model4,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[4,i] = (f[1,1] +　f[2,2])/nrow(test)
}

avgAcc = rep(0,4)
for(j in 1:4){
  for(i in 1:5){
    avgAcc[j] = avgAcc[j]+errors[j, i]
  } }






errors = matrix( nrow = 7, ncol = 5)
errors[1,2] = 0
for(j in 1:7){
  for(i in 1:5){
    errors[j,i] = 0
  } }

for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model5 <- glm(as.factor(Purchased.Bike)~Marital.Status+ Children+ Income  + Cars  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model5)
  prob<-predict(object =model5,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[1,i] = (f[1,1] +　f[2,2])/nrow(test)
}


for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model6 <- glm(as.factor(Purchased.Bike)~Marital.Status + Children+ Income  + poly(Cars,2) + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model6)
  prob<-predict(object =model6,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[2,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model7 <- glm(as.factor(Purchased.Bike)~Marital.Status+ Children+ Income  + poly(Cars,3)  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model7)
  prob<-predict(object =model7,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[3,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model8 <- glm(as.factor(Purchased.Bike)~Marital.Status+ Children+ Income  + poly(Cars,4)  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model8)
  prob<-predict(object =model8,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[4,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model9 <- glm(as.factor(Purchased.Bike)~Marital.Status + Children+ poly(Income,2)  + poly(Cars,2) + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model9)
  prob<-predict(object =model9,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[5,i] = (f[1,1] +　f[2,2])/nrow(test)
}


for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model10 <- glm(as.factor(Purchased.Bike)~Marital.Status + Children+ poly(Income,2)  + poly(Cars,3) + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model10)
  prob<-predict(object =model10,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[6,i] = (f[1,1] +　f[2,2])/nrow(test)
}


for(i in 1:k){ #the k folds for each model
  test = filter(bikeShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(bikeShuffle, test, by="id")
  
  model11 <- glm(as.factor(Purchased.Bike)~Marital.Status + Children+ poly(Income,2)  + poly(Cars,4) + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
  summary(model11)
  prob<-predict(object =model11,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,"yes","no")
  pred<-factor(pred,levels = c("no","yes"),order=TRUE)
  f<-table(test$Purchased.Bike,pred)
  
  errors[7,i] = (f[1,1] +　f[2,2])/nrow(test)
}

avgAcc = rep(0,7)
for(j in 1:7){
  for(i in 1:5){
    avgAcc[j] = avgAcc[j]+errors[j, i]
  } }




#now we confirmed that our calculated CV errors are very similar to those offered by glm
#(difference likely due to random points chosen) we can move on
#we did this so we could find Standard Error (SE) and create error bars
se = rep(0, 7)
for (i in 1:7){
  se[i] = sqrt(var(errors[i,])/k)
}
se
#now making data frame for ease of plotting 
x = seq(1,7, by = 1)
faithBest = data.frame(x,avgAcc/k , se) 
p1 <-ggplot(data = faithBest, aes(x = x, y=avgAcc.k))+ geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = avgAcc.k-se, ymax = avgAcc.k +se))+labs(x="Model Complexity")
p1
p2 <- ggplot(data = faithBest, aes(x = x, y=avgAcc.k))+ geom_point()+
  geom_line()+labs(x="Model Complexity")
p2
