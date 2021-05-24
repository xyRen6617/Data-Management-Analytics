library(tidyverse )
library(MASS)
library(corrplot)


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

index <-  sort(sample(nrow(bike_data), nrow(bike_data)*.7))
train <- bike_data[index,]
test <-  bike_data[-index,]

model1 <- glm(as.factor(Purchased.Bike)~Income + Home.Owner, data= train, family=binomial)
summary(model1)
prob<-predict(object =model1,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,"yes","no")
pred<-factor(pred,levels = c("no","yes"),order=TRUE)
f<-table(test$Purchased.Bike,pred)
f

model2 <- glm(as.factor(Purchased.Bike)~., data= train, family=binomial)
summary(model2)
prob<-predict(object =model2,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,"yes","no")
pred<-factor(pred,levels = c("no","yes"),order=TRUE)
f<-table(test$Purchased.Bike,pred)
f





model3 <- glm(as.factor(Purchased.Bike)~Marital.Status + Children+ Income  + Cars + Home.Owner + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope +`EducationHigh School` + OccupationProfessional  , data= train, family=binomial)
summary(model3)
prob<-predict(object =model3,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,"yes","no")
pred<-factor(pred,levels = c("no","yes"),order=TRUE)
f<-table(test$Purchased.Bike,pred)
f

model4 <- glm(as.factor(Purchased.Bike)~Marital.Status + Children+ Income  + Cars  + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope + OccupationProfessional  , data= train, family=binomial)
summary(model4)
prob<-predict(object =model4,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,"yes","no")
pred<-factor(pred,levels = c("no","yes"),order=TRUE)
f<-table(test$Purchased.Bike,pred)
f


library(pROC)
roc_curve <- roc(test$Purchased.Bike,prob)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+ annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2)))+ labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')


model5 = lda(Purchased.Bike~. , data = train)

model5
bike_prd = predict(model5, test)
table(bike_prd$class,test$Purchased.Bike)

summary(model5)

C<-cor(bike_data)
corrplot(C,method='number')


model5 = lda(Purchased.Bike~Marital.Status + Children+ Income  + Cars + Home.Owner + `RegionNorth America` + `Commute.Distance0-1 Miles`+`Commute.Distance2-5 Miles` + RegionEurope +`EducationHigh School` + OccupationProfessional, data = train)

model5
bike_prd = predict(model5, test)
table(bike_prd$class,test$Purchased.Bike)

summary(model5)

roc_qda <- roc(response = ctrain$diabetes, predictor = predict_qdatrain$posterior[,"1"])
plot(roc_qda, col="red", lwd=3, main="ROC curve QDA")
auc(roc_qda)roc_qda <- roc(response = ctrain$diabetes, predictor = predict_qdatrain$posterior[,"1"])
plot(roc_qda, col="red", lwd=3, main="ROC curve QDA")
auc(roc_qda)

roc_qda<-roc(response = test$Purchased.Bike,predictor = bike_prd$posterior[,1], plot=TRUE, print.thres=TRUE, print.auc=TRUE)

plot(roc_qda, col="red", lwd=3, main="ROC curve QDA")
text(x = .4, y = .4, labels = "AUC = 0.645")

auc(roc_qda)

