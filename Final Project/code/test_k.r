library(ggplot2)
library(DataExplorer)
library(stats)
library(GGally)
library(tidyverse )
library(MASS)
library(corrplot)



#model1 possession VS others



man_united_data$team = "man_united"
names(man_united_data)[117] = "touches"
names(man_united_data)[119] = "passes"
names(man_united_data)[115] = "possession"
names(man_united_data)[133] = "shots"
names(man_united_data)[135] = "shots_on_target"
names(man_united_data)[123] = "fouls"
names(man_united_data)[125] = "cards"
names(man_united_data)[127] = "clearances"
names(man_united_data)[129] = "corners"
names(man_united_data)[131] = "sg"


man_city_data$team = "man_city"
names(man_city_data)[117] = "touches"
names(man_city_data)[119] = "passes"
names(man_city_data)[115] = "possession"
names(man_city_data)[133] = "shots"
names(man_city_data)[135] = "shots_on_target"
names(man_city_data)[123] = "fouls"
names(man_city_data)[125] = "cards"
names(man_city_data)[127] = "clearances"
names(man_city_data)[129] = "corners"
names(man_city_data)[131] = "sg"


chelsea_data$team = "chelsea"
names(chelsea_data)[117] = "touches"
names(chelsea_data)[119] = "passes"
names(chelsea_data)[115] = "possession"
names(chelsea_data)[133] = "shots"
names(chelsea_data)[135] = "shots_on_target"
names(chelsea_data)[123] = "fouls"
names(chelsea_data)[125] = "cards"
names(chelsea_data)[127] = "clearances"
names(chelsea_data)[129] = "corners"
names(chelsea_data)[131] = "sg"


arsenal_data$team = "arsenal"
names(arsenal_data)[117] = "touches"
names(arsenal_data)[119] = "passes"
names(arsenal_data)[115] = "possession"
names(arsenal_data)[133] = "shots"
names(arsenal_data)[135] = "shots_on_target"
names(arsenal_data)[123] = "fouls"
names(arsenal_data)[125] = "cards"
names(arsenal_data)[127] = "clearances"
names(arsenal_data)[129] = "corners"
names(arsenal_data)[131] = "sg"



model1_data = rbind(man_united_data[,c(117,119,115,133,135,123,125,127,129,137,131)],man_city_data[,c(117,119,115,133,135,123,125,127,129,137,131)],chelsea_data[,c(117,119,115,133,135,123,125,127,129,137,131)],arsenal_data[,c(117,119,115,133,135,123,125,127,129,137,131)])

model1_data$result = 0

model1_data$res


model1_data <- subset(model1_data, select = -c(team))

model1_data$result[model1_data$sg >= 0] <- 1

num_model = 9

numRows = nrow(model1_data) #there are 272 in this set 
id = seq(1, numRows, by =1)
model1Shuffle = slice(model1_data, sample(1:n()))
model1Shuffle = mutate(model1Shuffle, id)
k = 5 #5-fold validation
errors = matrix( nrow = num_model, ncol = 5)
errors[1,2] = 0






for(j in 1:num_model){
  for(i in 1:5){
    errors[j,i] = 0
  } }

test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
train = anti_join(model1Shuffle, test, by="id")

for(i in 1:k){ #the k folds for each model
  
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~passes  + shots_on_target + clearances, data= train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[1,i] = (f[1,1] +　f[2,2])/nrow(test)
  
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~touches + passes  + shots_on_target + clearances, data= train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[2,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~possession + touches + passes  + shots_on_target + clearances, data= train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.9,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  print(f)
  
  errors[3,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~possession + touches + passes  + shots_on_target  + cards  + clearances, data= train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[4,i] = (f[1,1] +　f[2,2])/nrow(test)
}

for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~ poly (possession, 2) + touches + passes  + shots_on_target + cards  + clearances, data= train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[5,i] = (f[1,1] +　f[2,2])/nrow(test)
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~ poly (possession, 3) + touches + passes  + shots_on_target + cards  + clearances, data= train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[6,i] = (f[1,1] +　f[2,2])/nrow(test)
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~ poly (possession, 4) + touches + passes  + poly (shots_on_target, 2) + cards  + poly(clearances, 2),data=train,family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[7,i] = (f[1,1] +　f[2,2])/nrow(test)
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~ poly (possession, 4) + touches + passes  + poly (shots_on_target, 2) + cards  + clearances, data= train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[8,i] = (f[1,1] +　f[2,2])/nrow(test)
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1Shuffle, test, by="id")
  model1 <- glm(result~ poly (possession, 4) + touches + passes  + poly (shots_on_target, 2) + cards  + poly(clearances, 2),data=train, family=binomial,maxit=250)
  summary(model1)
  prob<-predict(object =model1,newdata=test,type = "response")
  pred<-ifelse(prob>=0.5,1,0)
  pred<-factor(pred,levels = c(0,1),order=TRUE)
  f<-table(test$result,pred)
  
  errors[9,i] = (f[1,1] +　f[2,2])/nrow(test)
}



avgAcc = rep(0,num_model)
for(j in 1:num_model){
  for(i in 1:5){
    avgAcc[j] = avgAcc[j]+errors[j, i]
  } }




#now we confirmed that our calculated CV errors are very similar to those offered by glm
#(difference likely due to random points chosen) we can move on
#we did this so we could find Standard Error (SE) and create error bars
se = rep(0, num_model)
for (i in 1:num_model){
  se[i] = sqrt(var(errors[i,])/k)
}
se
se
#now making data frame for ease of plotting 
x = seq(1,num_model, by = 1)
faithBest = data.frame(x,avgAcc/k , se) 
p1 <-ggplot(data = faithBest, aes(x = x, y=avgAcc.k))+ geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = avgAcc.k-se, ymax = avgAcc.k +se))+labs(x="Model Complexity")+ggtitle("avgAcc with model complexity")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave("avgAcc_with_se.png",p1,width = 12,height = 9)

p2 <- ggplot(data = faithBest, aes(x = x, y=avgAcc.k))+ geom_point()+
  geom_line()+labs(x="Model Complexity")+ggtitle("avgAcc with model complexity")+theme(plot.title = element_text(hjust = 0.5))
p2
ggsave("avgAcc.png",p2,width = 12,height = 9)

faithBest
