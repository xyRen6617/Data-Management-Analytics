library(boot)
library(ggplot2)
library(leaps)
library(dplyr)

setwd("C:/Users/duli/Documents/20210530")
admission <- read.csv("Admission_Predict.csv")
admission_data<-admission[,-c(1)]
numRows = nrow(admission) 
id = seq(1, numRows, by =1)
admissionShuffle = slice(admission, sample(1:n()))
admissionShuffle = mutate(admissionShuffle, id)
k = 5 #5-fold validation


errors = matrix( nrow = 7, ncol = 5)
cps = matrix( nrow = 7, ncol = 5)
bics = matrix( nrow = 7, ncol = 5)
aics = matrix( nrow = 7, ncol = 5)
adjr2s = matrix( nrow = 7, ncol = 5)

cp_ = rep(0,7)
bic_ = rep(0,7)
r2_ = rep(0,7) 

errors[1,2] = 0
cps[1,1] = 0
bics[1,1] = 0
adjr2s[1,1] = 0
View(errors)
  for(i in 1:5){
    errors[i] = 0
  } 
totalError = 0

best = regsubsets(Chance.of.Admit~., data = admission)


for(i in 1:k){ #the k folds for each model
  test = filter(admissionShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(admissionShuffle, test, by="id")
  model = lm(Chance.of.Admit~ CGPA, data=train)
  errors[1,i] = mean((test$Chance.of.Admit - predict.lm(model, test))^2)
  best_train = regsubsets(Chance.of.Admit~., data = train)
  
  cps[1,i] = summary(best_train)$cp[1]
  bics[1,i] = summary(best_train)$bic[1]
  adjr2s[1,i] = summary(best_train)$adjr2[1]
  
}





for(i in 1:k){ #the k folds for each model
  test = filter(admissionShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(admissionShuffle, test, by="id")
  model = lm(Chance.of.Admit~CGPA + GRE.Score , data=train)
  errors[2,i] = mean((test$Chance.of.Admit - predict.lm(model, test))^2)
  best_train = regsubsets(Chance.of.Admit~CGPA + GRE.Score, data = train)
  
  cps[2,i] = summary(best_train)$cp[2]
  bics[2,i] = summary(best_train)$bic[2]
  adjr2s[2,i] = summary(best_train)$adjr2[2]
  }


for(i in 1:k){ #the k folds for each model
    test = filter(admissionShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
    train = anti_join(admissionShuffle, test, by="id")
    model = lm(Chance.of.Admit~CGPA + LOR + GRE.Score, data=train)
    errors[3,i] = mean((test$Chance.of.Admit - predict.lm(model, test))^2)
    best_train = regsubsets(Chance.of.Admit~CGPA + LOR + GRE.Score, data = train)
    f = 3
    cps[f,i] = summary(best_train)$cp[f]
    bics[f,i] = summary(best_train)$bic[f]
    adjr2s[f,i] = summary(best_train)$adjr2[f]
}



for(i in 1:k){ #the k folds for each model
  test = filter(admissionShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(admissionShuffle, test, by="id")
  model = lm(Chance.of.Admit~CGPA + LOR + GRE.Score + Research, data=train)
  errors[4,i] = mean((test$Chance.of.Admit - predict.lm(model, test))^2)
  best_train = regsubsets(Chance.of.Admit~CGPA + LOR + GRE.Score + Research, data = train)
  f = 4
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
}


for(i in 1:k){ #the k folds for each model
  test = filter(admissionShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(admissionShuffle, test, by="id")
  model = lm(Chance.of.Admit~TOEFL.Score + CGPA + LOR + GRE.Score + Research, data=train)
  errors[5,i] = mean((test$Chance.of.Admit - predict.lm(model, test))^2)
  best_train = regsubsets(Chance.of.Admit~TOEFL.Score + CGPA + LOR + GRE.Score + Research, data = train)
  f = 5
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
}



for(i in 1:k){ #the k folds for each model
  test = filter(admissionShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(admissionShuffle, test, by="id")
  model = lm(Chance.of.Admit~TOEFL.Score + CGPA + LOR + GRE.Score + Research+ University.Rating 
             , data=train)
  errors[6,i] = mean((test$Chance.of.Admit - predict.lm(model, test))^2)
  best_train = regsubsets(Chance.of.Admit~TOEFL.Score + CGPA + LOR + GRE.Score + Research+ University.Rating, data = train)
  f = 6
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
}



for(i in 1:k){ #the k folds for each model
  test = filter(admissionShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(admissionShuffle, test, by="id")
  model = lm(Chance.of.Admit~CGPA + TOEFL.Score + GRE.Score + University.Rating 
             + SOP + LOR + Research, data=train)
  errors[7,i] = mean((test$Chance.of.Admit - predict.lm(model, test))^2)
  best_train = regsubsets(Chance.of.Admit~CGPA + TOEFL.Score + GRE.Score + University.Rating 
                          + SOP + LOR + Research, data = train)
  f = 7
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
}


#find average for each model to see how compares with cv.error from glm



avgRegEr = rep(0,7)
avgCp = rep(0, 7)
avgBic = rep(0, 7)
avgR2 = rep(0, 7)





for(j in 1:7){
  for(i in 1:5){
    avgRegEr[j] = avgRegEr[j]+error[j, i]
  } }

for(j in 1:7){
  for(i in 1:5){
    avgBic[j] = avgBic[j]+bics[j, i]
  } }

for(j in 1:7){
  for(i in 1:5){
    avgR2[j] = avgR2[j]+adjr2s[j, i]
  } }

for(j in 1:7){
  for(i in 1:5){
    avgCp[j] = avgCp[j]+cps[j, i]
  } }

x = seq(1, 7, by=1)
y = avgCp/k
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()

ggsave("cp.png",p1,width = 12,height = 9)

y = avgBic/k
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()

ggsave("bic.png",p1,width = 12,height = 9)


y = avgR2/k
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()

ggsave("adjr2.png",p1,width = 12,height = 9)


y1 = summary(best)$adjr2[1:7]



y2 = summary(best)$cp[1:7]


y3 = summary(best)$bic[1:7]

data1 = data.frame(x,y1, avgR2/k)

library(tidyverse)
ggplot()+geom_point(aes(x=x, y=y1), col="red")+ geom_point(aes(x=x, y=avgR2/k), col="blue")+ ylab("adjr2")+labs(x="Model Complexity")


ggplot()+geom_point(aes(x=x, y=y3), col="red")+ geom_point(aes(x=x, y=avgBic/k), col="blue")+ ylab("bic")+labs(x="Model Complexity")


ggplot()+geom_point(aes(x=x, y=y2), col="red")+ geom_point(aes(x=x, y=avgCp/k), col="blue")+ ylab("cp")+labs(x="Model Complexity")

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
faithBest = data.frame(x,avgRegEr/k , se)
faithBest
p1 <-ggplot(data = faithBest, aes(x = x, y=avgRegEr.k))+ geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = avgRegEr.k-se, ymax = avgRegEr.k +se))+labs(x="Model Complexity")
p1
p2 <- ggplot(data = faithBest, aes(x = x, y=avgRegEr.k))+ geom_point()+
  geom_line()+labs(x="Model Complexity")
p2

