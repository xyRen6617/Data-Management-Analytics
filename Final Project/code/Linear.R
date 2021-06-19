library(corrplot)
library(ggplot2)
library(leaps)
library(boot)
library(dplyr)


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


model1_data = rbind(man_united_data[,c(117,119,115,133,135,123,125,127,129,137)],man_city_data[,c(117,119,115,133,135,123,125,127,129,137)],chelsea_data[,c(117,119,115,133,135,123,125,127,129,137)],arsenal_data[,c(117,119,115,133,135,123,125,127,129,137)])

fit <- lm(possession~touches + passes + shots + shots_on_target + fouls + cards + corners + clearances,data=model1_data)

sum_fit = summary(fit)

sum_fit$r.squared
sum_fit$adj.r.squared

model1_data_select <- subset(model1_data, select = -c(team,cards))







best = regsubsets(possession~., data = model1_data_select)

plot(best,scale = "adjr2")
plot(best,scale = "r2")
plot(best,scale = "bic")
plot(best,scale = "Cp")



summary(best)

summary(best)$rsq


coef(best,1)
summary(best)$adjr2

summary(best)$cp



x = seq(1, num_model, by=1)
y = summary(best)$adjr2
adjr2Res = data.frame(x, y)
p1 <-ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()+xlab("model complexity")+ylab("adjr2")+ ggtitle("adjr2 value with model complexity")+theme(plot.title = element_text(hjust = 0.5))

ggsave("adjr2.png",p1,width = 12,height = 9)


y = summary(best)$cp
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()+xlab("model complexity")+ylab("cp")+ ggtitle("cp value with model complexity")+theme(plot.title = element_text(hjust = 0.5))

ggsave("cp.png",p1,width = 12,height = 9)

y = summary(best)$bic
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()+xlab("model complexity")+ylab("bic")+ ggtitle("bic value with model complexity")+theme(plot.title = element_text(hjust = 0.5))

ggsave("bic.png",p1,width = 12,height = 9)


y = summary(best)$rsq
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()+xlab("model complexity")+ylab("rsq")+ ggtitle("rsq value with model complexity")+theme(plot.title = element_text(hjust = 0.5))

ggsave("rsq.png",p1,width = 12,height = 9)

coef(best,1)
coef(best,2)
coef(best,3)
coef(best,4)
coef(best,5)
coef(best,6)
coef(best,7)





















#CV
numRows = nrow(model1_data_select) 
id = seq(1, numRows, by =1)
model1_Shuffle = slice(model1_data_select, sample(1:n()))
model1_Shuffle = mutate(model1_Shuffle, id)
k = 5 #5-fold validation

num_model = 10

errors = matrix( nrow = num_model, ncol = 5)
cps = matrix( nrow = num_model, ncol = 5)
bics = matrix( nrow = num_model, ncol = 5)
aics = matrix( nrow = num_model, ncol = 5)
adjr2s = matrix( nrow = num_model, ncol = 5)

cp_ = rep(0,num_model)
bic_ = rep(0,num_model)
r2_ = rep(0,num_model) 

errors[1,2] = 0
cps[1,1] = 0
bics[1,1] = 0
adjr2s[1,1] = 0

for(i in 1:5){
  errors[i] = 0
} 
totalError = 0

for(i in 1:k){ #the k folds for each model
  f = 1
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = lm(possession~touches,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)

  coef(best_train,f)

  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}


for(i in 1:k){ #the k folds for each model
  f = 2
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}

for(i in 1:k){ #the k folds for each model
  f = 3
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls,data=train)
  errors[3,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}


for(i in 1:k){ #the k folds for each model
  f = 4
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls+shots,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}


for(i in 1:k){ #the k folds for each model
  f = 5
  
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls+shots+passes,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}

for(i in 1:k){ #the k folds for each model
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls+shots+passes+clearances,data=train)
  errors[6,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  f = 6
  
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}



for(i in 1:k){ #the k folds for each model
  f = 7
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~poly(touches,2) + corners+fouls+shots+passes+clearances,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)

}

for(i in 1:k){ #the k folds for each model
  f = 8
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = lm(possession~poly(touches,3) + corners+fouls+shots+passes+clearances,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)

  
}

for(i in 1:k){ #the k folds for each model
  f = 9
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~poly(touches,4) + corners+fouls+shots+passes+clearances,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  

}

for(i in 1:k){ #the k folds for each model
  f = 10
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~poly(touches,5) + corners+fouls+shots+passes+clearances,data=train)
  errors[f,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  

  
}

avgRegEr = rep(0,num_model)
avgCp = rep(0, num_model)
avgBic = rep(0, num_model)
avgR2 = rep(0, num_model)





for(j in 1:num_model){
  for(i in 1:5){
    avgRegEr[j] = avgRegEr[j]+errors[j, i]
  } }

for(j in 1:num_model){
  for(i in 1:5){
    avgBic[j] = avgBic[j]+bics[j, i]
  } }

for(j in 1:num_model){
  for(i in 1:5){
    avgR2[j] = avgR2[j]+adjr2s[j, i]
  } }

for(j in 1:num_model){
  for(i in 1:5){
    avgCp[j] = avgCp[j]+cps[j, i]
  } }


x = seq(1, num_model, by=1)
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


y1 = summary(best)$adjr2[1:num_model]



y2 = summary(best)$cp[1:num_model]


y3 = summary(best)$bic[1:num_model]

data1 = data.frame(x,y1, avgR2/k)

library(tidyverse)
ggplot()+geom_point(aes(x=x, y=y1), col="red")+ geom_point(aes(x=x, y=avgR2/k), col="blue")+ ylab("adjr2")+labs(x="Model Complexity")


ggplot()+geom_point(aes(x=x, y=y3), col="red")+ geom_point(aes(x=x, y=avgBic/k), col="blue")+ ylab("bic")+labs(x="Model Complexity")


ggplot()+geom_point(aes(x=x, y=y2), col="red")+ geom_point(aes(x=x, y=avgCp/k), col="blue")+ ylab("cp")+labs(x="Model Complexity")

#now we confirmed that our calculated CV errors are very similar to those offered by glm
#(difference likely due to random points chosen) we can move on
#we did this so we could find Standard Error (SE) and create error bars
se = rep(0, num_model)
for (i in 1:num_model){
  se[i] = sqrt(var(errors[i,])/k)
}
se
#now making data frame for ease of plotting
x = seq(1,num_model, by = 1)
faithBest = data.frame(x,avgRegEr/k , se)
faithBest
p1 <-ggplot(data = faithBest, aes(x = x, y=avgRegEr.k))+ geom_point()+
  geom_line()+geom_errorbar(aes(ymin = avgRegEr.k-se, ymax = avgRegEr.k +se))+labs(x="Model Complexity")+ggtitle("avgRegErk with model complexity")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave("avgReg.png",p1,width = 12,height = 9)



p2 <- ggplot(data = faithBest, aes(x = x, y=avgRegEr.k))+ geom_line()+ggtitle("avgRegErk with model complexity")+theme(plot.title = element_text(hjust = 0.5))+labs(x="Model Complexity")+geom_point()
p2

ggsave("avgReg1.png",p2,width = 12,height = 9)



x = seq(1,num_model, by = 1)
faithBest = data.frame(x,avgCp/k , se)
faithBest
p1 <-ggplot(data = faithBest, aes(x = x, y=avgCp.k))+ geom_point()+
  geom_line()+geom_errorbar(aes(ymin = avgCp.k-se, ymax = avgCp.k +se))+labs(x="Model Complexity")+ggtitle("avgCp with model complexity")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave("avgCp.png",p1,width = 12,height = 9)



p2 <- ggplot(data = faithBest, aes(x = x, y=avgCp.k))+ geom_line()+ggtitle("avgCp with model complexity")+theme(plot.title = element_text(hjust = 0.5))+labs(x="Model Complexity")+geom_point()
p2

ggsave("avgCp1.png",p2,width = 12,height = 9)

library(corrplot)
corrplot.mixed(model1_data_select)
