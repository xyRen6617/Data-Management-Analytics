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

model1_data_select <- subset(model1_data, select = -c(team,cards))



numRows = nrow(model1_data_select) 
id = seq(1, numRows, by =1)
model1_Shuffle = slice(model1_data_select, sample(1:n()))
model1_Shuffle = mutate(model1_Shuffle, id)
k = 5 #5-fold validation

num_model = 5

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
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches,data=train)
  errors[1,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  f = 1
  coef(best_train,f)
  
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners,data=train)
  errors[2,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  f = 2
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}

for(i in 1:k){ #the k folds for each model
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls,data=train)
  errors[3,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  f = 3
  
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls+shots,data=train)
  errors[4,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  f = 4
  
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}


for(i in 1:k){ #the k folds for each model
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls+shots+passes,data=train)
  errors[5,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  f = 5
  
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
  test = filter(model1_Shuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
  train = anti_join(model1_Shuffle, test, by="id")
  model = glm(possession~touches + corners+fouls+shots+passes+clearances,data=train)
  errors[7,i] = mean((test$possession - predict.lm(model, test))^2)
  best_train = regsubsets(possession~., data = train)
  f = 7
  
  coef(best_train,f)
  cps[f,i] = summary(best_train)$cp[f]
  bics[f,i] = summary(best_train)$bic[f]
  adjr2s[f,i] = summary(best_train)$adjr2[f]
  
}