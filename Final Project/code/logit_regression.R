library(ggplot2)
library(DataExplorer)
library(stats)
library(GGally)



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

index <-  sort(sample(nrow(model1_data), nrow(model1_data)*.7))
train <- model1_data[index,]
test <-  model1_data[-index,]

model1 <- glm(result~possession + touches + passes + shots + shots_on_target + fouls + cards + corners + clearances, data= train, family=binomial,maxit=250)
summary(model1)
prob<-predict(object =model1,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,1,0)
pred<-factor(pred,levels = c(0,1),order=TRUE)
f<-table(test$result,pred)
f

model2 <- glm(result~possession + touches + passes  + shots_on_target  + cards  + clearances, data= train, family=binomial,maxit=250)
summary(model2)
prob<-predict(object =model2,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,1,0)
pred<-factor(pred,levels = c(0,1),order=TRUE)
f<-table(test$result,pred)
f


library(pROC)
roc_curve <- roc(test$result,prob)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
p1 = p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+ annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2)))+ labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')+theme(plot.title = element_text(hjust = 0.5))

ggsave("ROC.png",p1,width = 12,height = 9)




