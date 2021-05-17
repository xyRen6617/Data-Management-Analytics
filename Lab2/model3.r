install.packages("leaps")
library(leaps)
library(ggplot2)
library(ISLR)
library(tidyverse)

setwd("C:/Users/duli/Documents/20210514")

admission_data <- read.csv("Admission_Predict.csv")

admission_data<-admission_data[,-c(1)]

admission_data_1_1 <- read.csv("Admission_Predict_Ver1.1.csv")

ggplot(admission_data, aes(CGPA, Chance.of.Admit)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red", method = "lm") +
  scale_y_continuous(breaks = seq(0,1, by = 0.05)) +
  scale_x_continuous(breaks = seq(5,10, by = 0.5)) +
  xlab("Cumulative Grade Point Average") +
  ylab("Probabilty of Admission") +
  ggtitle("Probabilty of Admission vs Cumulative Grade Point Average")

best = regsubsets(Chance.of.Admit~., data = admission_data)

plot(best,scale = "adjr2")
plot(best,scale = "r2")
plot(best,scale = "bic")
plot(best,scale = "Cp")



summary(best)

summary(best)$rsq


coef(best,1)
summary(best)$adjr2

summary(best)$cp



x = seq(1, 7, by=1)
y = summary(best)$adjr2
adjr2Res = data.frame(x, y)
p1 <-ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()

ggsave("adjr2.png",p1,width = 12,height = 9)


y = summary(best)$cp
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()

ggsave("cp.png",p1,width = 12,height = 9)

y = summary(best)$bic
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()

ggsave("bic.png",p1,width = 12,height = 9)


y = summary(best)$rsq
adjr2Res = data.frame(x, y)
p1 <- ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()

ggsave("rsq.png",p1,width = 12,height = 9)

coef(best,1)
coef(best,2)
coef(best,3)
coef(best,4)
coef(best,5)


forward = regsubsets(Chance.of.Admit~., method="forward", data=admission_data)
summary(forward)$bic

summary(best)$adjr2
