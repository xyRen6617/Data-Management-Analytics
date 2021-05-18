#model1
library(ggplot2)
library(corrplot)
library(patchwork)


setwd("C:/Users/duli/Documents/20210514")

insurance_data = read.csv("insurance.csv")

admission_data <- read.csv("Admission_Predict.csv")

admission_data_1_1 <- read.csv("Admission_Predict_Ver1.1.csv")

sum(is.na(admission_data))


ggplot(admission_data,aes(x=GRE.Score,y=Chance.of.Admit))+geom_point()+geom_smooth()+ggtitle("Chances of Admit vs GRE Score")


ggplot(admission_data,aes(x=TOEFL.Score,y=Chance.of.Admit))+geom_point()+geom_smooth()+ggtitle("Chances of Admit vs GRE Score")



p10 <- ggplot(data=admission_data,aes(x=CGPA,y=TOEFL.Score,group=CGPA)) + geom_point()
p11 <- ggplot(data=admission_data,aes(x=round_gpa,y=TOEFL.Score,group=round_gpa)) + geom_boxplot()

p12 <- p10 + p11

ggsave("Test",p12,width = 12,height = 9)



admission_data$round_gpa = floor(admission_data$CGPA)

p1 <- ggplot(data=admission_data,aes(x=round_gpa,y=Chance.of.Admit,group=round_gpa))+scale_x_continuous(breaks=c(6:9),labels =c("6 to 6.99","7 to 7.99","8 to 8.99","9 to 9.99"))+xlab("CGPA group")+geom_boxplot()

ggsave("Gpa_Admit.png",p1,width = 12,height = 9)

p2 <- ggplot(data=admission_data,aes(x=round_gpa,y=TOEFL.Score,group=round_gpa))+scale_x_continuous(breaks=c(6:9),labels =c("6 to 6.99","7 to 7.99","8 to 8.99","9 to 9.99"))+xlab("CGPA group")+geom_boxplot()

ggsave("Gpa_Toefl.png",p2,width = 12,height = 9)

p3 <- ggplot(data=admission_data,aes(x=round_gpa,y=GRE.Score,group=round_gpa))+scale_x_continuous(breaks=c(6:9),labels =c("6 to 6.99","7 to 7.99","8 to 8.99","9 to 9.99"))+xlab("CGPA group")+geom_boxplot()

ggsave("Gpa_GRE.png",p3,width = 12,height = 9)

admission_data<-admission_data[,-c(1)]
admission_data<-admission_data[,-c(9)]

C<-cor(admission_data)
corrplot(C,method='number')

p4 <- ggplot(admission_data,aes(x=CGPA,y=GRE.Score))+geom_point()+geom_smooth()+ggtitle("GPA vs GRE Score")
ggsave("GPA_GREl.png",p4,width = 12,height = 9)


p5 <- ggplot(admission_data,aes(x=CGPA,y=TOEFL.Score))+geom_point()+geom_smooth()+ggtitle("GPA vs TOEFL Score")
ggsave("Gpa_TOEFLl.png",p5,width = 12,height = 9)

p6 <- ggplot(admission_data,aes(x=CGPA,y=GRE.Score,col=Research
))+geom_point()+geom_smooth()+ggtitle("GPA vs GRE Score with Research")
ggsave("GPA_GREl_research.png",p6,width = 12,height = 9)


p7 <- ggplot(admission_data,aes(x=CGPA,y=TOEFL.Score,col=Research))+geom_point()+geom_smooth()+ggtitle("GPA vs TOEFL Score with Research")
ggsave("Gpa_TOEFLl_research.png",p7,width = 12,height = 9)





fit <- lm(TOEFL.Score ~ CGPA, data = admission_data)
summary(fit)

fitted(fit)

residuals(fit)

p <- ggplot(admission_data, aes(CGPA, TOEFL.Score)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red", method = "lm") +
  ggtitle("TOEFL Score vs Cumulative Grade Point Average")

ggsave("fit_T.png",p,width = 12,height = 9)

fit <- lm(GRE.Score ~ CGPA, data = admission_data)
summary(fit)

fitted(fit)

residuals(fit)

p <- ggplot(admission_data, aes(CGPA, GRE.Score)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red", method = "lm") +
  ggtitle("GRE Score vs Cumulative Grade Point Average")

ggsave("fit_G.png",p,width = 12,height = 9)



#plot(admission_data$TOEFL.Score,admission_data$CGPA)
#abline(fit)

