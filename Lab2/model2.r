library(ggplot2)
library(corrplot)
library(patchwork)


setwd("C:/Users/duli/Documents/20210514")

insurance_data = read.csv("insurance.csv")

admission_data <- read.csv("Admission_Predict.csv")

admission_data_1_1 <- read.csv("Admission_Predict_Ver1.1.csv")

admission_data$round_gpa = round(admission_data$CGPA,digits = 0)

p4 <- ggplot(admission_data,aes(x=CGPA,y=Chance.of.Admit))+geom_point()+geom_smooth()+ggtitle("GPA vs Admit")
ggsave("GPA_Admit_scatter.png",p4,width = 12,height = 9)


p5 <- ggplot(admission_data,aes(x=TOEFL.Score,y=Chance.of.Admit))+geom_point()+geom_smooth()+ggtitle("TOEFL Score vs Admit")
ggsave("Toefl_Admit_scatter.png",p5,width = 12,height = 9)

p6 <- ggplot(admission_data,aes(x=GRE.Score,y=Chance.of.Admit,col=Research
))+geom_point()+geom_smooth()+ggtitle(" GRE Score vs Admit")
ggsave("GPA_GREl_research.png",p6,width = 12,height = 9)


p7 <- ggplot(admission_data,aes(x=CGPA,y=TOEFL.Score,col=Research))+geom_point()+geom_smooth()+ggtitle("TOEFL Score vs Admit")
ggsave("TOEFLl_Admit_scatter.png",p7,width = 12,height = 9)

p8 <- ggplot(admission_data,aes(x=CGPA,y=Chance.of.Admit,col=University.Rating))+geom_point()+geom_smooth()+ggtitle("GPA vs Admit  with University Rating")
ggsave("GPA_Admit_Rating_scatter.png",p8,width = 12,height = 9)

p9 <- ggplot(admission_data,aes(x=TOEFL.Score,y=Chance.of.Admit,col=University.Rating))+geom_point()+geom_smooth()+ggtitle("TOEFL Score vs Admit with University Rating")
ggsave("Toefl_Admit_rating_scatter.png",p9,width = 12,height = 9)


p10 <- ggplot(admission_data,aes(x=GRE.Score,y=Chance.of.Admit,col=Research
))+geom_point()+geom_smooth()+ggtitle(" GRE Score vs Admit with University Rating")
ggsave("GPA_GREl_research.png",p10,width = 12,height = 9)

model2 <- lm(Chance.of.Admit ~ TOEFL.Score + GRE.Score + CGPA, data = admission_data)

summary(model2)

