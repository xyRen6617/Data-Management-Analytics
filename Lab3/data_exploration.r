library(ggplot2)
library(beeswarm)
library(ggbeeswarm)
library(patchwork)

bike_data <-read.csv("bike_buyers_clean.csv")

p <- ggplot()+geom_bar(aes(x=bike_data$Marital.Status, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Marital.Status, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.8,.9))

ggsave("Martial_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Gender, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Gender, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.93,.93))

ggsave("Gender_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Income, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Income, fill = Purchased.Bike)) +
  geom_bar(position = position_dodge2(padding = 0,preserve = "single"),color="black") +theme(legend.position = c(.8,.9))
ggsave("Income_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Children, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Children, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 1) +
  theme(legend.position = c(.8,.9))

ggsave("Children_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Education, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Education, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.8,.9))

ggsave("Education_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Occupation, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Occupation, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.1,.9))

ggsave("Occupation_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Home.Owner, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Home.Owner, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.1,.9))


ggsave("Home_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Cars, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Cars, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.8,.9))

ggsave("Cars_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Commute.Distance, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Commute.Distance, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.8,.9))

ggsave("Commute_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Region, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Region, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.8,.9))

ggsave("Region_Purchased.png",p,width = 12,height = 9)

p <- ggplot()+geom_bar(aes(x=bike_data$Age, colour=bike_data$Purchased.Bike))

p <- ggplot(data = bike_data,aes(x = Age, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(size = 2,stat = "count", aes(label = ..count..),position = position_dodge(1), vjust = 0) +theme(legend.position = c(.8,.9))

ggsave("Age_Purchased.png",p,width = 12,height = 9)

beeswarm(Age ~ Purchased.Bike, data = bike_data, 
         log = TRUE, pch = 16, col = 1:3,cex=0.5,spacing = 0.8,main = 'beeswarm')

ggplot(bike_data,aes(Purchased.Bike, Age)) + geom_beeswarm()


ggplot(bike_data, aes(x = Purchased.Bike, y = Age)) +geom_beeswarm(cex=3)


Married <- subset(bike_data,bike_data$Marital.Status == "Married")
  
Single <- subset(bike_data,bike_data$Marital.Status == "Single")

p1 <- ggplot(data = Married,aes(x = Children, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  theme(legend.position = c(.8,.9),plot.title = element_text(hjust = 0.5))+ggtitle("Martial Status = Married")

p2 <- ggplot(data = Single,aes(x = Children, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  theme(legend.position = c(.8,.9),plot.title = element_text(hjust = 0.5))+ggtitle("Martial Status = Single")+

ggsave("test.png",p1,width = 12,height = 9)

p1 + p2

p1 <- ggplot(bike_data, aes(x = Occupation, y = Income))+ 
  geom_boxplot(aes(fill = Purchased.Bike),position=position_dodge(0.5),width=0.6)+theme(
                                                                                        plot.margin=unit(rep(0.5,4), 'lines'),
                                                                                        panel.background=element_rect(fill='transparent',color='black'),
                                                                                        panel.border=element_rect(fill='transparent', color='transparent'),
                                                                                        panel.grid=element_blank(),
                                                                                        axis.line = element_line(colour = "black"),
                                                                                        axis.title.x = element_blank(),
                                                                                        axis.title.y=element_text(face = "bold",size = 14),
                                                                                        axis.text = element_text(face = "bold",size = 12),         
                                                                                        axis.ticks = element_line(color='black'),
                                                                                        # axis.ticks.margin = unit(0.8,"lines"),
                                                                                        legend.title=element_blank(),
                                                                                        legend.position=c(0.85, 0.93),
                                                                                        legend.direction = "horizontal",
                                                                                        legend.text = element_text(face = "bold",size = 12,margin = margin(r=8)),
                                                                                        legend.background = element_rect( linetype="solid",colour ="black")
  )


No <- subset(bike_data,bike_data$Purchased.Bike == "No")

Yes <- subset(bike_data,bike_data$Purchased.Bike == "Yes")

No$Cars <- as.character(No$Cars)

Yes$Cars <- as.character(Yes$Cars)

p1 <- ggplot(data = No) +
  geom_bar(aes(x = Commute.Distance, fill = Cars),position = "dodge", color = "black") +
  theme(legend.position = c(.7,.8),plot.title = element_text(hjust = 0.5))+ggtitle(" Purchased Bike = No")

p2 <- ggplot(data = Yes) +
  geom_bar(aes(x = Commute.Distance, fill = Cars),position = "dodge", color = "black") +
  theme(legend.position = c(.7,.8),plot.title = element_text(hjust = 0.5))+ggtitle(" Purchased Bike = Yes")

ggsave("test.png",p1,width = 12,height = 9)

p1 + p2


Europe <- subset(bike_data,bike_data$Region == "Europe")
Pacific <- subset(bike_data,bike_data$Region == "Pacific")
NorthA <- subset(bike_data,bike_data$Region == "North America")

p1 <- ggplot(Europe,aes(Education, Age,color=factor(Purchased.Bike)))+ geom_quasirandom(dodge.width=1,size = 5)+ theme(legend.position = c(.75,.95),plot.title = element_text(hjust = 0.5)) + ggtitle("Region = Europe")

p2 <- ggplot(Pacific,aes(Education, Age,color=factor(Purchased.Bike)))+ geom_quasirandom(dodge.width=1,size = 5)+ theme(legend.position = c(.75,.95),plot.title = element_text(hjust = 0.5))+ggtitle("Region = Pacific")

p3<- ggplot(NorthA,aes(Education, Age,color=factor(Purchased.Bike)))+ geom_quasirandom(dodge.width=1,size = 5)+ theme(legend.position = c(.75,.95),plot.title = element_text(hjust = 0.5))+ggtitle("Region = North America")

p1 + p2 + p3
  
