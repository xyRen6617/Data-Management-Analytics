library(ggplot2)
library(DataExplorer)
library(stats)
library(GGally)
library(factoextra)



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




model1_data <- subset(model1_data, select = -c(team))

model1_data = scale(model1_data)

possession_sg = data()

one_dim <- data.frame(model1_data[,c(3,10)])

m1 = kmeans(one_dim, 1, nstart =100)
p = ggplot(data=one_dim, aes(x=possession, y=sg , colour = factor(m1$cluster)))+ geom_point()+ xlab("Possession")+
  ylab("Goal difference")+ labs(colour = "Cluster Number")+ ggtitle("Possession VS Goal difference Clusters")+theme(plot.title = element_text(hjust = 0.5))
ggsave("m1.png",p,width = 12,height = 9)


m2 = kmeans(one_dim, 2, nstart =100)
p = ggplot(data=one_dim, aes(x=possession, y=sg , colour = factor(m2$cluster)))+ geom_point()+ xlab("Possession")+
  ylab("Goal difference")+ labs(colour = "Cluster Number")+ ggtitle("Possession VS Goal difference Clusters")+theme(plot.title = element_text(hjust = 0.5))
ggsave("m2.png",p,width = 12,height = 9)


m3 = kmeans(one_dim, 3, nstart =100)
p = ggplot(data=one_dim, aes(x=possession, y=sg , colour = factor(m3$cluster)))+ geom_point()+ xlab("Possession")+
  ylab("Goal difference")+ labs(colour = "Cluster Number")+ ggtitle("Possession VS Goal difference Clusters")+theme(plot.title = element_text(hjust = 0.5))
ggsave("m3.png",p,width = 12,height = 9)

m4 = kmeans(one_dim, 4, nstart =100)
p = ggplot(data=one_dim, aes(x=possession, y=sg , colour = factor(m4$cluster)))+ geom_point()+ xlab("Possession")+
  ylab("Goal difference")+ labs(colour = "Cluster Number")+ ggtitle("Possession VS Goal difference Clusters")+theme(plot.title = element_text(hjust = 0.5))
ggsave("m4.png",p,width = 12,height = 9)

m5 = kmeans(one_dim, 5, nstart =100)
p = ggplot(data=one_dim, aes(x=possession, y=sg , colour = factor(m5$cluster)))+ geom_point()+ xlab("Possession")+
  ylab("Goal difference")+ labs(colour = "Cluster Number")+ ggtitle("Possession VS Goal difference Clusters")+theme(plot.title = element_text(hjust = 0.5))
ggsave("m5.png",p,width = 12,height = 9)




for (i in 2:5){
  print(paste("cluster number is :",i))
  m5 = kmeans(one_dim, i, nstart =1)
  print(paste("the value of withinss when nstart = 1 :", m5$withinss))
  print(paste("the value of tot.withinss when nstart = 1 :", m5$tot.withinss))
  print(paste("betweenss / totss = ",m5$betweenss / m5$totss))
  
  m5 = kmeans(one_dim, i, nstart =100)
  print(paste("the value of withinss when nstart = 100 :", m5$withinss))
  print(paste("the value of tot.withinss when nstart = 100 :", m5$tot.withinss))
  print(paste("betweenss / totss = ",m5$betweenss / m5$totss))
  
  m5 = kmeans(one_dim, i, nstart =500)
  print(paste("the value of withinss when nstart = 500 :", m5$withinss))
  print(paste("the value of tot.withinss when nstart = 500 :", m5$tot.withinss))
  print(paste("betweenss / totss = ",m5$betweenss / m5$totss))
}


