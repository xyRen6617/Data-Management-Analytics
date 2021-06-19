

man_united_data$ceil_possession = 0
man_united_data$ceil_possession = ceiling(man_united_data$man_united_possession/10)*10


p1 <- ggplot(man_united_data,aes(x = ceil_possession,y = man_united_fouls)) +geom_bar(stat = 'identity')  +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_united ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))

p1

man_united_data$opp_ceil_possession = 0
man_united_data$opp_ceil_possession = ceiling((100-man_united_data$opp_possession)/10)*10

p2 <- ggplot(man_united_data,aes(x = opp_ceil_possession, y = opp_fouls)) +geom_bar(stat = 'identity')  +xlab("1 - ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_united Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))

p2
p = p1 / p2

ggsave("man_united_opp_p_f_c.png",p,width = 9,height = 12)



man_city_data$ceil_possession = 0
man_city_data$ceil_possession = ceiling(man_city_data$man_city_possession/10)*10

p1 <- ggplot(data = man_city_data,aes(x = ceil_possession, y = man_city_fouls)) +geom_bar(stat = 'identity')+xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_city ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


man_city_data$opp_ceil_possession = 0
man_city_data$opp_ceil_possession = ceiling((100-man_city_data$opp_possession)/10)*10

p2 <- ggplot(data = man_city_data,aes(x = opp_ceil_possession, y = opp_fouls)) +geom_bar(stat = 'identity') +xlab("1 - ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_city Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


p = p1 / p2

ggsave("man_city_opp_p_f_c.png",p,width = 9,height = 12)
p

chelsea_data$ceil_possession = 0
chelsea_data$ceil_possession = ceiling(chelsea_data$chelsea_possession/10)*10

p1 <- ggplot(data = chelsea_data,aes(x = ceil_possession, y = chelsea_fouls)) +geom_bar(stat = 'identity')+xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of chelsea ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


chelsea_data$opp_ceil_possession = 0
chelsea_data$opp_ceil_possession = ceiling((100-chelsea_data$opp_possession)/10)*10

p2 <- ggplot(data = chelsea_data,aes(x = opp_ceil_possession, y = opp_fouls)) +geom_bar(stat = 'identity')+xlab("1 - ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of chelsea Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


p = p1/p2

ggsave("chelsea_opp_p_f_c.png",p,width = 9,height = 12)


arsenal_data$ceil_possession = 0
arsenal_data$ceil_possession = ceiling(arsenal_data$arsenal_possession/10)*10

p1 <- ggplot(data = arsenal_data,aes(x = ceil_possession, y = arsenal_fouls)) +geom_bar(stat = 'identity') +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of arsenal ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))



arsenal_data$opp_ceil_possession = 0
arsenal_data$opp_ceil_possession = ceiling((100-arsenal_data$opp_possession)/10)*10

p2 <- ggplot(data = arsenal_data,aes(x = opp_ceil_possession, y = opp_fouls)) +geom_bar(stat = 'identity')+xlab("1 - ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of arsenal Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


p = p1/p2

ggsave("arsenal_opp_p_f_c.png",p,width = 9,height = 12)




man_united_data$ceil_possession = 0
man_united_data$ceil_possession = ceiling(man_united_data$man_united_possession/10)*10

p1 <- ggplot(data = man_united_data,aes(x = ceil_possession, y = man_united_data$man_united_cards))+geom_bar(stat = 'identity')+xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_united ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))
p1

man_united_data$opp_ceil_possession = 0
man_united_data$opp_ceil_possession = ceiling((100-man_united_data$opp_possession)/10)*10

p2 <- ggplot(data = man_united_data,aes(x = opp_ceil_possession, y = opp_cards)) +geom_bar(stat = 'identity') +xlab("1 - ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_united Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


p = p1/p2

ggsave("man_united_opp_p_c_c.png",p,width = 9,height = 12)



man_city_data$ceil_possession = 0
man_city_data$ceil_possession = ceiling(man_city_data$man_city_possession/10)*10

p1 <- ggplot(data = man_city_data,aes(x = ceil_possession, y = man_city_data$man_city_cards)) +geom_bar(stat = 'identity') +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_city ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


man_city_data$opp_ceil_possession = 0
man_city_data$opp_ceil_possession = ceiling((100-man_city_data$opp_possession)/10)*10

p2 <- ggplot(data = man_city_data,aes(x = opp_ceil_possession, y = man_city_data$opp_cards)) +geom_bar(stat = 'identity') +xlab("1 - ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_city Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


p = p1/p2

ggsave("man_city_opp_p_c_c.png",p,width = 9,height = 12)


chelsea_data$ceil_possession = 0
chelsea_data$ceil_possession = ceiling(chelsea_data$chelsea_possession/10)*10

p1 <- ggplot(data = chelsea_data,aes(x = ceil_possession, y = chelsea_data$chelsea_cards)) +geom_bar(stat = 'identity') +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of chelsea ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


chelsea_data$opp_ceil_possession = 0
chelsea_data$opp_ceil_possession = ceiling((100-chelsea_data$opp_possession)/10)*10

p2 <- ggplot(data = chelsea_data,aes(x = opp_ceil_possession, y = chelsea_data$opp_cards)) +geom_bar(stat = 'identity') +xlab("1 - ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of chelsea Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


p = p1/p2

ggsave("chelsea_opp_p_c_c.png",p,width = 9,height = 12)


arsenal_data$ceil_possession = 0
arsenal_data$ceil_possession = ceiling(arsenal_data$arsenal_possession/10)*10

p1 <- ggplot(data = arsenal_data,aes(x = ceil_possession, y = arsenal_data$arsenal_cards)) +geom_bar(stat = 'identity') +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of arsenal ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


arsenal_data$opp_ceil_possession = 0
arsenal_data$opp_ceil_possession = ceiling((100-arsenal_data$opp_possession)/10)*10

p2 <- ggplot(data = arsenal_data,aes(x = opp_ceil_possession, y = arsenal_data$opp_cards)) +geom_bar(stat = 'identity') +xlab("1 - ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of arsenal Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))


p = p1/p2

ggsave("arsenal_opp_p_c_c.png",p,width = 9,height = 12)




man_united_data_shots = man_united_data[,c(133,135,137)]

man_united_data_shots_lf = melt(man_united_data_shots, id="ceil_possession")

p1 = ggplot(man_united_data_shots_lf,aes(x=ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = 'ceiling_possession',y = 'count', title = 'Shots and shots on target of man_united') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))

man_united_data_shots_opp = man_united_data[,c(134,136,138)]

man_united_data_shots_lf_opp = melt(man_united_data_shots_opp, id="opp_ceil_possession")

p2 = ggplot(man_united_data_shots_lf_opp,aes(x=opp_ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = '1 - ceiling_possession',y = 'count', title = 'Shots and shots on target of man_united Opp') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))
p2
p= p1/p2
ggsave("man_united_s_st.png",p,width = 10,height = 12)




man_city_data_shots = man_city_data[,c(133,135,137)]

man_city_data_shots_lf = melt(man_city_data_shots, id="ceil_possession")

p1 = ggplot(man_city_data_shots_lf,aes(x=ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = 'ceiling_possession',y = 'count', title = 'Shots and shots on target of man_city') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))

man_city_data_shots_opp = man_city_data[,c(134,136,138)]

man_city_data_shots_lf_opp = melt(man_city_data_shots_opp, id="opp_ceil_possession")

p2 = ggplot(man_city_data_shots_lf_opp,aes(x=opp_ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = '1 - ceiling_possession',y = 'count', title = 'Shots and shots on target of man_city Opp') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))
p2
p= p1/p2
ggsave("man_city_s_st.png",p,width = 10,height = 12)

chelsea_data_shots = chelsea_data[,c(133,135,137)]

chelsea_data_shots_lf = melt(chelsea_data_shots, id="ceil_possession")

p1 = ggplot(chelsea_data_shots_lf,aes(x=ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = 'ceiling_possession',y = 'count', title = 'Shots and shots on target of chelsea') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))

chelsea_data_shots_opp = chelsea_data[,c(134,136,138)]

chelsea_data_shots_lf_opp = melt(chelsea_data_shots_opp, id="opp_ceil_possession")

p2 = ggplot(chelsea_data_shots_lf_opp,aes(x=opp_ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = '1 - ceiling_possession',y = 'count', title = 'Shots and shots on target of chelsea Opp') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))
p2
p= p1/p2
ggsave("chelsea_s_st.png",p,width = 10,height = 12)

arsenal_data_shots = arsenal_data[,c(133,135,137)]

arsenal_data_shots_lf = melt(arsenal_data_shots, id="ceil_possession")

p1 = ggplot(arsenal_data_shots_lf,aes(x=ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = 'ceiling_possession',y = 'count', title = 'Shots and shots on target of arsenal') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))

arsenal_data_shots_opp = arsenal_data[,c(134,136,138)]

arsenal_data_shots_lf_opp = melt(arsenal_data_shots_opp, id="opp_ceil_possession")

p2 = ggplot(arsenal_data_shots_lf_opp,aes(x=opp_ceil_possession,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = '1 - ceiling_possession',y = 'count', title = 'Shots and shots on target of arsenal Opp') +theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"),limits = c(-5,100))
p2
p= p1/p2
ggsave("arsenal_s_st.png",p,width = 10,height = 12)


man_united_sg = c()
man_city_sg = c()
chelsea_sg = c()
arsenal_sg = c()


for (i in unique(full_data$season)){
  man_united_s = subset(man_united_data, season == i)
  man_city_s = subset(man_city_data, season == i)
  chelsea_s = subset(chelsea_data, season == i)
  arsenal_s = subset(arsenal_data, season == i)
  man_united_sg = append(man_united_sg,sum(man_united_s$man_united_sg))
  man_city_sg = append(man_city_sg,sum(man_city_s$man_city_sg))
  chelsea_sg = append(chelsea_sg,sum(chelsea_s$chelsea_sg))
  arsenal_sg = append(arsenal_sg,sum(arsenal_s$arsenal_sg))
}

season = unique(full_data$season)

for (i in 1:2){
  season = append(season,season)
}

sg = c()
sg = append(sg,man_united_sg)
sg = append(sg,man_city_sg)
sg = append(sg,chelsea_sg)
sg = append(sg,arsenal_sg)

team = c()
team = append(team,rep("man_united",length((man_united_sg))))
team = append(team,rep("man_city",length((man_city_sg))))
team = append(team,rep("chelsea",length((chelsea_sg))))
team = append(team,rep("arsenal",length((arsenal_sg))))

sg_data = data.frame(season,team,sg)


p1<-ggplot(data=sg_data, mapping=aes(x = season, y = sg,fill=team))+geom_bar(stat="identity",position=position_dodge(0.75))+ylab("goals difference")+ labs(colour = "Team")+ ggtitle("Goals difference for each season of four teams")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "goal_diff_season.png",width = 12,height = 9)



man_city_sg_data = subset(sg_data,team=="man_city")

man_city_sg_data = man_city_sg_data[order(man_city_sg_data$season),]
man_city_data$man_city_sg

p = ggplot(data = man_city_data) + geom_boxplot(aes(x = season, y = man_city_sg))+ylab("goals difference")+ labs(colour = "Team")+ ggtitle("Goals difference for each season of man city")+theme(plot.title = element_text(hjust = 0.5))
ggsave("man_city_goal_diff.png",p,width = 12,height = 9)

sg_data = sg_data[order(sg_data$season),]

man_united_data$s = man_united_data$man_united_possession
man_united_data$team = "man_united"
man_city_data$s = man_city_data$man_city_possession
man_city_data$team = "man_city"
chelsea_data$s = chelsea_data$chelsea_possession
chelsea_data$team = "chelsea"
arsenal_data$s = arsenal_data$arsenal_possession
arsenal_data$team = "arsenal"


sg_full_data = rbind(man_united_data[,c(3,137,138)],man_city_data[,c(3,137,138)],chelsea_data[,c(3,137,138)],arsenal_data[,c(3,137,138)])


p <- ggplot(sg_full_data, aes(x = season, y = s))+
  geom_boxplot(aes(fill = team),position=position_dodge(0.7),width=0.6)+ylab("possession")+ labs(colour = "Team")+ ggtitle("Possession for each season of four teams")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p,filename = "possession_four.png",width = 18,height = 9)

