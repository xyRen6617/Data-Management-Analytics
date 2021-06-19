library(ggplot2)
library(DataExplorer)
library(patchwork)


setwd('C:\\Users\\duli\\Documents\\20210614')

full_data = read.csv('df_full_premierleague.csv')

plot_missing(full_data)

full_data <- na.omit(full_data)

#Man United

man_united_home = subset(full_data, home_team == "Manchester United")
man_united_away = subset(full_data, away_team == "Manchester United")


man_united_home$man_united_possession = 0
man_united_away$man_united_possession = 0
man_united_home$opp_possession = 0
man_united_away$opp_possession = 0
man_united_home$man_united_possession = man_united_home$home_possession
man_united_away$man_united_possession = man_united_away$away_possession
man_united_home$opp_possession = man_united_home$away_possession
man_united_away$opp_possession = man_united_away$home_possession


man_united_home$man_united_touches = 0
man_united_away$man_united_touches = 0
man_united_home$opp_touches = 0
man_united_away$opp_touches = 0
man_united_home$man_united_touches = man_united_home$home_touches
man_united_away$man_united_touches = man_united_away$away_touches
man_united_home$opp_touches = man_united_home$away_touches
man_united_away$opp_touches = man_united_away$home_touches



man_united_home$man_united_passes = 0
man_united_away$man_united_passes = 0
man_united_home$opp_passes = 0
man_united_away$opp_passes = 0
man_united_home$man_united_passes = man_united_home$home_passes
man_united_away$man_united_passes = man_united_away$away_passes
man_united_home$opp_passes = man_united_home$away_passes
man_united_away$opp_passes = man_united_away$home_passes

man_united_home$man_united_shot_acc = 0
man_united_away$man_united_shot_acc = 0
man_united_home$opp_shot_acc = 0
man_united_away$opp_shot_acc = 0
man_united_home$man_united_shot_acc = man_united_home$home_shots_on_target / man_united_home$home_shots
man_united_away$man_united_shot_acc = man_united_away$away_shots_on_target /  man_united_away$away_shots
man_united_home$opp_shot_acc = man_united_home$away_shots_on_target / man_united_home$away_shots
man_united_away$opp_shot_acc = man_united_away$home_shots_on_target /  man_united_away$home_shots


man_united_home$man_united_fouls = 0
man_united_away$man_united_fouls = 0
man_united_home$opp_fouls = 0
man_united_away$opp_fouls = 0
man_united_home$man_united_fouls = man_united_home$home_fouls_conceded
man_united_away$man_united_fouls = man_united_away$away_fouls_conceded
man_united_home$opp_fouls = man_united_home$away_fouls_conceded
man_united_away$opp_fouls = man_united_away$home_fouls_conceded


man_united_home$man_united_cards = 0
man_united_away$man_united_cards = 0
man_united_home$opp_cards = 0
man_united_away$opp_cards = 0
man_united_home$man_united_cards = man_united_home$home_red_cards + man_united_home$home_yellow_cards
man_united_away$man_united_cards = man_united_away$away_red_cards + man_united_away$away_yellow_cards
man_united_home$opp_cards = man_united_home$away_red_cards + man_united_home$away_yellow_cards
man_united_away$opp_cards = man_united_away$home_red_cards + man_united_away$home_yellow_cards


man_united_home$man_united_clearances = 0
man_united_away$man_united_clearances = 0
man_united_home$opp_clearances = 0
man_united_away$opp_clearances = 0
man_united_home$man_united_clearances = man_united_home$home_clearances
man_united_away$man_united_clearances = man_united_away$away_clearances
man_united_home$opp_clearances = man_united_home$away_clearances
man_united_away$opp_clearances = man_united_away$home_clearances


man_united_home$man_united_corners = 0
man_united_away$man_united_corners = 0
man_united_home$opp_corners = 0
man_united_away$opp_corners = 0
man_united_home$man_united_corners = man_united_home$home_corners
man_united_away$man_united_corners = man_united_away$away_corners
man_united_home$opp_corners = man_united_home$away_corners
man_united_away$opp_corners = man_united_away$home_corners


man_united_home$man_united_sg = 0
man_united_away$man_united_sg = 0
man_united_home$opp_sg = 0
man_united_away$opp_sg = 0
man_united_home$man_united_sg = man_united_home$sg_match_ft
man_united_away$man_united_sg = -man_united_away$sg_match_ft
man_united_home$opp_sg = -man_united_home$sg_match_ft
man_united_away$opp_sg = man_united_away$sg_match_ft

man_united_home$man_united_shots = 0
man_united_away$man_united_shots = 0
man_united_home$opp_shots = 0
man_united_away$opp_shots = 0
man_united_home$man_united_shots = man_united_home$home_shots
man_united_away$man_united_shots = man_united_away$away_shots
man_united_home$opp_shots = man_united_home$away_shots
man_united_away$opp_shots = man_united_away$home_shots

man_united_home$man_united_shots_on_target = 0
man_united_away$man_united_shots_on_target = 0
man_united_home$opp_shots_on_target = 0
man_united_away$opp_shots_on_target = 0
man_united_home$man_united_shots_on_target = man_united_home$home_shots_on_target
man_united_away$man_united_shots_on_target = man_united_away$away_shots_on_target
man_united_home$opp_shots_on_target = man_united_home$away_shots_on_target
man_united_away$opp_shots_on_target = man_united_away$home_shots_on_target

man_united_home$away_team = "Opp"
man_united_away$home_team = "Opp"
man_united_data = rbind(man_united_home,man_united_away)

#Man city

man_city_home = subset(full_data, home_team == "Manchester City")
man_city_away = subset(full_data, away_team == "Manchester City")


man_city_home$man_city_possession = 0
man_city_away$man_city_possession = 0
man_city_home$opp_possession = 0
man_city_away$opp_possession = 0
man_city_home$man_city_possession = man_city_home$home_possession
man_city_away$man_city_possession = man_city_away$away_possession
man_city_home$opp_possession = man_city_home$away_possession
man_city_away$opp_possession = man_city_away$home_possession


man_city_home$man_city_touches = 0
man_city_away$man_city_touches = 0
man_city_home$opp_touches = 0
man_city_away$opp_touches = 0
man_city_home$man_city_touches = man_city_home$home_touches
man_city_away$man_city_touches = man_city_away$away_touches
man_city_home$opp_touches = man_city_home$away_touches
man_city_away$opp_touches = man_city_away$home_touches



man_city_home$man_city_passes = 0
man_city_away$man_city_passes = 0
man_city_home$opp_passes = 0
man_city_away$opp_passes = 0
man_city_home$man_city_passes = man_city_home$home_passes
man_city_away$man_city_passes = man_city_away$away_passes
man_city_home$opp_passes = man_city_home$away_passes
man_city_away$opp_passes = man_city_away$home_passes

man_city_home$man_city_shot_acc = 0
man_city_away$man_city_shot_acc = 0
man_city_home$opp_shot_acc = 0
man_city_away$opp_shot_acc = 0
man_city_home$man_city_shot_acc = man_city_home$home_shots_on_target / man_city_home$home_shots
man_city_away$man_city_shot_acc = man_city_away$away_shots_on_target /  man_city_away$away_shots
man_city_home$opp_shot_acc = man_city_home$away_shots_on_target / man_city_home$away_shots
man_city_away$opp_shot_acc = man_city_away$home_shots_on_target /  man_city_away$home_shots


man_city_home$man_city_fouls = 0
man_city_away$man_city_fouls = 0
man_city_home$opp_fouls = 0
man_city_away$opp_fouls = 0
man_city_home$man_city_fouls = man_city_home$home_fouls_conceded
man_city_away$man_city_fouls = man_city_away$away_fouls_conceded
man_city_home$opp_fouls = man_city_home$away_fouls_conceded
man_city_away$opp_fouls = man_city_away$home_fouls_conceded


man_city_home$man_city_cards = 0
man_city_away$man_city_cards = 0
man_city_home$opp_cards = 0
man_city_away$opp_cards = 0
man_city_home$man_city_cards = man_city_home$home_red_cards + man_city_home$home_yellow_cards
man_city_away$man_city_cards = man_city_away$away_red_cards + man_city_away$away_yellow_cards
man_city_home$opp_cards = man_city_home$away_red_cards + man_city_home$away_yellow_cards
man_city_away$opp_cards = man_city_away$home_red_cards + man_city_away$home_yellow_cards


man_city_home$man_city_clearances = 0
man_city_away$man_city_clearances = 0
man_city_home$opp_clearances = 0
man_city_away$opp_clearances = 0
man_city_home$man_city_clearances = man_city_home$home_clearances
man_city_away$man_city_clearances = man_city_away$away_clearances
man_city_home$opp_clearances = man_city_home$away_clearances
man_city_away$opp_clearances = man_city_away$home_clearances


man_city_home$man_city_corners = 0
man_city_away$man_city_corners = 0
man_city_home$opp_corners = 0
man_city_away$opp_corners = 0
man_city_home$man_city_corners = man_city_home$home_corners
man_city_away$man_city_corners = man_city_away$away_corners
man_city_home$opp_corners = man_city_home$away_corners
man_city_away$opp_corners = man_city_away$home_corners


man_city_home$man_city_sg = 0
man_city_away$man_city_sg = 0
man_city_home$opp_sg = 0
man_city_away$opp_sg = 0
man_city_home$man_city_sg = man_city_home$sg_match_ft
man_city_away$man_city_sg = -man_city_away$sg_match_ft
man_city_home$opp_sg = -man_city_home$sg_match_ft
man_city_away$opp_sg = man_city_away$sg_match_ft

man_city_home$man_city_shots = 0
man_city_away$man_city_shots = 0
man_city_home$opp_shots = 0
man_city_away$opp_shots = 0
man_city_home$man_city_shots = man_city_home$home_shots
man_city_away$man_city_shots = man_city_away$away_shots
man_city_home$opp_shots = man_city_home$away_shots
man_city_away$opp_shots = man_city_away$home_shots

man_city_home$man_city_shots_on_target = 0
man_city_away$man_city_shots_on_target = 0
man_city_home$opp_shots_on_target = 0
man_city_away$opp_shots_on_target = 0
man_city_home$man_city_shots_on_target = man_city_home$home_shots_on_target
man_city_away$man_city_shots_on_target = man_city_away$away_shots_on_target
man_city_home$opp_shots_on_target = man_city_home$away_shots_on_target
man_city_away$opp_shots_on_target = man_city_away$home_shots_on_target

man_city_home$away_team = "Opp"
man_city_away$home_team = "Opp"
man_city_data = rbind(man_city_home,man_city_away)



#Chelsea

chelsea_home = subset(full_data, home_team == "Chelsea")
chelsea_away = subset(full_data, away_team == "Chelsea")


chelsea_home$chelsea_possession = 0
chelsea_away$chelsea_possession = 0
chelsea_home$opp_possession = 0
chelsea_away$opp_possession = 0
chelsea_home$chelsea_possession = chelsea_home$home_possession
chelsea_away$chelsea_possession = chelsea_away$away_possession
chelsea_home$opp_possession = chelsea_home$away_possession
chelsea_away$opp_possession = chelsea_away$home_possession


chelsea_home$chelsea_touches = 0
chelsea_away$chelsea_touches = 0
chelsea_home$opp_touches = 0
chelsea_away$opp_touches = 0
chelsea_home$chelsea_touches = chelsea_home$home_touches
chelsea_away$chelsea_touches = chelsea_away$away_touches
chelsea_home$opp_touches = chelsea_home$away_touches
chelsea_away$opp_touches = chelsea_away$home_touches



chelsea_home$chelsea_passes = 0
chelsea_away$chelsea_passes = 0
chelsea_home$opp_passes = 0
chelsea_away$opp_passes = 0
chelsea_home$chelsea_passes = chelsea_home$home_passes
chelsea_away$chelsea_passes = chelsea_away$away_passes
chelsea_home$opp_passes = chelsea_home$away_passes
chelsea_away$opp_passes = chelsea_away$home_passes

chelsea_home$chelsea_shot_acc = 0
chelsea_away$chelsea_shot_acc = 0
chelsea_home$opp_shot_acc = 0
chelsea_away$opp_shot_acc = 0
chelsea_home$chelsea_shot_acc = chelsea_home$home_shots_on_target / chelsea_home$home_shots
chelsea_away$chelsea_shot_acc = chelsea_away$away_shots_on_target /  chelsea_away$away_shots
chelsea_home$opp_shot_acc = chelsea_home$away_shots_on_target / chelsea_home$away_shots
chelsea_away$opp_shot_acc = chelsea_away$home_shots_on_target /  chelsea_away$home_shots


chelsea_home$chelsea_fouls = 0
chelsea_away$chelsea_fouls = 0
chelsea_home$opp_fouls = 0
chelsea_away$opp_fouls = 0
chelsea_home$chelsea_fouls = chelsea_home$home_fouls_conceded
chelsea_away$chelsea_fouls = chelsea_away$away_fouls_conceded
chelsea_home$opp_fouls = chelsea_home$away_fouls_conceded
chelsea_away$opp_fouls = chelsea_away$home_fouls_conceded


chelsea_home$chelsea_cards = 0
chelsea_away$chelsea_cards = 0
chelsea_home$opp_cards = 0
chelsea_away$opp_cards = 0
chelsea_home$chelsea_cards = chelsea_home$home_red_cards + chelsea_home$home_yellow_cards
chelsea_away$chelsea_cards = chelsea_away$away_red_cards + chelsea_away$away_yellow_cards
chelsea_home$opp_cards = chelsea_home$away_red_cards + chelsea_home$away_yellow_cards
chelsea_away$opp_cards = chelsea_away$home_red_cards + chelsea_away$home_yellow_cards


chelsea_home$chelsea_clearances = 0
chelsea_away$chelsea_clearances = 0
chelsea_home$opp_clearances = 0
chelsea_away$opp_clearances = 0
chelsea_home$chelsea_clearances = chelsea_home$home_clearances
chelsea_away$chelsea_clearances = chelsea_away$away_clearances
chelsea_home$opp_clearances = chelsea_home$away_clearances
chelsea_away$opp_clearances = chelsea_away$home_clearances


chelsea_home$chelsea_corners = 0
chelsea_away$chelsea_corners = 0
chelsea_home$opp_corners = 0
chelsea_away$opp_corners = 0
chelsea_home$chelsea_corners = chelsea_home$home_corners
chelsea_away$chelsea_corners = chelsea_away$away_corners
chelsea_home$opp_corners = chelsea_home$away_corners
chelsea_away$opp_corners = chelsea_away$home_corners


chelsea_home$chelsea_sg = 0
chelsea_away$chelsea_sg = 0
chelsea_home$opp_sg = 0
chelsea_away$opp_sg = 0
chelsea_home$chelsea_sg = chelsea_home$sg_match_ft
chelsea_away$chelsea_sg = -chelsea_away$sg_match_ft
chelsea_home$opp_sg = -chelsea_home$sg_match_ft
chelsea_away$opp_sg = chelsea_away$sg_match_ft


chelsea_home$chelsea_shots = 0
chelsea_away$chelsea_shots = 0
chelsea_home$opp_shots = 0
chelsea_away$opp_shots = 0
chelsea_home$chelsea_shots = chelsea_home$home_shots
chelsea_away$chelsea_shots = chelsea_away$away_shots
chelsea_home$opp_shots = chelsea_home$away_shots
chelsea_away$opp_shots = chelsea_away$home_shots

chelsea_home$chelsea_shots_on_target = 0
chelsea_away$chelsea_shots_on_target = 0
chelsea_home$opp_shots_on_target = 0
chelsea_away$opp_shots_on_target = 0
chelsea_home$chelsea_shots_on_target = chelsea_home$home_shots_on_target
chelsea_away$chelsea_shots_on_target = chelsea_away$away_shots_on_target
chelsea_home$opp_shots_on_target = chelsea_home$away_shots_on_target
chelsea_away$opp_shots_on_target = chelsea_away$home_shots_on_target

chelsea_home$away_team = "Opp"
chelsea_away$home_team = "Opp"
chelsea_data = rbind(chelsea_home,chelsea_away)


#Arsenal

arsenal_home = subset(full_data, home_team == "Arsenal")
arsenal_away = subset(full_data, away_team == "Arsenal")


arsenal_home$arsenal_possession = 0
arsenal_away$arsenal_possession = 0
arsenal_home$opp_possession = 0
arsenal_away$opp_possession = 0
arsenal_home$arsenal_possession = arsenal_home$home_possession
arsenal_away$arsenal_possession = arsenal_away$away_possession
arsenal_home$opp_possession = arsenal_home$away_possession
arsenal_away$opp_possession = arsenal_away$home_possession


arsenal_home$arsenal_touches = 0
arsenal_away$arsenal_touches = 0
arsenal_home$opp_touches = 0
arsenal_away$opp_touches = 0
arsenal_home$arsenal_touches = arsenal_home$home_touches
arsenal_away$arsenal_touches = arsenal_away$away_touches
arsenal_home$opp_touches = arsenal_home$away_touches
arsenal_away$opp_touches = arsenal_away$home_touches



arsenal_home$arsenal_passes = 0
arsenal_away$arsenal_passes = 0
arsenal_home$opp_passes = 0
arsenal_away$opp_passes = 0
arsenal_home$arsenal_passes = arsenal_home$home_passes
arsenal_away$arsenal_passes = arsenal_away$away_passes
arsenal_home$opp_passes = arsenal_home$away_passes
arsenal_away$opp_passes = arsenal_away$home_passes

arsenal_home$arsenal_shot_acc = 0
arsenal_away$arsenal_shot_acc = 0
arsenal_home$opp_shot_acc = 0
arsenal_away$opp_shot_acc = 0
arsenal_home$arsenal_shot_acc = arsenal_home$home_shots_on_target / arsenal_home$home_shots
arsenal_away$arsenal_shot_acc = arsenal_away$away_shots_on_target /  arsenal_away$away_shots
arsenal_home$opp_shot_acc = arsenal_home$away_shots_on_target / arsenal_home$away_shots
arsenal_away$opp_shot_acc = arsenal_away$home_shots_on_target /  arsenal_away$home_shots


arsenal_home$arsenal_fouls = 0
arsenal_away$arsenal_fouls = 0
arsenal_home$opp_fouls = 0
arsenal_away$opp_fouls = 0
arsenal_home$arsenal_fouls = arsenal_home$home_fouls_conceded
arsenal_away$arsenal_fouls = arsenal_away$away_fouls_conceded
arsenal_home$opp_fouls = arsenal_home$away_fouls_conceded
arsenal_away$opp_fouls = arsenal_away$home_fouls_conceded


arsenal_home$arsenal_cards = 0
arsenal_away$arsenal_cards = 0
arsenal_home$opp_cards = 0
arsenal_away$opp_cards = 0
arsenal_home$arsenal_cards = arsenal_home$home_red_cards + arsenal_home$home_yellow_cards
arsenal_away$arsenal_cards = arsenal_away$away_red_cards + arsenal_away$away_yellow_cards
arsenal_home$opp_cards = arsenal_home$away_red_cards + arsenal_home$away_yellow_cards
arsenal_away$opp_cards = arsenal_away$home_red_cards + arsenal_away$home_yellow_cards


arsenal_home$arsenal_clearances = 0
arsenal_away$arsenal_clearances = 0
arsenal_home$opp_clearances = 0
arsenal_away$opp_clearances = 0
arsenal_home$arsenal_clearances = arsenal_home$home_clearances
arsenal_away$arsenal_clearances = arsenal_away$away_clearances
arsenal_home$opp_clearances = arsenal_home$away_clearances
arsenal_away$opp_clearances = arsenal_away$home_clearances


arsenal_home$arsenal_corners = 0
arsenal_away$arsenal_corners = 0
arsenal_home$opp_corners = 0
arsenal_away$opp_corners = 0
arsenal_home$arsenal_corners = arsenal_home$home_corners
arsenal_away$arsenal_corners = arsenal_away$away_corners
arsenal_home$opp_corners = arsenal_home$away_corners
arsenal_away$opp_corners = arsenal_away$home_corners


arsenal_home$arsenal_sg = 0
arsenal_away$arsenal_sg = 0
arsenal_home$opp_sg = 0
arsenal_away$opp_sg = 0
arsenal_home$arsenal_sg = arsenal_home$sg_match_ft
arsenal_away$arsenal_sg = -arsenal_away$sg_match_ft
arsenal_home$opp_sg = -arsenal_home$sg_match_ft
arsenal_away$opp_sg = arsenal_away$sg_match_ft

arsenal_home$arsenal_shots = 0
arsenal_away$arsenal_shots = 0
arsenal_home$opp_shots = 0
arsenal_away$opp_shots = 0
arsenal_home$arsenal_shots = arsenal_home$home_shots
arsenal_away$arsenal_shots = arsenal_away$away_shots
arsenal_home$opp_shots = arsenal_home$away_shots
arsenal_away$opp_shots = arsenal_away$home_shots

arsenal_home$arsenal_shots_on_target = 0
arsenal_away$arsenal_shots_on_target = 0
arsenal_home$opp_shots_on_target = 0
arsenal_away$opp_shots_on_target = 0
arsenal_home$arsenal_shots_on_target = arsenal_home$home_shots_on_target
arsenal_away$arsenal_shots_on_target = arsenal_away$away_shots_on_target
arsenal_home$opp_shots_on_target = arsenal_home$away_shots_on_target
arsenal_away$opp_shots_on_target = arsenal_away$home_shots_on_target

arsenal_home$away_team = "Opp"
arsenal_away$home_team = "Opp"
arsenal_data = rbind(arsenal_home,arsenal_away)



#plot

man_united_possession = append(man_united_data$man_united_possession,man_united_data$opp_possession)
man_united_touches = append(man_united_data$man_united_touches,man_united_data$opp_touches)

man_united_possession_Touches = data.frame(man_united_possession,man_united_touches)
man_united_possession_Touches$team = "Opp"
man_united_possession_Touches$team[1:length(man_united_data$man_united_possession)] = "Man United"

p1 = ggplot(data = man_united_possession_Touches) + 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_touches, color = team))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Touches of Man United")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_united_p_t.png",width = 12,height = 9)


p1 = ggplot(data = man_united_data)+ 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_touches))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Man United")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_united_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_touches))+ xlab("1 - Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Man United Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_united_p_t_c.png",width = 9,height = 12)


man_city_possession = append(man_city_data$man_city_possession,man_city_data$opp_possession)
man_city_touches = append(man_city_data$man_city_touches,man_city_data$opp_touches)

man_city_possession_Touches = data.frame(man_city_possession,man_city_touches)
man_city_possession_Touches$team = "Opp"
man_city_possession_Touches$team[1:length(man_city_data$man_city_possession)] = "Man city"

p1 = ggplot(data = man_city_possession_Touches) + 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_touches, color = team))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Touches of Man City")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_city_p_t.png",width = 12,height = 9)


p1 = ggplot(data = man_city_data)+ 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_touches))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Man City")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_city_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_touches))+ xlab("1 - Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Man City Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_city_p_t_c.png",width = 9,height = 12)



chelsea_possession = append(chelsea_data$chelsea_possession,chelsea_data$opp_possession)
chelsea_touches = append(chelsea_data$chelsea_touches,chelsea_data$opp_touches)

chelsea_possession_Touches = data.frame(chelsea_possession,chelsea_touches)
chelsea_possession_Touches$team = "Opp"
chelsea_possession_Touches$team[1:length(chelsea_data$chelsea_possession)] = "Chelsea"

p1 = ggplot(data = chelsea_possession_Touches) + 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_touches, color = team))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Touches of Chelsea")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "chelsea_p_t.png",width = 12,height = 9)


p1 = ggplot(data = chelsea_data)+ 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_touches))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Chelsea")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = chelsea_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_touches))+ xlab("1 - Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Chelsea Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "chelsea_p_t_c.png",width = 9,height = 12)



arsenal_possession = append(arsenal_data$arsenal_possession,arsenal_data$opp_possession)
arsenal_touches = append(arsenal_data$arsenal_touches,arsenal_data$opp_touches)

arsenal_possession_Touches = data.frame(arsenal_possession,arsenal_touches)
arsenal_possession_Touches$team = "Opp"
arsenal_possession_Touches$team[1:length(arsenal_data$arsenal_possession)] = "Arsenal"

p1 = ggplot(data = arsenal_possession_Touches) + 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_touches, color = team))+ xlab("1 - Possession in the match")+
  ylab("Touch in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Touches of Arsenal")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "arsenal_p_t.png",width = 12,height = 9)


p1 = ggplot(data = arsenal_data)+ 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_touches))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Arsenal")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = arsenal_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_touches))+ xlab("Possession in the match")+
  ylab("Touch in the match")+ ggtitle("Possession VS Touches of Arsenal Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "arsenal_p_t_c.png",width = 9,height = 12)


# Possession VS Pass

man_united_possession = append(man_united_data$man_united_possession,man_united_data$opp_possession)
man_united_passes = append(man_united_data$man_united_passes,man_united_data$opp_passes)

man_united_possession_passes = data.frame(man_united_possession,man_united_passes)
man_united_possession_passes$team = "Opp"
man_united_possession_passes$team[1:length(man_united_data$man_united_possession)] = "Man United"

p1 = ggplot(data = man_united_possession_passes) + 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_passes, color = team))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Passes of Man United")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_united_p_p.png",width = 12,height = 9)

p1 = ggplot(data = man_united_data)+ 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_passes))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ ggtitle("Possession VS passes of Man_United")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_united_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_passes))+ xlab("1 - Possession in the match")+
  ylab("Passess in the match")+ ggtitle("Possession VS passes of Man_United Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_united_p_p_c.png",width = 9,height = 12)


man_city_possession = append(man_city_data$man_city_possession,man_city_data$opp_possession)
man_city_passes = append(man_city_data$man_city_passes,man_city_data$opp_passes)

man_city_possession_passes = data.frame(man_city_possession,man_city_passes)
man_city_possession_passes$team = "Opp"
man_city_possession_passes$team[1:length(man_city_data$man_city_possession)] = "Man city"

p1 = ggplot(data = man_city_possession_passes) + 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_passes, color = team))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Passes of Man City")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_city_p_p.png",width = 12,height = 9)


p1 = ggplot(data = man_city_data)+ 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_passes))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ ggtitle("Possession VS passes of man_city")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_city_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_passes))+ xlab("1 - Possession in the match")+
  ylab("Passess in the match")+ ggtitle("Possession VS passes of man_city Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_city_p_p_c.png",width = 9,height = 12)



chelsea_possession = append(chelsea_data$chelsea_possession,chelsea_data$opp_possession)
chelsea_passes = append(chelsea_data$chelsea_passes,chelsea_data$opp_passes)

chelsea_possession_passes = data.frame(chelsea_possession,chelsea_passes)
chelsea_possession_passes$team = "Opp"
chelsea_possession_passes$team[1:length(chelsea_data$chelsea_possession)] = "Chelsea"

p1 = ggplot(data = chelsea_possession_passes) + 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_passes, color = team))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Passes of Chelsea")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "chelsea_p_p.png",width = 12,height = 9)


p1 = ggplot(data = chelsea_data)+ 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_passes))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ ggtitle("Possession VS passes of chelsea")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = chelsea_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_passes))+ xlab("1 - Possession in the match")+
  ylab("Passess in the match")+ ggtitle("Possession VS passes of chelsea Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "chelsea_p_p_c.png",width = 9,height = 12)



arsenal_possession = append(arsenal_data$arsenal_possession,arsenal_data$opp_possession)
arsenal_passes = append(arsenal_data$arsenal_passes,arsenal_data$opp_passes)

arsenal_possession_passes = data.frame(arsenal_possession,arsenal_passes)
arsenal_possession_passes$team = "Opp"
arsenal_possession_passes$team[1:length(arsenal_data$arsenal_possession)] = "Arsenal"

p1 = ggplot(data = arsenal_possession_passes) + 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_passes, color = team))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ labs(colour = "Team")+ ggtitle("Possession VS Passes of Arsenal")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "arsenal_p_p.png",width = 12,height = 9)

p1 = ggplot(data = arsenal_data)+ 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_passes))+ xlab("Possession in the match")+
  ylab("Passes in the match")+ ggtitle("Possession VS passes of arsenal")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = arsenal_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_passes))+ xlab("1 - Possession in the match")+
  ylab("Passess in the match")+ ggtitle("Possession VS passes of arsenal Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "arsenal_p_p_c.png",width = 9,height = 12)


#Possession VS shot acc

man_united_possession = append(man_united_data$man_united_possession,man_united_data$opp_possession)
man_united_shot_acc = append(man_united_data$man_united_shot_acc,man_united_data$opp_shot_acc)

man_united_possession_shot_acc = data.frame(man_united_possession,man_united_shot_acc)
man_united_possession_shot_acc$team = "Opp"
man_united_possession_shot_acc$team[1:length(man_united_data$man_united_possession)] = "Man United"

p1 = ggplot(data = man_united_possession_shot_acc) + 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_shot_acc, color = team))+ xlab("Possession in the match")+
  ylab("shot_acc in the match")+ labs(colour = "Team")+ ggtitle("Possession VS shot_acc of Man United")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_united_p_s.png",width = 12,height = 9)




man_city_possession = append(man_city_data$man_city_possession,man_city_data$opp_possession)
man_city_shot_acc = append(man_city_data$man_city_shot_acc,man_city_data$opp_shot_acc)

man_city_possession_shot_acc = data.frame(man_city_possession,man_city_shot_acc)
man_city_possession_shot_acc$team = "Opp"
man_city_possession_shot_acc$team[1:length(man_city_data$man_city_possession)] = "Man city"

p1 = ggplot(data = man_city_possession_shot_acc) + 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_shot_acc, color = team))+ xlab("Possession in the match")+
  ylab("shot_acc in the match")+ labs(colour = "Team")+ ggtitle("Possession VS shot_acc of Man City")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_city_p_s.png",width = 12,height = 9)




chelsea_possession = append(chelsea_data$chelsea_possession,chelsea_data$opp_possession)
chelsea_shot_acc = append(chelsea_data$chelsea_shot_acc,chelsea_data$opp_shot_acc)

chelsea_possession_shot_acc = data.frame(chelsea_possession,chelsea_shot_acc)
chelsea_possession_shot_acc$team = "Opp"
chelsea_possession_shot_acc$team[1:length(chelsea_data$chelsea_possession)] = "Chelsea"

p1 = ggplot(data = chelsea_possession_shot_acc) + 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_shot_acc, color = team))+ xlab("Possession in the match")+
  ylab("shot_acc in the match")+ labs(colour = "Team")+ ggtitle("Possession VS shot_acc of Chelsea")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "chelsea_p_s.png",width = 12,height = 9)



arsenal_possession = append(arsenal_data$arsenal_possession,arsenal_data$opp_possession)
arsenal_shot_acc = append(arsenal_data$arsenal_shot_acc,arsenal_data$opp_shot_acc)

arsenal_possession_shot_acc = data.frame(arsenal_possession,arsenal_shot_acc)
arsenal_possession_shot_acc$team = "Opp"
arsenal_possession_shot_acc$team[1:length(arsenal_data$arsenal_possession)] = "Arsenal"

p1 = ggplot(data = arsenal_possession_shot_acc) + 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_shot_acc, color = team))+ xlab("Possession in the match")+
  ylab("shot_acc in the match")+ labs(colour = "Team")+ ggtitle("Possession VS shot_acc of Arsenal")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "arsenal_p_s.png",width = 12,height = 9)



#poession vs fouls

man_united_possession = append(man_united_data$man_united_possession,man_united_data$opp_possession)
man_united_fouls = append(man_united_data$man_united_fouls,man_united_data$opp_fouls)

man_united_possession_fouls = data.frame(man_united_possession,man_united_fouls)
man_united_possession_fouls$team = "Opp"
man_united_possession_fouls$team[1:length(man_united_data$man_united_possession)] = "Man United"

p1 = ggplot(data = man_united_possession_fouls) + 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_fouls, color = team))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ labs(colour = "Team")+ ggtitle("Possession VS fouls of Man United")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_united_p_f.png",width = 12,height = 9)

p1 = ggplot(data = man_united_data)+ 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_fouls))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ ggtitle("Possession VS fouls of Man_United")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_united_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_fouls))+ xlab("1 - Possession in the match")+
  ylab("foulss in the match")+ ggtitle("Possession VS fouls of Man_United Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_united_p_f_c.png",width = 9,height = 12)


man_city_possession = append(man_city_data$man_city_possession,man_city_data$opp_possession)
man_city_fouls = append(man_city_data$man_city_fouls,man_city_data$opp_fouls)

man_city_possession_fouls = data.frame(man_city_possession,man_city_fouls)
man_city_possession_fouls$team = "Opp"
man_city_possession_fouls$team[1:length(man_city_data$man_city_possession)] = "Man city"

p1 = ggplot(data = man_city_possession_fouls) + 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_fouls, color = team))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ labs(colour = "Team")+ ggtitle("Possession VS fouls of Man City")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_city_p_f.png",width = 12,height = 9)

p1 = ggplot(data = man_city_data)+ 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_fouls))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ ggtitle("Possession VS fouls of man_city")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_city_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_fouls))+ xlab("1 - Possession in the match")+
  ylab("foulss in the match")+ ggtitle("Possession VS fouls of man_city Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_city_p_f_c.png",width = 9,height = 12)


chelsea_possession = append(chelsea_data$chelsea_possession,chelsea_data$opp_possession)
chelsea_fouls = append(chelsea_data$chelsea_fouls,chelsea_data$opp_fouls)

chelsea_possession_fouls = data.frame(chelsea_possession,chelsea_fouls)
chelsea_possession_fouls$team = "Opp"
chelsea_possession_fouls$team[1:length(chelsea_data$chelsea_possession)] = "Chelsea"

p1 = ggplot(data = chelsea_possession_fouls) + 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_fouls, color = team))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ labs(colour = "Team")+ ggtitle("Possession VS fouls of Chelsea")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "chelsea_p_f.png",width = 12,height = 9)

p1 = ggplot(data = chelsea_data)+ 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_fouls))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ ggtitle("Possession VS fouls of chelsea")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = chelsea_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_fouls))+ xlab("1 - Possession in the match")+
  ylab("foulss in the match")+ ggtitle("Possession VS fouls of chelsea Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "chelsea_p_f_c.png",width = 9,height = 12)



arsenal_possession = append(arsenal_data$arsenal_possession,arsenal_data$opp_possession)
arsenal_fouls = append(arsenal_data$arsenal_fouls,arsenal_data$opp_fouls)

arsenal_possession_fouls = data.frame(arsenal_possession,arsenal_fouls)
arsenal_possession_fouls$team = "Opp"
arsenal_possession_fouls$team[1:length(arsenal_data$arsenal_possession)] = "Arsenal"

p1 = ggplot(data = arsenal_possession_fouls) + 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_fouls, color = team))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ labs(colour = "Team")+ ggtitle("Possession VS fouls of Arsenal")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "arsenal_p_f.png",width = 12,height = 9)

p1 = ggplot(data = arsenal_data)+ 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_fouls))+ xlab("Possession in the match")+
  ylab("fouls in the match")+ ggtitle("Possession VS fouls of arsenal")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = arsenal_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_fouls))+ xlab("1 - Possession in the match")+
  ylab("foulss in the match")+ ggtitle("Possession VS fouls of arsenal Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "arsenal_p_f_c.png",width = 9,height = 12)

#Poessions VS cards

man_united_possession = append(man_united_data$man_united_possession,man_united_data$opp_possession)
man_united_cards = append(man_united_data$man_united_cards,man_united_data$opp_cards)

man_united_possession_cards = data.frame(man_united_possession,man_united_cards)
man_united_possession_cards$team = "Opp"
man_united_possession_cards$team[1:length(man_united_data$man_united_possession)] = "Man United"

p1 = ggplot(data = man_united_possession_cards) + 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_cards, color = team))+ xlab("Possession in the match")+
  ylab("cards in the match")+ labs(colour = "Team")+ ggtitle("Possession VS cards of Man United")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_united_p_c.png",width = 12,height = 9)



p1 = ggplot(data = man_united_data)+ 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_cards))+ xlab("Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of Man_United")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_united_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_cards))+ xlab("1 - Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of Man_United Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_united_p_c_c.png",width = 9,height = 12)



man_city_possession = append(man_city_data$man_city_possession,man_city_data$opp_possession)
man_city_cards = append(man_city_data$man_city_cards,man_city_data$opp_cards)

man_city_possession_cards = data.frame(man_city_possession,man_city_cards)
man_city_possession_cards$team = "Opp"
man_city_possession_cards$team[1:length(man_city_data$man_city_possession)] = "Man city"

p1 = ggplot(data = man_city_possession_cards) + 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_cards, color = team))+ xlab("Possession in the match")+
  ylab("cards in the match")+ labs(colour = "Team")+ ggtitle("Possession VS cards of Man City")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_city_p_c.png",width = 12,height = 9)

p1 = ggplot(data = man_city_data)+ 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_cards))+ xlab("Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of man_city")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = man_city_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_cards))+ xlab("1 - Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of man_city Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "man_city_p_c_c.png",width = 9,height = 12)


chelsea_possession = append(chelsea_data$chelsea_possession,chelsea_data$opp_possession)
chelsea_cards = append(chelsea_data$chelsea_cards,chelsea_data$opp_cards)

chelsea_possession_cards = data.frame(chelsea_possession,chelsea_cards)
chelsea_possession_cards$team = "Opp"
chelsea_possession_cards$team[1:length(chelsea_data$chelsea_possession)] = "Chelsea"

p1 = ggplot(data = chelsea_possession_cards) + 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_cards, color = team))+ xlab("Possession in the match")+
  ylab("cards in the match")+ labs(colour = "Team")+ ggtitle("Possession VS cards of Chelsea")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "chelsea_p_c.png",width = 12,height = 9)

p1 = ggplot(data = chelsea_data)+ 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_cards))+ xlab("Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of chelsea")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = chelsea_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_cards))+ xlab("1 - Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of chelsea Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "chelsea_p_c_c.png",width = 9,height = 12)


arsenal_possession = append(arsenal_data$arsenal_possession,arsenal_data$opp_possession)
arsenal_cards = append(arsenal_data$arsenal_cards,arsenal_data$opp_cards)

arsenal_possession_cards = data.frame(arsenal_possession,arsenal_cards)
arsenal_possession_cards$team = "Opp"
arsenal_possession_cards$team[1:length(arsenal_data$arsenal_possession)] = "Arsenal"

p1 = ggplot(data = arsenal_possession_cards) + 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_cards, color = team))+ xlab("Possession in the match")+
  ylab("cards in the match")+ labs(colour = "Team")+ ggtitle("Possession VS cards of Arsenal")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "arsenal_p_c.png",width = 12,height = 9)

p1 = ggplot(data = arsenal_data)+ 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_cards))+ xlab("Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of arsenal")+theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = arsenal_data) + 
  geom_point(mapping = aes(x = 100 - opp_possession, y = opp_cards))+ xlab("1 - Possession in the match")+
  ylab("cards in the match")+ ggtitle("Possession VS cards of arsenal Opp")+theme(plot.title = element_text(hjust = 0.5))

p = p1 / p2

ggsave (p,filename = "arsenal_p_c_c.png",width = 9,height = 12)

#po VS corners

man_united_possession = append(man_united_data$man_united_possession,man_united_data$opp_possession)
man_united_corners = append(man_united_data$man_united_corners,man_united_data$opp_corners)

man_united_possession_corners = data.frame(man_united_possession,man_united_corners)
man_united_possession_corners$team = "Opp"
man_united_possession_corners$team[1:length(man_united_data$man_united_possession)] = "Man United"

p1 = ggplot(data = man_united_possession_corners) + 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_corners, color = team))+ xlab("Possession in the match")+
  ylab("corners in the match")+ labs(colour = "Team")+ ggtitle("Possession VS corners of Man United")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_united_p_cor.png",width = 12,height = 9)




man_city_possession = append(man_city_data$man_city_possession,man_city_data$opp_possession)
man_city_corners = append(man_city_data$man_city_corners,man_city_data$opp_corners)

man_city_possession_corners = data.frame(man_city_possession,man_city_corners)
man_city_possession_corners$team = "Opp"
man_city_possession_corners$team[1:length(man_city_data$man_city_possession)] = "Man city"

p1 = ggplot(data = man_city_possession_corners) + 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_corners, color = team))+ xlab("Possession in the match")+
  ylab("corners in the match")+ labs(colour = "Team")+ ggtitle("Possession VS corners of Man City")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_city_p_cor.png",width = 12,height = 9)




chelsea_possession = append(chelsea_data$chelsea_possession,chelsea_data$opp_possession)
chelsea_corners = append(chelsea_data$chelsea_corners,chelsea_data$opp_corners)

chelsea_possession_corners = data.frame(chelsea_possession,chelsea_corners)
chelsea_possession_corners$team = "Opp"
chelsea_possession_corners$team[1:length(chelsea_data$chelsea_possession)] = "Chelsea"

p1 = ggplot(data = chelsea_possession_corners) + 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_corners, color = team))+ xlab("Possession in the match")+
  ylab("corners in the match")+ labs(colour = "Team")+ ggtitle("Possession VS corners of Chelsea")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "chelsea_p_cor.png",width = 12,height = 9)



arsenal_possession = append(arsenal_data$arsenal_possession,arsenal_data$opp_possession)
arsenal_corners = append(arsenal_data$arsenal_corners,arsenal_data$opp_corners)

arsenal_possession_corners = data.frame(arsenal_possession,arsenal_corners)
arsenal_possession_corners$team = "Opp"
arsenal_possession_corners$team[1:length(arsenal_data$arsenal_possession)] = "Arsenal"

p1 = ggplot(data = arsenal_possession_corners) + 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_corners, color = team))+ xlab("Possession in the match")+
  ylab("corners in the match")+ labs(colour = "Team")+ ggtitle("Possession VS corners of Arsenal")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "arsenal_p_cor.png",width = 12,height = 9)


#po vs clearance

man_united_possession = append(man_united_data$man_united_possession,man_united_data$opp_possession)
man_united_clearances = append(man_united_data$man_united_clearances,man_united_data$opp_clearances)

man_united_possession_clearances = data.frame(man_united_possession,man_united_clearances)
man_united_possession_clearances$team = "Opp"
man_united_possession_clearances$team[1:length(man_united_data$man_united_possession)] = "Man United"

p1 = ggplot(data = man_united_possession_clearances) + 
  geom_point(mapping = aes(x = man_united_possession, y = man_united_clearances, color = team))+ xlab("Possession in the match")+
  ylab("clearances in the match")+ labs(colour = "Team")+ ggtitle("Possession VS clearances of Man United")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_united_p_cl.png",width = 12,height = 9)




man_city_possession = append(man_city_data$man_city_possession,man_city_data$opp_possession)
man_city_clearances = append(man_city_data$man_city_clearances,man_city_data$opp_clearances)

man_city_possession_clearances = data.frame(man_city_possession,man_city_clearances)
man_city_possession_clearances$team = "Opp"
man_city_possession_clearances$team[1:length(man_city_data$man_city_possession)] = "Man city"

p1 = ggplot(data = man_city_possession_clearances) + 
  geom_point(mapping = aes(x = man_city_possession, y = man_city_clearances, color = team))+ xlab("Possession in the match")+
  ylab("clearances in the match")+ labs(colour = "Team")+ ggtitle("Possession VS clearances of Man City")+theme(plot.title = element_text(hjust = 0.5))
p1
ggsave (p1,filename = "man_city_p_cl.png",width = 12,height = 9)




chelsea_possession = append(chelsea_data$chelsea_possession,chelsea_data$opp_possession)
chelsea_clearances = append(chelsea_data$chelsea_clearances,chelsea_data$opp_clearances)

chelsea_possession_clearances = data.frame(chelsea_possession,chelsea_clearances)
chelsea_possession_clearances$team = "Opp"
chelsea_possession_clearances$team[1:length(chelsea_data$chelsea_possession)] = "Chelsea"

p1 = ggplot(data = chelsea_possession_clearances) + 
  geom_point(mapping = aes(x = chelsea_possession, y = chelsea_clearances, color = team))+ xlab("Possession in the match")+
  ylab("clearances in the match")+ labs(colour = "Team")+ ggtitle("Possession VS clearances of Chelsea")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "chelsea_p_cl.png",width = 12,height = 9)



arsenal_possession = append(arsenal_data$arsenal_possession,arsenal_data$opp_possession)
arsenal_clearances = append(arsenal_data$arsenal_clearances,arsenal_data$opp_clearances)

arsenal_possession_clearances = data.frame(arsenal_possession,arsenal_clearances)
arsenal_possession_clearances$team = "Opp"
arsenal_possession_clearances$team[1:length(arsenal_data$arsenal_possession)] = "Arsenal"

p1 = ggplot(data = arsenal_possession_clearances) + 
  geom_point(mapping = aes(x = arsenal_possession, y = arsenal_clearances, color = team))+ xlab("Possession in the match")+
  ylab("clearances in the match")+ labs(colour = "Team")+ ggtitle("Possession VS clearances of Arsenal")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "arsenal_p_cl.png",width = 12,height = 9)

man_united_shots = c()
man_city_shots = c()
chelsea_shots = c()
arsenal_shots = c()


for (i in unique(full_data$season)){
  man_united_s = subset(man_united_data, season == i)
  man_city_s = subset(man_city_data, season == i)
  chelsea_s = subset(chelsea_data, season == i)
  arsenal_s = subset(arsenal_data, season == i)
  man_united_shots = append(man_united_shots,sum(man_united_s$man_united_shots))
  man_city_shots = append(man_city_shots,sum(man_city_s$man_city_shots))
  chelsea_shots = append(chelsea_shots,sum(chelsea_s$chelsea_shots))
  arsenal_shots = append(arsenal_shots,sum(arsenal_s$arsenal_shots))
}

season = unique(full_data$season)

for (i in 1:2){
  season = append(season,season)
}

shots = c()
shots = append(shots,man_united_shots)
shots = append(shots,man_city_shots)
shots = append(shots,chelsea_shots)
shots = append(shots,arsenal_shots)

team = c()
team = append(team,"man_united")
team = append(team,"man_city")
team = append(team,"chelsea")
team = append(team,"arsenal")

shots_data = data.frame(season,team,shots)


p1<-ggplot(data=shots_data, mapping=aes(x = season, y = shots,fill=team))+geom_bar(stat="identity",position=position_dodge(0.75))+ylab("goals difference")+ labs(colour = "Team")+ ggtitle("Goals difference for each season of four teams")+theme(plot.title = element_text(hjust = 0.5))
ggsave (p1,filename = "shots_per_season.png",width = 12,height = 9)











man_united_data$ceil_possession = 0
man_united_data$ceil_possession = ceiling(man_united_data$man_united_possession/10)*10

p1 <- ggplot(data = man_united_data,aes(x = ceil_possession, fill = man_united_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of man_united ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))



man_united_data$opp_ceil_possession = 0
man_united_data$opp_ceil_possession = ceiling(man_united_data$opp_possession/10)*10

p2 <- ggplot(data = man_united_data,aes(x = opp_ceil_possession, fill = man_united_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of man_united Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("man_united_opp_p_s_c.png",p,width = 18,height = 9)



man_city_data$ceil_possession = 0
man_city_data$ceil_possession = ceiling(man_city_data$man_city_possession/10)*10

p1 <- ggplot(data = man_city_data,aes(x = ceil_possession, fill = man_city_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of man_city ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


man_city_data$opp_ceil_possession = 0
man_city_data$opp_ceil_possession = ceiling(man_city_data$opp_possession/10)*10

p2 <- ggplot(data = man_city_data,aes(x = opp_ceil_possession, fill = man_city_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of man_city Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("man_city_opp_p_s_c.png",p,width = 18,height = 9)


chelsea_data$ceil_possession = 0
chelsea_data$ceil_possession = ceiling(chelsea_data$chelsea_possession/10)*10

p1 <- ggplot(data = chelsea_data,aes(x = ceil_possession, fill = chelsea_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of chelsea ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


chelsea_data$opp_ceil_possession = 0
chelsea_data$opp_ceil_possession = ceiling(chelsea_data$opp_possession/10)*10

p2 <- ggplot(data = chelsea_data,aes(x = opp_ceil_possession, fill = chelsea_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of chelsea Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("chelsea_opp_p_s_c.png",p,width = 18,height = 9)


arsenal_data$ceil_possession = 0
arsenal_data$ceil_possession = ceiling(arsenal_data$arsenal_possession/10)*10

p1 <- ggplot(data = arsenal_data,aes(x = ceil_possession, fill = arsenal_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of arsenal ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))



arsenal_data$opp_ceil_possession = 0
arsenal_data$opp_ceil_possession = ceiling(arsenal_data$opp_possession/10)*10

p2 <- ggplot(data = arsenal_data,aes(x = opp_ceil_possession, fill = arsenal_shots)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots count")+ ggtitle("Possession VS Shots of arsenal Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("arsenal_opp_p_s_c.png",p,width = 18,height = 9)




man_united_data$ceil_possession = 0
man_united_data$ceil_possession = ceiling(man_united_data$man_united_possession/10)*10

p1 <- ggplot(data = man_united_data,aes(x = ceil_possession, fill = man_united_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of man_united ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


man_united_data$opp_ceil_possession = 0
man_united_data$opp_ceil_possession = ceiling(man_united_data$opp_possession/10)*10

p2 <- ggplot(data = man_united_data,aes(x = opp_ceil_possession, fill = man_united_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of man_united Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("man_united_opp_p_st_c.png",p,width = 18,height = 9)



man_city_data$ceil_possession = 0
man_city_data$ceil_possession = ceiling(man_city_data$man_city_possession/10)*10

p1 <- ggplot(data = man_city_data,aes(x = ceil_possession, fill = man_city_data$man_city_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of man_city ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


man_city_data$opp_ceil_possession = 0
man_city_data$opp_ceil_possession = ceiling(man_city_data$opp_possession/10)*10

p2 <- ggplot(data = man_city_data,aes(x = opp_ceil_possession, fill = man_city_data$man_city_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of man_city Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("man_city_opp_p_st_c.png",p,width = 18,height = 9)


chelsea_data$ceil_possession = 0
chelsea_data$ceil_possession = ceiling(chelsea_data$chelsea_possession/10)*10

p1 <- ggplot(data = chelsea_data,aes(x = ceil_possession, fill = chelsea_data$chelsea_shots_on_target)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of chelsea ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


chelsea_data$opp_ceil_possession = 0
chelsea_data$opp_ceil_possession = ceiling(chelsea_data$opp_possession/10)*10

p2 <- ggplot(data = chelsea_data,aes(x = opp_ceil_possession, fill = chelsea_data$chelsea_shots_on_target)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of chelsea Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("chelsea_opp_p_st_c.png",p,width = 18,height = 9)


arsenal_data$ceil_possession = 0
arsenal_data$ceil_possession = ceiling(arsenal_data$arsenal_possession/10)*10

p1 <- ggplot(data = arsenal_data,aes(x = ceil_possession, fill = arsenal_data$arsenal_shots_on_target)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of arsenal ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


arsenal_data$opp_ceil_possession = 0
arsenal_data$opp_ceil_possession = ceiling(arsenal_data$opp_possession/10)*10

p2 <- ggplot(data = arsenal_data,aes(x = opp_ceil_possession, fill = arsenal_data$arsenal_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("shots_on_target count")+ ggtitle("Possession VS shots_on_target of arsenal Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 + p2

ggsave("arsenal_opp_p_st_c.png",p,width = 18,height = 9)







man_united_data$ceil_possession = 0
man_united_data$ceil_possession = ceiling(man_united_data$man_united_possession/10)*10

p1 <- ggplot(data = man_united_data,aes(x = ceil_possession, fill = man_united_fouls)) +geom_bar(position = "dodge", color = "black") +xlim(c(-5,110))+geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_united ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))

p1

man_united_data$opp_ceil_possession = 0
man_united_data$opp_ceil_possession = ceiling(man_united_data$opp_possession/10)*10

p2 <- ggplot(data = man_united_data,aes(x = opp_ceil_possession, fill = man_united_fouls)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_united Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"))+xlim(c(-5,110))+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90), labels = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"))

p2
p = p1 / p2

ggsave("man_united_opp_p_f_c.png",p,width = 9,height = 12)



man_city_data$ceil_possession = 0
man_city_data$ceil_possession = ceiling(man_city_data$man_city_possession/10)*10

p1 <- ggplot(data = man_city_data,aes(x = ceil_possession, fill = man_city_fouls)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_city ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


man_city_data$opp_ceil_possession = 0
man_city_data$opp_ceil_possession = ceiling(man_city_data$opp_possession/10)*10

p2 <- ggplot(data = man_city_data,aes(x = opp_ceil_possession, fill = man_city_fouls)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of man_city Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1 / p2

ggsave("man_city_opp_p_f_c.png",p,width = 9,height = 12)


chelsea_data$ceil_possession = 0
chelsea_data$ceil_possession = ceiling(chelsea_data$chelsea_possession/10)*10

p1 <- ggplot(data = chelsea_data,aes(x = ceil_possession, fill = chelsea_fouls)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of chelsea ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


chelsea_data$opp_ceil_possession = 0
chelsea_data$opp_ceil_possession = ceiling(chelsea_data$opp_possession/10)*10

p2 <- ggplot(data = chelsea_data,aes(x = opp_ceil_possession, fill = chelsea_fouls)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of chelsea Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1/p2

ggsave("chelsea_opp_p_f_c.png",p,width = 9,height = 12)


arsenal_data$ceil_possession = 0
arsenal_data$ceil_possession = ceiling(arsenal_data$arsenal_possession/10)*10

p1 <- ggplot(data = arsenal_data,aes(x = ceil_possession, fill = arsenal_fouls)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of arsenal ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))



arsenal_data$opp_ceil_possession = 0
arsenal_data$opp_ceil_possession = ceiling(arsenal_data$opp_possession/10)*10

p2 <- ggplot(data = arsenal_data,aes(x = opp_ceil_possession, fill = arsenal_fouls)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("fouls count")+ ggtitle("Possession VS fouls of arsenal Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1/p2

ggsave("arsenal_opp_p_f_c.png",p,width = 9,height = 12)




man_united_data$ceil_possession = 0
man_united_data$ceil_possession = ceiling(man_united_data$man_united_possession/10)*10

p1 <- ggplot(data = man_united_data,aes(x = ceil_possession, fill = man_united_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_united ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


man_united_data$opp_ceil_possession = 0
man_united_data$opp_ceil_possession = ceiling(man_united_data$opp_possession/10)*10

p2 <- ggplot(data = man_united_data,aes(x = opp_ceil_possession, fill = man_united_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_united Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1/p2

ggsave("man_united_opp_p_c_c.png",p,width = 9,height = 12)



man_city_data$ceil_possession = 0
man_city_data$ceil_possession = ceiling(man_city_data$man_city_possession/10)*10

p1 <- ggplot(data = man_city_data,aes(x = ceil_possession, fill = man_city_data$man_city_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_city ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


man_city_data$opp_ceil_possession = 0
man_city_data$opp_ceil_possession = ceiling(man_city_data$opp_possession/10)*10

p2 <- ggplot(data = man_city_data,aes(x = opp_ceil_possession, fill = man_city_data$man_city_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of man_city Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1/p2

ggsave("man_city_opp_p_c_c.png",p,width = 9,height = 12)


chelsea_data$ceil_possession = 0
chelsea_data$ceil_possession = ceiling(chelsea_data$chelsea_possession/10)*10

p1 <- ggplot(data = chelsea_data,aes(x = ceil_possession, fill = chelsea_data$chelsea_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of chelsea ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


chelsea_data$opp_ceil_possession = 0
chelsea_data$opp_ceil_possession = ceiling(chelsea_data$opp_possession/10)*10

p2 <- ggplot(data = chelsea_data,aes(x = opp_ceil_possession, fill = chelsea_data$chelsea_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of chelsea Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1/p2

ggsave("chelsea_opp_p_c_c.png",p,width = 9,height = 12)


arsenal_data$ceil_possession = 0
arsenal_data$ceil_possession = ceiling(arsenal_data$arsenal_possession/10)*10

p1 <- ggplot(data = arsenal_data,aes(x = ceil_possession, fill = arsenal_data$arsenal_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of arsenal ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


arsenal_data$opp_ceil_possession = 0
arsenal_data$opp_ceil_possession = ceiling(arsenal_data$opp_possession/10)*10

p2 <- ggplot(data = arsenal_data,aes(x = opp_ceil_possession, fill = arsenal_data$arsenal_cards)) +geom_bar(position = "dodge", color = "black") +geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +xlab("ceiling possessions")+ylab("cards count")+ ggtitle("Possession VS cards of arsenal Opp ")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(30,40,50,60,70,80), labels = c("21-30","31-40","41-50","51-60","61-70","71-80"))


p = p1/p2

ggsave("arsenal_opp_p_c_c.png",p,width = 9,height = 12)



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

