library(ggplot2)
library(DataExplorer)
library(patchwork)
library(reshape2)



setwd('C:\\Users\\duli\\Documents\\20210614')

full_data = read.csv('df_full_premierleague.csv')

plot_missing(full_data)

full_data <- na.omit(full_data)



#Man United data processing

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

#Man city data processing

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



#Chelsea data processing

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


#Arsenal data processing

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