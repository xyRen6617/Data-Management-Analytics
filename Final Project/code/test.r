man_united_data$ceil_possession = 0
man_united_data$ceil_possession = ceiling(man_united_data$man_united_possession/10)*10



p <- ggplot(data = man_united_data,aes(x = ceil_possession, fill = Purchased.Bike)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 1), vjust = 2) +
  theme(legend.position = c(.8,.9))