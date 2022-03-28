RACES <- read.csv('Oracle data/RACES.csv')
library(ggplot2)

head(RACES)

ggplot(RACES, aes(x = WEATHER_WET))+
  geom_bar(aes(color = OVERTAKEN_POSITIONS_TOTAL))
