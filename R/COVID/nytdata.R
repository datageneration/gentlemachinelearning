
## Gentile Introduction to Machine Learning

install.packages("readr")
install.packages("tidyverse")

library(readr)
library(tidyverse)
corcounty_nyt <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
corstate_nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", stringsAsFactors = FALSE)
library(ggplot2)
top5=c("New York", "New Jersey","California","Florida", "Texas")
attach(corstate_nyt)
top5data <- corstate_nyt[which(state == top5),]
texasdata = corstate_nyt[which(state == "Texas"),]

ggplot(data=top5data, aes(x=date, y=cases, col=state)) +
  geom_line()+
  geom_point()

