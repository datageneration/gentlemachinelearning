## Gentile Introduction to Machine Learning

# Package readr imports CSV data
# read_csv() in tidyverse simplifies the csv import
install.packages("readr")
install.packages("tidyverse")
library(readr)
library(tidyverse)

# Read in COVID19 data from NYTimes API
corcounty_nyt <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
corstate_nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", stringsAsFactors = FALSE)
library(ggplot2)
top5=c("New York", "New Jersey","California","Florida", "Texas")
attach(corstate_nyt)
top5data <- corstate_nyt[which(state == top5),]
texasdata = corstate_nyt[which(state == "Texas"),]
njdata = corstate_nyt[which(state == "New Jersey"),]
nydata = corstate_nyt[which(state == "New York"),]
fldata = corstate_nyt[which(state == "Florida"),]
cadata = corstate_nyt[which(state == "California"),]

# Use the %in% operator to identify if an element belongs to a vector.
top5data <- corstate_nyt[which(state %in% top5),]

ggplot(data=top5data, aes(x=date, y=cases, col=state)) +
  geom_line() +
  geom_point(cex=.3) +
  theme_bw()

