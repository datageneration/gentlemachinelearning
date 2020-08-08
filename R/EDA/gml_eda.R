## Gentle Machine Learning
## Exploratary Data Analysis
## Adapted from Grolemund, Garrett, and Hadley Wickham. 2018 
## R for data science. Ch.7 (https://r4ds.had.co.nz/).

# install.packages("tidyverse")
library(tidyverse)

# Plot diamonds data
attach(diamonds)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) +
  theme_bw()

# Simple table
# %>% forward piping operator - forward programming
diamonds %>% 
  count(cut)

# Frequency table with chart
# install.packages("descr")
library(descr)
freq(diamonds$cut)
library(RColorBrewer)

# What is the carat variable?
descr(diamonds$carat)

# A histogram divides the x-axis into equally spaced bins and then uses 
# the height of a bar to display the number of observations per each bin.
hist(carat)

# Another look
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5) +
  theme_bw()  # Why this looks different from hist(carat)

# Another another look
freq(diamonds$carat)

# Can you build a histogram for cut?  Why not?

# Look closer at smaller carat diamonds (left portion from previous histogram)

smaller <- diamonds %>% 
  filter(carat < 3)

# Set small binwidth
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1) + theme_bw()

# Polygon
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1) + theme_bw() +
  scale_color_brewer(palette = "Spectral") 

# mpg data

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() + theme_bw()

# To make the trend easier to see, we can reorder class based on the median value of hwy

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip() + theme_bw()

# Continuous variables

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) + theme_bw()

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100) + theme_bw()

