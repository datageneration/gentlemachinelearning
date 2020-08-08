## Gentle Machine Learning
## Scatter plot matrix
## Extracted from Alexander C. Tan, Karl Ho & Cal Clark. 2020. The political 
## economy of Taiwan’s regional relations, Asian Affairs: An American Review


# Check packages
doInstall <- TRUE  # For checking if package is installed
toInstall <- c("openxlsx", "tidyverse", "RColorBrewer", "GGally")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, require, character.only = TRUE) # call into library

# Import data from GitHub
imfgrowth = openxlsx::read.xlsx("https://github.com/datageneration/gentlemachinelearning/raw/master/data/imfgrowth.xlsx")

attach(imfgrowth)

# imfgrowth = rename(imfgrowth, US = "United.States")
imf8019 = imfgrowth[which(imfgrowth$Year<2020),]
imf8019$decade = as.factor(imf8019$decade) # Change decade into factor
attach(imf8019)

# Create group for comparison
# NSP is the countries targeted by Taiwan in its New Sound Bound Policy (2016)
tcuan = data.frame(China, Taiwan, United.States, NSP, ASEAN)

# Pairwise scatterplot matrix
# Specifying font, subject to font availabiliy on local computer
ggpairs(tcuan) + theme_bw() +  
  theme(text = element_text(size=12,  family = "Palatino"))

## Bivariate scatterplots with regression line
ggduo(
  tcuan,
  types = list(continuous = "smooth_lm")) + theme_bw()

## Scatter plot matrix
## Choose variables to be plotted
ggscatmat(imf8019, columns = 20:24,  alpha = 0.8) + 
  theme_bw() +  
  theme(text = element_text(size=12,  family = "Palatino"), ) + 
  labs(y = "Economic growth, 1980-2018",x = "Economic growth, 1980-2018") +
  scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1") 

