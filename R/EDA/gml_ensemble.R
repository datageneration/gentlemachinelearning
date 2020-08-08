## Gentle Machine Learning
## Scatter plot matrix
## Adapted from example in Unwin, Antony.2015. Graphical data analysis with R. Vol. 27. CRC Press.

doInstall <- TRUE  # For checking if package is installed
toInstall <- c("pgmm", "tidyverse", "pdp", "GGally", "grid", "gridExtra")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE) # call into library

# Load data
# Data on the chemical composition of coffee samples collected from around the 
# world, comprising 43 samples from 29 countries. Each sample is either of the 
# Arabica or Robusta variety. Twelve of the thirteen chemical constituents 
# reported in the study are given. 
# The omitted variable is total chlorogenic acid; it is generally the sum of 
# the chlorogenic, neochlorogenic and isochlorogenic acid values.

data(coffee, package="pgmm")
coffee <- within(coffee, Type <- ifelse(Variety==1,
                                        "Arabica", "Robusta"))
names(coffee) <- abbreviate(names(coffee), 8)
a <- ggplot(coffee, aes(x=Type)) + geom_bar(aes(fill=Type)) +
  scale_fill_manual(values = c("grey70", "red")) +
  guides(fill=FALSE) + ylab("") +
  theme_bw() +
  theme(text = element_text(family="Palatino")) 
 
b <- ggplot(coffee, aes(x=Fat, y=Caffine, colour=Type)) +
  geom_point(size=2) +
  scale_colour_manual(values = c("grey70", "red")) +
  theme_bw() +
  theme(text = element_text(family="Palatino"))
 
c <- ggparcoord(coffee[order(coffee$Type),], columns=3:14,
                groupColumn="Type", scale="uniminmax",
                mapping = aes(size = 1), splineFactor = TRUE ) +
  xlab("") +  ylab("") +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("grey","red")) +
  theme_bw() +
  theme(text = element_text(family="Palatino")) 

# Combine into one page using grid
grid.arrange(arrangeGrob(a, b, ncol=2, widths=c(1,2)),
             c, nrow=2) 

