## Gentle Machine Learning
## Principal Component Analysis


# Dataset: USArrests is the sample dataset used in 
# McNeil, D. R. (1977) Interactive Data Analysis. New York: Wiley.
# Murder	numeric	Murder arrests (per 100,000)
# Assault	numeric	Assault arrests (per 100,000)
# UrbanPop	numeric	Percent urban population
# Rape	numeric	Rape arrests (per 100,000)
# For each of the fifty states in the United States, the dataset contains the number 
# of arrests per 100,000 residents for each of three crimes: Assault, Murder, and Rape. 
# UrbanPop is the percent of the population in each state living in urban areas.
library(datasets)
library(ISLR)
arrest = USArrests
states=row.names(USArrests)
names(USArrests)

# Get means and variances of variables
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# PCA with scaling
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out) # Five
pr.out$center # the centering and scaling used (means)
pr.out$scale # the matrix of variable loadings (eigenvectors)
pr.out$rotation
dim(pr.out$x)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

## Use factoextra package
library(factoextra)
fviz(pr.out, "ind", geom = "auto", mean.point = TRUE, font.family = "Georgia")
fviz_pca_biplot(pr.out, font.family = "Georgia", col.var="firebrick1")

