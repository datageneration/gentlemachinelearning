## Gentle Machine Learning
## K-nearest Neighbor (KNN)
## Adapted from James, G., Witten, D., Hastie, T. and Tibshirani, R., 2013. 
## An introduction to statistical learning (Vol. 112, p. 18). New York: springer

install.packages("ISLR")
library(ISLR)
sm = Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket, pch=16, cex=.2, col="blue")
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume, pch=16, cex=.5, col="blue")
648/(648+602)
# Create training data set
# Note: no reshuffling.  Why? 
train=(Year<2005)

# Create testing data sets
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

install.packages("class")
library(class)

train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)

# Running K-nearest Neighbor 
# k-nearest neighbor classification for test set from training set. 
# For each row of the test set, the k nearest (in Euclidean distance) 
# training set vectors are found, and the classification is decided by 
# majority vote, with ties broken at random. 
# If there are ties for the kth nearest vector, all candidates are 
# included in the vote.

knn.pred=knn(train.X,test.X,train.Direction,k=1)

# Confusion matrix
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)

# What is the chance the stock is going up?
mean(knn.pred==Direction.2005)

