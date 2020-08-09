## Gentle Machine Learning
## Supervised Learning (Classification): Tree-based models
## Decision Tree: rpart
## Conditional Inference Tree: party
## Random Forest: randomForest
## Packages used: AER, rpart, rpart.plot, rattle, party, randomForest

#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")

library("rpart")
library("rpart.plot")
library("rattle")

# AER Package (AER: Applied Econometrics with R)
# install.packages("AER")
library(AER)

# CreditCard dataset
# card:   "Was the application for a credit card accepted?"
# reports: Number of major derogatory reports.
# age:     Age in years plus twelfths of a year.
# income:  Yearly income (in USD 10,000)
# owner:   Does the individual own their home?
# months:  Months living at current address.
# help("CreditCard") for dataset detail
# Reference: Greene, W.H. (2003). Econometric Analysis, 5th edition. 
#            Upper Saddle River, NJ: Prentice Hall. 
# Link: http://pages.stern.nyu.edu/~wgreene/Text/tables/tablelist5.htm.

# Load dataset
data(CreditCard)

# Subset data including predictor variables
bankcard <- subset(CreditCard, select = c(card, reports, age, income, owner, months))

# Recode card to 0, 1
bankcard$card <- ifelse(bankcard$card == "yes", 1, 0);

set.seed(1001)
# Order data by row number
newbankcard <- bankcard[sample(nrow(bankcard)),]

# Indexing for training data
t_idx <- sample(seq_len(nrow(bankcard)), size = round(0.70 * nrow(bankcard)))

# Build train and test data
traindata <- newbankcard[t_idx,]
testdata <- newbankcard[ - t_idx,]

# Decision tree model
dtree_creditcard <- rpart::rpart(formula = card ~ ., data = traindata, method = "class", control = rpart.control(cp = 0.001)) # complexity parameter

# Plot Decision tree 
rattle::fancyRpartPlot(dtree_creditcard, type = 1, main = "Decision tree", caption = "Credit card approval" )

resultdt <- predict(dtree_creditcard, newdata = testdata, type = "class")

# Confusion matrix
cm_creditcarddt <- table(testdata$card, resultdt, dnn = c("Actual", "Predicted"))
cm_creditcarddt

# Predicted Approval rate
cm_creditcarddt[4] / sum(cm_creditcarddt[, 2])

# Predicted Denial rate 
cm_creditcarddt[1] / sum(cm_creditcarddt[, 1])

# Accuracy
accuracydt <- sum(diag(cm_creditcarddt)) / sum(cm_creditcarddt)
accuracydt

# install.packages("party") 
library(party)

# Conditional Inference Tree
cit <- ctree(card ~ ., data = traindata)
plot(cit, main = "Conditional Inference Tree")

# Confusion matrix
cm_creditcardcit = table(testdata$card, round(predict(cit, newdata = testdata)), dnn = c("Actual", "Predicted"))

# Predicted Approval rate
cm_creditcardcit[4] / sum(cm_creditcardcit[, 2])

# Predicted Denial rate
cm_creditcardcit[1] / sum(cm_creditcardcit[, 1])

# Accuracy
accuracycit <- sum(diag(cm_creditcardcit)) / sum(cm_creditcardcit)
accuracycit

# Random Forest
# install.packages("randomForest")
library(randomForest)
set.seed(1001)

# randomForest model
rf_creditcard <- randomForest(card ~ ., data = traindata, importance = T, proximity = T, do.trace = 100)
plot(rf_creditcard)

round(importance(rf_creditcard), 3) # to three decimal place

resultrf <- predict(rf_creditcard, newdata = testdata)
resultrf_Approved <- ifelse(resultrf > 0.6, 1, 0)

# Confusion matrix
cm_creditcardrf <- table(testdata$card, resultrf_Approved, dnn = c("Actual", "Predicted"))
cm_creditcardrf

# Predicted Approval rate
cm_creditcardrf[4] / sum(cm_creditcardrf[, 2])

# Predicted Denial rate
cm_creditcardrf[1] / sum(cm_creditcardrf[, 1])

# Accuracy
accuracyrf <- sum(diag(cm_creditcardrf)) / sum(cm_creditcardrf)
accuracyrf

