## Interpretable Machine Learning model
## Adapted from Molnar, Christoph. 2020. Interpretable machine learning. Lulu.com and
## Biecek, Przemyslaw, and Tomasz Burzykowski., 2021. Explanatory model analysis: explore, explain, and examine predictive models. CRC Press

# install.packages(c("titanic", "DALEX", "shapper", "e1071","tictoc")
# Load the Kaggle dataset named "titanic"
library(titanic) # Not the same as Titanic data in dataset package
library(DALEX)
library(shapper)
library(e1071)
library(tictoc)
tt=data.frame(titanic)
data(titanic)

## Titanic dataset variables
## Pclass Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd) 
## survical Survival (0 = No; 1 = Yes)
## name Name
## sex Sex
## age Age
## sibsp Number of Siblings/Spouses Aboard
## parch Number of Parents/Children Aboard
## ticket Ticket Number
## fare Passenger Fare (British pound)
## cabin Cabin
## embarked Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton) 
## boat Lifeboat
## body Body Identification Number
## home.dest Home/Destination## 

# Training data

titanic <- titanic_train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
titanic$Survived <- factor(titanic$Survived) # Converting to factor
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)
titanic <- na.omit(titanic) # Remove missing value cases
head(titanic)

# install.packages("randomForest")
library(randomForest)
set.seed(123)
tic() # Timing the procedure of random forest modeling
model_rf <- randomForest(Survived ~ . , data = titanic)
toc()

model_rf

# Creating a profile
new_passanger <- data.frame(
  Pclass = 1,
  Sex = factor("male", levels = c("female", "male")),
  Age = 8,
  SibSp = 0,
  Parch = 0,
  Fare = 72,
  Embarked = factor("C", levels = c("","C","Q","S"))
)

# Predicting survival using rf model and profile
predict(model_rf, new_passanger, type = "prob")

# Creating an explainer
exp_rf <- explain(model_rf, data = titanic[,-1])

# Shapley values and plot
ive_rf <- shap(exp_rf, new_observation = new_passanger)
plot(ive_rf)

# Support Vector Machine 
model_svm <- svm(Survived~. , data = titanic, probability = TRUE)

# Get probabilities from SVM model
  
attr(predict(model_svm, newdata = new_passanger, probability = TRUE), "probabilities")

# Creating an explainer for SVM
exp_svm <- explain(model_svm, data = titanic[,-1])

# Shapley values and plot
ive_svm <- shap(exp_svm, new_passanger)
plot(ive_svm)
plot(ive_rf, ive_svm)

plot(ive_rf, show_predicted = FALSE)





