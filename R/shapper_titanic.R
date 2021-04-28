install.packages("titanic")
library("titanic")
titanic <- titanic_train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)
titanic <- na.omit(titanic)
head(titanic)

install.packages("randomForest")
set.seed(123)
model_rf <- randomForest(Survived ~ . , data = titanic)
model_rf
new_passanger <- data.frame(
  Pclass = 1,
  Sex = factor("male", levels = c("female", "male")),
  Age = 8,
  SibSp = 0,
  Parch = 0,
  Fare = 72,
  Embarked = factor("C", levels = c("","C","Q","S"))
)
predict(model_rf, new_passanger, type = "prob")
exp_rf <- explain(model_rf, data = titanic[,-1])
ive_rf <- shap(exp_rf, new_observation = new_passanger)
plot(ive_rf)
library("e1071")
model_svm <- svm(Survived~. , data = titanic, probability = TRUE)
model_svm
attr(predict(model_svm, newdata = new_passanger, probability = TRUE), "probabilities")
exp_svm <- explain(model_svm, data = titanic[,-1])
ive_svm <- shap(exp_svm, new_passanger)
plot(ive_rf, ive_svm)
plot(ive_rf, show_predcited = FALSE)
