library(caTools)
library(ggplot2)
library(GGally)
library(e1071)
dataset = iris
str(dataset)
summary(dataset)
split = sample.split(dataset$Species, SplitRatio = .8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
nrow(training_set)
nrow(test_set)
ggpairs(training_set, ggplot2::aes(colour = Species, alpha = 0.4))
training_set[,1:4] = scale(training_set[,1:4])
test_set[,1:4] = scale(test_set[,1:4])

classifier1 = svm(formula = Species~., data = training_set, type = 'C-classification', kernel = 'radial')
# funcion kernel determina la funcion y va aprendiendo la curva

classifier2 = svm(formula = Species~ Petal.Width + Petal.Length, data = training_set, type = 'C-classification', kernel = 'radial')
#test set -5 son 30 valores del test al azaf
test_pred1 = predict(classifier1, type = 'response', newdata = test_set[-5])
test_pred2 = predict(classifier2, type = 'response', newdata = test_set[-5])
# Making Confusion Matrix
cm1 = table(test_set[,5], test_pred1)
cm2 = table(test_set[,5], test_pred2)
cm1 # Confusion Matrix for all parameters
cm2
