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

training_set<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDTrainet.csv", header=T, sep=";")
test_set<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDTestet.csv", header=T, sep=";")

training_set <- nids
test_set <- nids1

x5 = nids[2:125974,5] 
x5 = as.numeric(x5) 
x6 = nids[2:125974,6] 
x6 = as.numeric(x6) 
y = nids[2:125974,42] #todos los elementos de la 11va columna menos el primero
y = (y=="anomaly")*1 #anomaly por 1

ggpairs(training_set, ggplot2::aes(colour = training_set$target, alpha = 0.4))

training_set[,5:6] = scale(training_set[,5:6])
test_set[,5:6] = scale(test_set[,5:6])

classifier1 = svm(formula = training_set$target~., data = training_set, type = 'C-classification', kernel = 'linear')
classifier2 = svm(formula =training_set$target~ training_set$src_bytes+training_set$dst_bytes, data = training_set, type = 'C-classification', kernel = 'radial')

test_pred1 = predict(classifier1, type = 'response', newdata = test_set[-5])
test_pred2 = predict(classifier2, type = 'response', newdata = test_set[-5])

# Making Confusion Matrix
cm1 = table(test_set[,5], test_pred1)
cm2 = table(test_set[,42], test_pred2)
cm1 # Confusion Matrix for all parameters
cm2
