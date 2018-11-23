library(caret)
library(e1071)
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDPCAVARIANZAS.csv" # datos 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
h<- nids[,1:10]
y=nids[,25]
v43 = nids[,25] 
v43 = (v43=="anomaly")*1 #anomaly por 1
r=data.frame(v43)
l<-cbind(h,c(r))
model <- svm(l$v43 ~ ., data = l)
# alternatively the traditional interface:
x <- subset(l, select = -v43)
model <- svm(x, y)  
print(model)
summary(model)
# test with train data
#pred <- predict(model, x)
# (same as:)
pred <- fitted(model)
#matriz de confusion
library(caret)
table(pred, y)
xtab <- table(pred,y)
print(confusionMatrix(xtab[2:1,2:1]))

#grafica
input.data <- data.frame(l[, c(2,3)], response = as.factor(l$v43))
svm.fit <- svm(response ~., data = input.data, kernel = "radial", cost = 10, scale = FALSE)
plot(svm.fit, input.data)



