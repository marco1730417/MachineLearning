library(caret)
library(e1071)
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWTrain.csv" # datos 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
h<- nids[,5:42]
v43 = nids[,43] 
v43 = (v43=="anomaly")*1 #anomaly por 1
r=data.frame(v43)
l<-cbind(h,c(r))
model <- svm(l$v43 ~ ., data = l)
# alternatively the traditional interface:
x <- subset(l, select = -v43)
y <- nids$V43
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



