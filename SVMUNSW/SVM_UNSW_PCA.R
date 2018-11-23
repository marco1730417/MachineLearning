library(caret)
library(e1071)
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWPCA.csv" # datos 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
h<- nids[1:82332,1:18]
v43 = nids[1:82332,40] # con el objetivo de coger los datos necesarios
v43l=nids[1:82332,40]
v43 = (v43=="anomaly")*1 #anomaly por 1
r=data.frame(v43)
l<-cbind(h,c(r))
#print(l)
model <- svm(l$v43 ~ ., data = l)
# alternatively the traditional interface:
x <- subset(l, select = -v43)
y <- v43l
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



