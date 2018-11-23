path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv" # datos 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
V1<- nids[,1]
d2<-nids[,5:6] 
V10<-nids[,10:10]  
d4<-nids[,12:13]  
d5<-nids[,16:17]  
d6<-nids[,22:29]  
d7<-nids[,32:34]  
d8<-nids[,37:41]  
d9<-nids[,42:42]
c1<-cbind(d2,c(V10))
c2<-cbind(c1,c(d4))
c3<-cbind(c2,c(d5))
c4<-cbind(c3,c(d6))
c5<-cbind(c4,c(d7))
c6<-cbind(c5,c(d8))
h<-cbind(c6,c(V1))
v42 = nids[,42] 
v42 = (v42=="anomaly")*1 #anomaly por 1
r<-data.frame(v42)
l<-cbind(h,c(r))
model <- svm(l$v42 ~ ., data = l)

# alternatively the traditional interface:
x <- subset(l, select = -v42)
y <- nids$V42
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



