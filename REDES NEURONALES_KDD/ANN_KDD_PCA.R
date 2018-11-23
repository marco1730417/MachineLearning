library(neuralnet)
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDPCAVARIANZAS.csv" # datos 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
fold.test <- sample(nrow(nids), nrow(nids) / 2)
train <- nids[fold.test, ]
test <- nids[-fold.test, ]
ann <- neuralnet(as.numeric(train$V25)~
                   train$V1+
                   train$V2+
                   train$V3+
                   train$V4+
                   train$V5+
                   train$V6+
                   train$V7+
                   train$V8+
                   train$V9+
                   train$V10
                   ,train,hidden=1,stepmax=1e6)
plot(ann, rep = "best")
output <- compute(ann, test[ , c("V1","V2",
                                 "V3","V4",
                                 "V5","V6","V7",
                                 "V8",
                                 "V9","V10")])
result <- data.frame(
  Real = test$V25,
  Predicted = levels(nids$V25)[round(output$net.result)])
result
#matriz de confusion y resultados
#library(caret)
xtab <- table(result$Predicted,result$Real)
print(confusionMatrix(xtab[2:1,2:1]))

