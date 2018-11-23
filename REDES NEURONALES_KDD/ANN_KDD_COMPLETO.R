library(neuralnet)
nids<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDTrainet.csv", header=T, sep=";")
nids1<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDTestet.csv", header=T, sep=";")
train <- nids
test <- nids1
ann <- neuralnet(as.numeric(nids$target)~nids$duration+nids$src_bytes+
                   nids$dst_bytes+nids$hot+
                   nids$logged_in+
                   nids$num_compromised+
                   nids$num_root+
                   nids$num_file_creations+
                   nids$is_guest_login+
                   nids$count+
                   nids$srv_count+
                   nids$serror_rate+
                   nids$srv_serror_rate+
                   nids$rerror_rate+
                   nids$srv_rerror_rate+
                   nids$same_srv_rate+
                   nids$dst_host_count+
                   nids$dst_host_srv_count+
                   nids$dst_host_same_srv_rate+
                   nids$dst_host_srv_diff_host_rate+
                   nids$dst_host_serror_rate+
                   nids$dst_host_srv_serror_rate+
                   nids$dst_host_rerror_rate+
                   nids$dst_host_srv_rerror_rate, train,hidden=1,stepmax=1e6)
plot(ann, rep = "best")
output <- compute(ann, test[ , c("duration","src_bytes",
                                 "dst_bytes","hot",
                                 "logged_in","num_compromised","num_root",
                                 "num_file_creations",
                                 "is_guest_login","count","srv_count","serror_rate",
                                 "srv_serror_rate","rerror_rate","srv_rerror_rate","same_srv_rate",
                                 "dst_host_count","dst_host_srv_count",
                                 "dst_host_same_srv_rate",
                                 "dst_host_srv_diff_host_rate","dst_host_serror_rate",
                                 "dst_host_srv_serror_rate",
                                 "dst_host_rerror_rate","dst_host_srv_rerror_rate")])
result <- data.frame(
  Real = test$target,
  Predicted = levels(nids$target)[round(output$net.result)])
result
#matriz de confusion y resultados
library(caret)
xtab <- table(result$Predicted,result$Real)
print(confusionMatrix(xtab[2:1,2:1]))

