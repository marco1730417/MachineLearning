library("corrplot") # 
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWcompleto.csv" # datos 
correlacion = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
x<-correlacion[,5:42] 
corr<-cor(x)
corrplot(corr,tl.cex=0.5) 