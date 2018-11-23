library("corrplot") # 
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWcompleto.csv" # datos 
correlacion = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
x<-correlacion[,5:42] #se escoje estas variables por que brindan desviaciones>0#
corr<-cor(x)#almaceno la correlacion
corrplot(corr,tl.cex=0.5) #tamaño de letra tl.cex