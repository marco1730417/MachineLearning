library("corrplot") 
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv" # datos 
correlacion = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
d<- correlacion[,1] # seleccionamos la duracion
x<-correlacion[,5:9] #se escoje estas variables por que brindan desviaciones>0
y<-correlacion[,10:19] #se escoje estas variables por que brindan desviaciones>0 
z<-correlacion[,21:41] #se escoje estas variables por que brindan desviaciones>0 
matriz<-cbind(x,c(y))  #concatenacion
matriz1<-cbind(matriz,c(z))#concatenacion
matriz2<-cbind(matriz1,c(d))
corr<-cor(matriz2)#almaceno la correlacion
corrplot(corr,tl.cex=0.5) #tamaño de letra tl.cex
