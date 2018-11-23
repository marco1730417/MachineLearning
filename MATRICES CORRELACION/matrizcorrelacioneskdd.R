library("corrplot") 
path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv" # datos 
mydata = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
V1<- mydata[,1]
d2<-mydata[,5:6] 
V10<-mydata[,10:10]  
d4<-mydata[,12:13]  
d5<-mydata[,16:17]  
d6<-mydata[,22:29]  
d7<-mydata[,32:34]  
d8<-mydata[,37:41]  
c1<-cbind(d2,c(V10))
c2<-cbind(c1,c(d4))
c3<-cbind(c2,c(d5))
c4<-cbind(c3,c(d6))
c5<-cbind(c4,c(d7))
c6<-cbind(c5,c(d8))
h<-cbind(c6,c(V1))#concatenacion
corr<-cor(h)#almaceno la correlacion
corrplot(corr,tl.cex=0.5)
