#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental",force = TRUE)
library(ggbiplot)
library(devtools)
require(ggplot2); require(stringr)
#Carga del dataset
mydata<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv", header=T, sep=";")
# Define variables cbind nos sirve para contatenar columnas y formar una sola matriz
d1<- mydata[,1]
d2<-mydata[,5:6] #se escoje estas variables por que brindan desviaciones>0
d3<-mydata[,10:10] #se escoje estas variables por que brindan desviaciones>0 
d4<-mydata[,12:13] #se escoje estas variables por que brindan desviaciones>0 
d5<-mydata[,16:17] #se escoje estas variables por que brindan desviaciones>0 
d6<-mydata[,22:29] #se escoje estas variables por que brindan desviaciones>0 
d7<-mydata[,32:34] #se escoje estas variables por que brindan desviaciones>0 
d8<-mydata[,37:41] #se escoje estas variables por que brindan desviaciones>0 
c1<-cbind(d2,c(d3))  #concatenacion
c2<-cbind(c1,c(d4))#concatenacion
c3<-cbind(c2,c(d5))#concatenacion
c4<-cbind(c3,c(d6))#concatenacion
c5<-cbind(c4,c(d7))#concatenacion
c6<-cbind(c5,c(d8))#concatenacion
h<-cbind(c6,c(d1))#concatenacion
y<-mydata[,42] #normal o anormal SIN CONVERSION 1 o 0 se escoje estas variables por que brindan desviaciones>0 
# Principal component analysis
cor(h)
pca1<-princomp(h,scores=TRUE,cor=TRUE)
summary(pca1)  # importante para determinar cuantas componentes principales se obtienen
desv=pca1$sdev # desviacion estandar
vari=desv^2 # varianza (desviacion al cuadrado)
componentes=names(pca1$sdev) #captura el valor de los nombres de las componentes 
c=1:24 # variables que coge numeros del 1 al 29
df = data.frame(x=c, y=vari)
p = ggplot(df, aes(c,y, fill=componentes)) + geom_bar(stat="identity")+labs(x = "Componentes",y = "Varianza"+geom_boxplot())
graf = p + scale_fill_discrete()
print(graf)
print(summary(pca1))

#SEGUNDA GRAFICA ANALISIS COMPONENTES PRINCIPALES
path1="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv" # capturar dataset
correlacion1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)d11<- correlacion1[,1]
d11<- correlacion1[,1]
d21<-correlacion1[,5:6] #se escoje estas variables por que brindan desviaciones>0
d31<-correlacion1[,10:10] #se escoje estas variables por que brindan desviaciones>0 
d41<-correlacion1[,12:13] #se escoje estas variables por que brindan desviaciones>0 
d51<-correlacion1[,16:17] #se escoje estas variables por que brindan desviaciones>0 
d61<-correlacion1[,22:29] #se escoje estas variables por que brindan desviaciones>0 
d71<-correlacion1[,32:34] #se escoje estas variables por que brindan desviaciones>0 
d81<-correlacion1[,37:41] #se escoje estas variables por que brindan desviaciones>0 
c11<-cbind(d21,c(d31))  #concatenacion
c21<-cbind(c11,c(d41))#concatenacion
c31<-cbind(c21,c(d51))#concatenacion
c41<-cbind(c31,c(d61))#concatenacion
c51<-cbind(c41,c(d71))#concatenacion
c61<-cbind(c51,c(d81))#concatenacion
f11<-cbind(c61,c(d11))#concatenacion
y1<-correlacion1[,42] #se escoje por que nos va dar el nombre
wine.pca = prcomp(f11, scale. = TRUE) 
g = ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = y1, ellipse = TRUE, circle = FALSE) 
g = g + scale_color_discrete(name = '')  +theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
print(pca1$scores)
component = data.frame(pca1$scores)
#write.csv(component, file="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/Scripts R/SCRIPTS VARIANZAS/KDDPCAVARIANZAS.csv", row.names = F)
component
