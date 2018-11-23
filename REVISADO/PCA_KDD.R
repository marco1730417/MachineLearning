#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental",force = TRUE)
library(ggbiplot)
library(devtools)
require(ggplot2); require(stringr)
#Carga del dataset
mydata<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv", header=T, sep=";")
# Define variables cbind nos sirve para contatenar columnas y formar una sola matriz
d1<- mydata[,1]
x1<-mydata[,5:9] #se escoje estas variables por que brindan desviaciones>0
y1<-mydata[,10:19] #se escoje estas variables por que brindan desviaciones>0 
z1<-mydata[,21:41] #se escoje estas variables por que brindan desviaciones>0 
y<-mydata[,42] #normal o anormal SIN CONVERSION 1 o 0 se escoje estas variables por que brindan desviaciones>0 
m1<-cbind(x1,c(y1))  #concatenacion
X1<-cbind(m1,c(z1))#concatenacion
h<-cbind(X1,c(d1))# concatenacion final
# Principal component analysis
cor(h)
pca1<-princomp(h,scores=TRUE,cor=TRUE)
summary(pca1)  # importante para determinar cuantas componentes principales se obtienen
desv=pca1$sdev # desviacion estandar
vari=desv^2 # varianza (desviacion al cuadrado)
componentes=names(pca1$sdev) #captura el valor de los nombres de las componentes 
c=1:37 # variables que coge numeros del 1 al 29
df = data.frame(x=c, y=vari)
p = ggplot(df, aes(c,y, fill=componentes)) + geom_bar(stat="identity")+labs(x = "Componentes",y = "Varianza")
graf = p + scale_fill_discrete()
print(graf)
print(summary(pca1))

#SEGUNDA GRAFICA ANALISIS COMPONENTES PRINCIPALES
path1="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv" # capturar dataset
correlacion1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
d<- correlacion1[,1] # seleccionamos la duracion
x11<-correlacion1[,5:9] #se escoje estas variables por que brindan desviaciones>0
y11<-correlacion1[,10:19] #se escoje estas variables por que brindan desviaciones>0 
z11<-correlacion1[,21:41] #se escoje estas variables por que brindan desviaciones>0 
m11<-cbind(x11,c(y11))  #concatenacion
L11<-cbind(m11,c(z11))#concatenacion
f11<-cbind(L11,c(d))
y1<-correlacion1[,42] #se escoje por que nos va dar el nombre
wine.pca = prcomp(f11, scale. = TRUE) 
g = ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = y1, ellipse = TRUE, circle = FALSE) 
g = g + scale_color_discrete(name = '')  +theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
print(pca1$scores)
#write.csv(pca1$scores, file = "C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDPCA1.csv")

