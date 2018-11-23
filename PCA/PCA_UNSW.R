#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental",force = TRUE)
library(ggbiplot)
library(devtools)
require(ggplot2); require(stringr)
#Carga del dataset
mydata<- read.csv("C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWcompleto.csv", header=T, sep=";")
# Define variables cbind nos sirve para contatenar columnas y formar una sola matriz
d1<- mydata[,1]
y1<-mydata[,5:42] #se escoje estas variables por que brindan desviaciones>0
x1<-cbind(y1,c(d1))

# Principal component analysis
cor(x1)
pca1<-princomp(x1,scores=TRUE,cor=TRUE)
desv=pca1$sdev # desviacion estandar
vari=desv^2 # varianza (desviacion al cuadrado)
Variables=names(pca1$sdev) #captura el valor de los nombres de las componentes 
c=1:39 # variables que coge numeros del 1 al 39
df = data.frame(x=c, y=vari)
p = ggplot(df, aes(c,y, fill=Variables)) + geom_bar(stat="identity")+labs(x = "Componentes",y = "Varianza")
graf = p + scale_fill_discrete()
print(graf)
print(summary(pca1))

#SEGUNDA GRAFICA ANALISIS COMPONENTES PRINCIPALES
path1="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWcompleto.csv" # capturar dataset
correlacion1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
d11<- correlacion1[,1]
y11<-correlacion1[,5:42] #se escoje estas variables por que brindan desviaciones>0
x11<-cbind(y11,c(d11))
y1<-correlacion1[,43] #la que nos da el nombre 
wine.pca = prcomp(x11, scale. = TRUE) 
g = ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = y1, ellipse = TRUE, circle = FALSE) 
g = g + scale_color_discrete(name = '')  +theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
print(pca1$scores)
write.csv(pca1$scores, file = "C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWPCA.csv")

