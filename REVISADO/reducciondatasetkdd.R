path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv" 
correlacion = as.data.frame(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
rand <- sample(nrow(correlacion), nrow(correlacion) / 25)
indices<-data.frame(rand)
datos <- correlacion[rand, ]
#exportamos en formato csv los indices como tambien los datos 
write.csv(indices, file = "C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/Indices.csv")
write.csv(datos, file = "C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDReducido.csv")



