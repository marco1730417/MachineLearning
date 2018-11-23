col1<- c(38,40)
col2<- c(4,4)
col3<- c(42,44)
  B<- rbind(col1,col2,col3) #pegamos los vectores en filas
 colnames(B)<-c("NSL-KDD","UNSW-NB15") # Ponemos nombres a las variables
op <- par(mar = c(5, 5, 3, 3) + 0.1) # Un ejemplo para cambiar las márgenes de la ventana.
boxplot(B,cex.axis=1,las=2,col = c("red","blue"),
        cex.lab=1, ylab="Variables",xlab="", names=c("NSL-KDD","UNSW-NB15")) # Obtención del diagrama.
