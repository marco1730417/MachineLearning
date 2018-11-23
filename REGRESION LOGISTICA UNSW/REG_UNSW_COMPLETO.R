path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWCompleto.csv" # 
nids = as.matrix(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
y = nids[1:82332,43] 
y = (y=="anomaly")*1 #anomaly por 1
x1 = nids[1:82332,1] 
x1 = as.numeric(x1) 
x5 = nids[1:82332,5] 
x5 = as.numeric(x5) 
x6 = nids[1:82332,6] 
x6 = as.numeric(x6) 
x7 = nids[1:82332,7] 
x7 = as.numeric(x7) 
x8 = nids[1:82332,8] 
x8 = as.numeric(x8) 
x9 = nids[1:82332,9] 
x9 = as.numeric(x9) 
x10 = nids[1:82332,10] 
x10 = as.numeric(x10) 
x11 = nids[1:82332,11] 
x11 = as.numeric(x11) 
x12 = nids[1:82332,12] 
x12 = as.numeric(x12) 
x13 = nids[1:82332,13] 
x13= as.numeric(x13) 
x14 = nids[1:82332,14] 
x14 = as.numeric(x14) 
x15 = nids[1:82332,15] 
x15 = as.numeric(x15) 
x16 = nids[1:82332,16] 
x16 = as.numeric(x16) 
x17 = nids[1:82332,17] 
x17 = as.numeric(x17) 
x18 = nids[1:82332,18] 
x18 = as.numeric(x18) 
x19 = nids[1:82332,19] 
x19 = as.numeric(x19) 
x21 = nids[1:82332,21] 
x21 = as.numeric(x21) 
x22 = nids[1:82332,22] 
x22 = as.numeric(x22) 
x23 = nids[1:82332,23] 
x23 = as.numeric(x23) 
x24 = nids[1:82332,24] 
x24 = as.numeric(x24) 
x25 = nids[1:82332,25] 
x25 = as.numeric(x25) 
x26 = nids[1:82332,26] 
x26 = as.numeric(x26) 
x27 = nids[1:82332,27] 
x27 = as.numeric(x27) 
x28 = nids[1:82332,28] 
x28 = as.numeric(x28) 
x29 = nids[1:82332,29] 
x29 = as.numeric(x29) 
x30 = nids[1:82332,30] 
x30 = as.numeric(x30) 
x31 = nids[1:82332,31] 
x31= as.numeric(x31) 
x32 = nids[1:82332,32] 
x32 = as.numeric(x32) 
x33 = nids[1:82332,33] 
x33 = as.numeric(x33) 
x34 = nids[1:82332,34] 
x34 = as.numeric(x34) 
x35 = nids[1:82332,35] 
x35 = as.numeric(x35) 
x36 = nids[1:82332,36] 
x36 = as.numeric(x36) 
x37 = nids[1:82332,37] 
x37 = as.numeric(x37) 
x38 = nids[1:82332,38] 
x38 = as.numeric(x38) 
x39 = nids[1:82332,39] 
x39 = as.numeric(x39) 
x40 = nids[1:82332,40] 
x40 = as.numeric(x40) 
x41 = nids[1:82332,41] 
x41 = as.numeric(x41) 
x42 = nids[1:82332,42] 
x42 = as.numeric(x42) 

#TRAINING
nids_sol = glm(y ~ x1+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x21+x22+x23+x24+x25+x26+x27+x29+x30+x31+x32+x33+x34+x35+x36+x37+x38+x39+x40+x41+x42,family=binomial()) #calculo de la regresion lineal (funcion directa)
print (summary(nids_sol)) #resumen de los resultados de la regresion lineal 
y_estim =exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x1+
    nids_sol$coefficients[3]*x5+
    nids_sol$coefficients[4]*x6+
    nids_sol$coefficients[5]*x7+
    nids_sol$coefficients[6]*x8+
    nids_sol$coefficients[7]*x9+
    nids_sol$coefficients[8]*x10+
    nids_sol$coefficients[9]*x11+
    nids_sol$coefficients[10]*x12+
    nids_sol$coefficients[11]*x13+
    nids_sol$coefficients[12]*x14+
    nids_sol$coefficients[13]*x15+
    nids_sol$coefficients[14]*x16+
    nids_sol$coefficients[15]*x17+
    nids_sol$coefficients[16]*x18+
    nids_sol$coefficients[17]*x19+
    nids_sol$coefficients[18]*x21+
    nids_sol$coefficients[19]*x22+
    nids_sol$coefficients[20]*x23+
    nids_sol$coefficients[21]*x24+
    nids_sol$coefficients[22]*x25+
    nids_sol$coefficients[23]*x26+
    nids_sol$coefficients[24]*x27+
    nids_sol$coefficients[25]*x29+
    nids_sol$coefficients[26]*x30+
    nids_sol$coefficients[27]*x31+
    nids_sol$coefficients[28]*x32+
    nids_sol$coefficients[29]*x33+
    nids_sol$coefficients[30]*x34+
    nids_sol$coefficients[31]*x35+
    nids_sol$coefficients[32]*x36+
    nids_sol$coefficients[33]*x37+
    nids_sol$coefficients[34]*x38+
    nids_sol$coefficients[35]*x39+
    nids_sol$coefficients[36]*x40+
    nids_sol$coefficients[37]*x41+
    nids_sol$coefficients[38]*x42
  
) / (1+exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x1+
    nids_sol$coefficients[3]*x5+
    nids_sol$coefficients[4]*x6+
    nids_sol$coefficients[5]*x7+
    nids_sol$coefficients[6]*x8+
    nids_sol$coefficients[7]*x9+
    nids_sol$coefficients[8]*x10+
    nids_sol$coefficients[9]*x11+
    nids_sol$coefficients[10]*x12+
    nids_sol$coefficients[11]*x13+
    nids_sol$coefficients[12]*x14+
    nids_sol$coefficients[13]*x15+
    nids_sol$coefficients[14]*x16+
    nids_sol$coefficients[15]*x17+
    nids_sol$coefficients[16]*x18+
    nids_sol$coefficients[17]*x19+
    nids_sol$coefficients[18]*x21+
    nids_sol$coefficients[19]*x22+
    nids_sol$coefficients[20]*x23+
    nids_sol$coefficients[21]*x24+
    nids_sol$coefficients[22]*x25+
    nids_sol$coefficients[23]*x26+
    nids_sol$coefficients[24]*x27+
    nids_sol$coefficients[25]*x29+
    nids_sol$coefficients[26]*x30+
    nids_sol$coefficients[27]*x31+
    nids_sol$coefficients[28]*x32+
    nids_sol$coefficients[29]*x33+
    nids_sol$coefficients[30]*x34+
    nids_sol$coefficients[31]*x35+
    nids_sol$coefficients[32]*x36+
    nids_sol$coefficients[33]*x37+
    nids_sol$coefficients[34]*x38+
    nids_sol$coefficients[35]*x39+
    nids_sol$coefficients[36]*x40+
    nids_sol$coefficients[37]*x41+
    nids_sol$coefficients[38]*x42
  
) ) 

#ecuacion de regresion logistica estimada
maxi = max(y_estim) #valor maximo de la ecuacion estimada
mini = min(y_estim) #valor maximo de la ecuacion estimada
treshold = (mini+maxi)/2 #umbral de decision
treshold_1 = rep(c(treshold),each=length(y_estim)) #umbral de decision en forma de vector
survived_pred = (y_estim > treshold)*1 #vector de sobrevivientes predecidos
difer = (y == survived_pred)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errores = length(difer) - sum(difer) #numero de errores cometidos
porc_err = errores/length(difer)*100 #porcentaje de error
print (porc_err)   # 

#TEST
yt = nids[82333:257673,43] #todos los elementos de la 11va columna menos el primero
yt= (yt=="anomaly")*1 #anomaly por 1
x1t = nids[82333:257673,1] 
x1t = as.numeric(x1t) 
x5t = nids[82333:257673,5] 
x5t = as.numeric(x5t) 
x6t = nids[82333:257673,6] 
x6t = as.numeric(x6t) 
x7t = nids[82333:257673,7] 
x7t = as.numeric(x7t) 
x8t = nids[82333:257673,8] 
x8t = as.numeric(x8t) 
x9t = nids[82333:257673,9] 
x9t = as.numeric(x9t) 
x10t = nids[82333:257673,10] 
x10t = as.numeric(x10t) 
x11t = nids[82333:257673,11] 
x11t = as.numeric(x11t) 
x12t = nids[82333:257673,12] 
x12t = as.numeric(x12t) 
x13t = nids[82333:257673,13] 
x13t= as.numeric(x13t) 
x14t = nids[82333:257673,14] 
x14t = as.numeric(x14t) 
x15t = nids[82333:257673,15] 
x15t = as.numeric(x15t) 
x16t = nids[82333:257673,16] 
x16t = as.numeric(x16t) 
x17t = nids[82333:257673,17] 
x17t = as.numeric(x17t) 
x18t = nids[82333:257673,18] 
x18t = as.numeric(x18t) 
x19t = nids[82333:257673,19] 
x19t = as.numeric(x19t) 
x21t = nids[82333:257673,21] 
x21t = as.numeric(x21t) 
x22t = nids[82333:257673,22] 
x22t = as.numeric(x22t) 
x23t = nids[82333:257673,23] 
x23t = as.numeric(x23t) 
x24t = nids[82333:257673,24] 
x24t = as.numeric(x24t) 
x25t = nids[82333:257673,25] 
x25t = as.numeric(x25t) 
x26t = nids[82333:257673,26] 
x26t = as.numeric(x26t) 
x27t = nids[82333:257673,27] 
x27t = as.numeric(x27t) 
x28t = nids[82333:257673,28] 
x28t = as.numeric(x28t) 
x29t = nids[82333:257673,29] 
x29t = as.numeric(x29t) 
x30t = nids[82333:257673,30] 
x30t = as.numeric(x30t) 
x31t = nids[82333:257673,31] 
x31t= as.numeric(x31t) 
x32t = nids[82333:257673,32] 
x32t = as.numeric(x32t) 
x33t = nids[82333:257673,33] 
x33t = as.numeric(x33t) 
x34t = nids[82333:257673,34] 
x34t = as.numeric(x34t) 
x35t = nids[82333:257673,35] 
x35t = as.numeric(x35t) 
x36t = nids[82333:257673,36] 
x36t = as.numeric(x36t) 
x37t = nids[82333:257673,37] 
x37t = as.numeric(x37t) 
x38t = nids[82333:257673,38] 
x38t = as.numeric(x38t) 
x39t = nids[82333:257673,39] 
x39t = as.numeric(x39t) 
x40t = nids[82333:257673,40] 
x40t = as.numeric(x40t) 
x41t = nids[82333:257673,41] 
x41t = as.numeric(x41t) 
x42t = nids[82333:257673,42] 
x42t = as.numeric(x42t) 
y_estimt =exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x1t+
    nids_sol$coefficients[3]*x5t+
    nids_sol$coefficients[4]*x6t+
    nids_sol$coefficients[5]*x7t+
    nids_sol$coefficients[6]*x8t+
    nids_sol$coefficients[7]*x9t+
    nids_sol$coefficients[8]*x10t+
    nids_sol$coefficients[9]*x11t+
    nids_sol$coefficients[10]*x12t+
    nids_sol$coefficients[11]*x13t+
    nids_sol$coefficients[12]*x14t+
    nids_sol$coefficients[13]*x15t+
    nids_sol$coefficients[14]*x16t+
    nids_sol$coefficients[15]*x17t+
    nids_sol$coefficients[16]*x18t+
    nids_sol$coefficients[17]*x19t+
    nids_sol$coefficients[18]*x21t+
    nids_sol$coefficients[19]*x22t+
    nids_sol$coefficients[20]*x23t+
    nids_sol$coefficients[21]*x24t+
    nids_sol$coefficients[22]*x25t+
    nids_sol$coefficients[23]*x26t+
    nids_sol$coefficients[24]*x27t+
    nids_sol$coefficients[25]*x29t+
    nids_sol$coefficients[26]*x30t+
    nids_sol$coefficients[27]*x31t+
    nids_sol$coefficients[28]*x32t+
    nids_sol$coefficients[29]*x33t+
    nids_sol$coefficients[30]*x34t+
    nids_sol$coefficients[31]*x35t+
    nids_sol$coefficients[32]*x36t+
    nids_sol$coefficients[33]*x37t+
    nids_sol$coefficients[34]*x38t+
    nids_sol$coefficients[35]*x39t+
    nids_sol$coefficients[36]*x40t+
    nids_sol$coefficients[37]*x41t+
    nids_sol$coefficients[38]*x42t
  ) /
  (1+exp(
    nids_sol$coefficients[1]+
      nids_sol$coefficients[2]*x1t+
      nids_sol$coefficients[3]*x5t+
      nids_sol$coefficients[4]*x6t+
      nids_sol$coefficients[5]*x7t+
      nids_sol$coefficients[6]*x8t+
      nids_sol$coefficients[7]*x9t+
      nids_sol$coefficients[8]*x10t+
      nids_sol$coefficients[9]*x11t+
      nids_sol$coefficients[10]*x12t+
      nids_sol$coefficients[11]*x13t+
      nids_sol$coefficients[12]*x14t+
      nids_sol$coefficients[13]*x15t+
      nids_sol$coefficients[14]*x16t+
      nids_sol$coefficients[15]*x17t+
      nids_sol$coefficients[16]*x18t+
      nids_sol$coefficients[17]*x19t+
      nids_sol$coefficients[18]*x21t+
      nids_sol$coefficients[19]*x22t+
      nids_sol$coefficients[20]*x23t+
      nids_sol$coefficients[21]*x24t+
      nids_sol$coefficients[22]*x25t+
      nids_sol$coefficients[23]*x26t+
      nids_sol$coefficients[24]*x27t+
      nids_sol$coefficients[25]*x29t+
      nids_sol$coefficients[26]*x30t+
      nids_sol$coefficients[27]*x31t+
      nids_sol$coefficients[28]*x32t+
      nids_sol$coefficients[29]*x33t+
      nids_sol$coefficients[30]*x34t+
      nids_sol$coefficients[31]*x35t+
      nids_sol$coefficients[32]*x36t+
      nids_sol$coefficients[33]*x37t+
      nids_sol$coefficients[34]*x38t+
      nids_sol$coefficients[35]*x39t+
      nids_sol$coefficients[36]*x40t+
      nids_sol$coefficients[37]*x41t+
      nids_sol$coefficients[38]*x42t
  ) ) 
maxit = max(y_estimt) #valor maximo de la ecuacion estimada
minit = min(y_estimt) #valor maximo de la ecuacion estimada
tresholdt = (minit+maxit)/2 #umbral de decision
treshold_1t = rep(c(tresholdt),each=length(y_estimt)) #umbral de decision en forma de vector
nids_predt = (y_estimt > tresholdt)*1 
difert = (yt == nids_predt)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errorest = length(difert) - sum(difert) #numero de errores cometidos
porc_errt = errorest/length(difert)*100 #porcentaje de error
print (porc_errt)   # 
#matriz de confusion y resultados
xtab <- table(nids_predt,yt)
print(confusionMatrix(xtab[2:1,2:1]))

#grafica
l = nids[1:82332,43] 
lr_data <- data.frame(predictor=nids_sol$linear.predictors, prob=nids_sol$fitted.values, clases=l)
ggplot(lr_data, aes(x=predictor, y=prob, color=clases)) + geom_point()
