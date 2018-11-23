path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDCompleto.csv" # datos nids
nids = as.matrix(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
y = nids[1:125973,42] #todos los elementos de la 11va columna menos el primero
y = (y=="anomaly")*1 #anomaly por 1
x1 = nids[1:125973,1] 
x1 = as.numeric(x1) 
x5 = nids[1:125973,5] 
x5 = as.numeric(x5) 
x6 = nids[1:125973,6] 
x6 = as.numeric(x6) 
x7 = nids[1:125973,7] 
x7 = as.numeric(x7) 
x8 = nids[1:125973,8] 
x8 = as.numeric(x8) 
x9 = nids[1:125973,9] 
x9 = as.numeric(x9) 
x10 = nids[1:125973,10] 
x10 = as.numeric(x10) 
x11 = nids[1:125973,11] 
x11 = as.numeric(x11) 
x12 = nids[1:125973,12] 
x12 = as.numeric(x12) 
x13 = nids[1:125973,13] 
x13= as.numeric(x13) 
x14 = nids[1:125973,14] 
x14 = as.numeric(x14) 
x15 = nids[1:125973,15] 
x15 = as.numeric(x15) 
x16 = nids[1:125973,16] 
x16 = as.numeric(x16) 
x17 = nids[1:125973,17] 
x17 = as.numeric(x17) 
x18 = nids[1:125973,18] 
x18 = as.numeric(x18) 
x19 = nids[1:125973,19] 
x19 = as.numeric(x19) 
x21 = nids[1:125973,21] 
x21 = as.numeric(x21) 
x22 = nids[1:125973,22] 
x22 = as.numeric(x22) 
x23 = nids[1:125973,23] 
x23 = as.numeric(x23) 
x24 = nids[1:125973,24] 
x24 = as.numeric(x24) 
x25 = nids[1:125973,25] 
x25 = as.numeric(x25) 
x26 = nids[1:125973,26] 
x26 = as.numeric(x26) 
x27 = nids[1:125973,27] 
x27 = as.numeric(x27) 
x28 = nids[1:125973,28] 
x28 = as.numeric(x28) 
x29 = nids[1:125973,29] 
x29 = as.numeric(x29) 
x30 = nids[1:125973,30] 
x30 = as.numeric(x30) 
x31 = nids[1:125973,31] 
x31= as.numeric(x31) 
x32 = nids[1:125973,32] 
x32 = as.numeric(x32) 
x33 = nids[1:125973,33] 
x33 = as.numeric(x33) 
x34 = nids[1:125973,34] 
x34 = as.numeric(x34) 
x35 = nids[1:125973,35] 
x35 = as.numeric(x35) 
x36 = nids[1:125973,36] 
x36 = as.numeric(x36) 
x37 = nids[1:125973,37] 
x37 = as.numeric(x37) 
x38 = nids[1:125973,38] 
x38 = as.numeric(x38) 
x39 = nids[1:125973,39] 
x39 = as.numeric(x39) 
x40 = nids[1:125973,40] 
x40 = as.numeric(x40) 
x41 = nids[1:125973,41] 
x41 = as.numeric(x41) 

#TRAINING 
nids_sol = glm(y ~ x1+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34+x35+x36+x37+x38+x39+x40+x41,family=binomial()) #calculo de la regresion lineal (funcion directa)
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
    nids_sol$coefficients[25]*x28+
    nids_sol$coefficients[26]*x29+
    nids_sol$coefficients[27]*x30+
    nids_sol$coefficients[28]*x31+
    nids_sol$coefficients[29]*x32+
    nids_sol$coefficients[30]*x33+
    nids_sol$coefficients[31]*x34+
    nids_sol$coefficients[32]*x35+
    nids_sol$coefficients[33]*x36+
    nids_sol$coefficients[34]*x37+
    nids_sol$coefficients[35]*x38+
    nids_sol$coefficients[36]*x39+
    nids_sol$coefficients[37]*x40+
    nids_sol$coefficients[38]*x41)/ 
   (1+exp(nids_sol$coefficients[1]+
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
    nids_sol$coefficients[25]*x28+
    nids_sol$coefficients[26]*x29+
    nids_sol$coefficients[27]*x30+
    nids_sol$coefficients[28]*x31+
    nids_sol$coefficients[29]*x32+
    nids_sol$coefficients[30]*x33+
    nids_sol$coefficients[31]*x34+
    nids_sol$coefficients[32]*x35+
    nids_sol$coefficients[33]*x36+
    nids_sol$coefficients[34]*x37+
    nids_sol$coefficients[35]*x38+
    nids_sol$coefficients[36]*x39+
    nids_sol$coefficients[37]*x40+
    nids_sol$coefficients[38]*x41)) 

#Ecuacion de regresion logistica estimada
maxi = max(y_estim) #valor maximo de la ecuacion estimada
mini = min(y_estim) #valor maximo de la ecuacion estimada
treshold = (mini+maxi)/2 #umbral de decision
treshold_1 = rep(c(treshold),each=length(y_estim)) #umbral de decision en forma de vector
nids_pred = (y_estim > treshold)*1 #vector de sobrevivientes predecidos
difer = (y == nids_pred)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errores = length(difer) - sum(difer) #numero de errores cometidos
porc_err = errores/length(difer)*100 #porcentaje de error

#TEST
yt = nids[125974:148516,42] #todos los elementos de la 11va columna menos el primero
yt= (yt=="anomaly")*1 #anomaly por 1
x1t = nids[125974:148516,1] 
x1t = as.numeric(x1t) 
x5t = nids[125974:148516,5] 
x5t = as.numeric(x5t) 
x6t = nids[125974:148516,6] 
x6t = as.numeric(x6t) 
x7t = nids[125974:148516,7] 
x7t = as.numeric(x7t) 
x8t = nids[125974:148516,8] 
x8t = as.numeric(x8t) 
x9t = nids[125974:148516,9] 
x9t = as.numeric(x9t) 
x10t = nids[125974:148516,10] 
x10t = as.numeric(x10t) 
x11t = nids[125974:148516,11] 
x11t = as.numeric(x11t) 
x12t = nids[125974:148516,12] 
x12t = as.numeric(x12t) 
x13t = nids[125974:148516,13] 
x13t= as.numeric(x13t) 
x14t = nids[125974:148516,14] 
x14t = as.numeric(x14t) 
x15t = nids[125974:148516,15] 
x15t = as.numeric(x15t) 
x16t = nids[125974:148516,16] 
x16t = as.numeric(x16t) 
x17t = nids[125974:148516,17] 
x17t = as.numeric(x17t) 
x18t = nids[125974:148516,18] 
x18t = as.numeric(x18t) 
x19t = nids[125974:148516,19] 
x19t = as.numeric(x19t) 
x21t = nids[125974:148516,21] 
x21t = as.numeric(x21t) 
x22t = nids[125974:148516,22] 
x22t = as.numeric(x22t) 
x23t = nids[125974:148516,23] 
x23t = as.numeric(x23t) 
x24t = nids[125974:148516,24] 
x24t = as.numeric(x24t) 
x25t = nids[125974:148516,25] 
x25t = as.numeric(x25t) 
x26t = nids[125974:148516,26] 
x26t = as.numeric(x26t) 
x27t = nids[125974:148516,27] 
x27t = as.numeric(x27t) 
x28t = nids[125974:148516,28] 
x28t = as.numeric(x28t) 
x29t = nids[125974:148516,29] 
x29t = as.numeric(x29t) 
x30t = nids[125974:148516,30] 
x30t = as.numeric(x30t) 
x31t = nids[125974:148516,31] 
x31t= as.numeric(x31t) 
x32t = nids[125974:148516,32] 
x32t = as.numeric(x32t) 
x33t = nids[125974:148516,33] 
x33t = as.numeric(x33t) 
x34t = nids[125974:148516,34] 
x34t = as.numeric(x34t) 
x35t = nids[125974:148516,35] 
x35t = as.numeric(x35t) 
x36t = nids[125974:148516,36] 
x36t = as.numeric(x36t) 
x37t = nids[125974:148516,37] 
x37t = as.numeric(x37t) 
x38t = nids[125974:148516,38] 
x38t = as.numeric(x38t) 
x39t = nids[125974:148516,39] 
x39t = as.numeric(x39t) 
x40t = nids[125974:148516,40] 
x40t = as.numeric(x40t) 
x41t = nids[125974:148516,41] 
x41t = as.numeric(x41t) 

    y_estimt =exp(nids_sol$coefficients[1]+
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
    nids_sol$coefficients[25]*x28t+
    nids_sol$coefficients[26]*x29t+
    nids_sol$coefficients[27]*x30t+
    nids_sol$coefficients[28]*x31t+
    nids_sol$coefficients[29]*x32t+
    nids_sol$coefficients[30]*x33t+
    nids_sol$coefficients[31]*x34t+
    nids_sol$coefficients[32]*x35t+
    nids_sol$coefficients[33]*x36t+
    nids_sol$coefficients[34]*x37t+
    nids_sol$coefficients[35]*x38t+
    nids_sol$coefficients[36]*x39t+
    nids_sol$coefficients[37]*x40t+
    nids_sol$coefficients[38]*x41t) / 
     (1+exp( nids_sol$coefficients[1]+
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
      nids_sol$coefficients[25]*x28t+
      nids_sol$coefficients[26]*x29t+
      nids_sol$coefficients[27]*x30t+
      nids_sol$coefficients[28]*x31t+
      nids_sol$coefficients[29]*x32t+
      nids_sol$coefficients[30]*x33t+
      nids_sol$coefficients[31]*x34t+
      nids_sol$coefficients[32]*x35t+
      nids_sol$coefficients[33]*x36t+
      nids_sol$coefficients[34]*x37t+
      nids_sol$coefficients[35]*x38t+
      nids_sol$coefficients[36]*x39t+
      nids_sol$coefficients[37]*x40t+
      nids_sol$coefficients[38]*x41t) ) 
#ecuacion de regresion logistica estimada
maxit = max(y_estimt) #valor maximo de la ecuacion estimada
minit = min(y_estimt) #valor maximo de la ecuacion estimada
tresholdt = (minit+maxit)/2 #umbral de decision
treshold_1t = rep(c(tresholdt),each=length(y_estimt)) #umbral de decision en forma de vector
nids_predt = (y_estimt > tresholdt)*1 #vector de sobrevivientes predecidos
difert = (yt == nids_predt)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errorest = length(difert) - sum(difert) #numero de errores cometidos
porc_errt = errorest/length(difert)*100 #porcentaje de error
print(porc_errt)
#matriz de confusion y resultados
xtab <- table(nids_predt,yt)
print(confusionMatrix(xtab[2:1,2:1]))

#grafica
l = nids[1:125973,42] 
lr_data <- data.frame(predictor=nids_sol$linear.predictors, prob=nids_sol$fitted.values, clases=l)
ggplot(lr_data, aes(x=predictor, y=prob, color=clases)) + geom_point()
