library(pROC)
library(doParallel)

path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/UNSWPCA.csv" # datos nids
nids = as.matrix(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
y = nids[1:82332,40] 
y = (y=="anomaly")*1 #anomaly por 1
x1 = nids[1:82332,1] 
x1 = as.numeric(x1) 
x2 = nids[1:82332,2] 
x2 = as.numeric(x2) 
x3 = nids[1:82332,3] 
x3 = as.numeric(x3) 
x4 = nids[1:82332,4] 
x4 = as.numeric(x4) 
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

#TRAINING 
nids_sol = glm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18,family=binomial()) #calculo de la regresion lineal (funcion directa)
print (summary(nids_sol)) #resumen de los resultados de la regresion lineal 
y_estim =exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x1+
    nids_sol$coefficients[3]*x2+
    nids_sol$coefficients[4]*x3+
    nids_sol$coefficients[5]*x4+
    nids_sol$coefficients[6]*x5+
    nids_sol$coefficients[7]*x6+
    nids_sol$coefficients[8]*x7+
    nids_sol$coefficients[9]*x8+
    nids_sol$coefficients[10]*x9+
    nids_sol$coefficients[11]*x10+
    nids_sol$coefficients[12]*x11+
    nids_sol$coefficients[13]*x12+
    nids_sol$coefficients[14]*x13+
    nids_sol$coefficients[15]*x14+
    nids_sol$coefficients[16]*x15+
    nids_sol$coefficients[17]*x16+
    nids_sol$coefficients[18]*x17+
    nids_sol$coefficients[19]*x18
) / (1+exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x1+
    nids_sol$coefficients[3]*x2+
    nids_sol$coefficients[4]*x3+
    nids_sol$coefficients[5]*x4+
    nids_sol$coefficients[6]*x5+
    nids_sol$coefficients[7]*x6+
    nids_sol$coefficients[8]*x7+
    nids_sol$coefficients[9]*x8+
    nids_sol$coefficients[10]*x9+
    nids_sol$coefficients[11]*x10+
    nids_sol$coefficients[12]*x11+
    nids_sol$coefficients[13]*x12+
    nids_sol$coefficients[14]*x13+
    nids_sol$coefficients[15]*x14+
    nids_sol$coefficients[16]*x15+
    nids_sol$coefficients[17]*x16+
    nids_sol$coefficients[18]*x17+
    nids_sol$coefficients[19]*x18
  
) ) 

#ecuacion de regresion logistica estimada
maxi = max(y_estim) #valor maximo de la ecuacion estimada
mini = min(y_estim) #valor maximo de la ecuacion estimada
treshold = (mini+maxi)/2 #umbral de decision
treshold_1 = rep(c(treshold),each=length(y_estim)) #umbral de decision en forma de vector
survived_pred = (y_estim > treshold)*1 
difer = (y == survived_pred)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errores = length(difer) - sum(difer) #numero de errores cometidos
porc_err = errores/length(difer)*100 #porcentaje de error de entramiento
print (porc_err)   # 

#TEST
yt = nids[82333:257673,40] #todos los elementos de la 11va columna menos el primero
yt= (yt=="anomaly")*1 #anomaly por 1
x1t = nids[82333:257673,1] 
x1t = as.numeric(x1t)
x2t = nids[82333:257673,2] 
x2t = as.numeric(x2t)
x3t = nids[82333:257673,3] 
x3t = as.numeric(x3t)
x4t = nids[82333:257673,4] 
x4t = as.numeric(x4t)
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

y_estimt =exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x1t+
    nids_sol$coefficients[3]*x2t+
    nids_sol$coefficients[4]*x3t+
    nids_sol$coefficients[5]*x4t+
    nids_sol$coefficients[6]*x5t+
    nids_sol$coefficients[7]*x6t+
    nids_sol$coefficients[8]*x7t+
    nids_sol$coefficients[9]*x8t+
    nids_sol$coefficients[10]*x9t+
    nids_sol$coefficients[11]*x10t+
    nids_sol$coefficients[12]*x11t+
    nids_sol$coefficients[13]*x12t+
    nids_sol$coefficients[14]*x13t+
    nids_sol$coefficients[15]*x14t+
    nids_sol$coefficients[16]*x15t+
    nids_sol$coefficients[17]*x16t+
    nids_sol$coefficients[18]*x17t+
    nids_sol$coefficients[19]*x18t
) /
  (1+exp(
    nids_sol$coefficients[1]+
      nids_sol$coefficients[2]*x1t+
      nids_sol$coefficients[3]*x2t+
      nids_sol$coefficients[4]*x3t+
      nids_sol$coefficients[5]*x4t+
      nids_sol$coefficients[6]*x5t+
      nids_sol$coefficients[7]*x6t+
      nids_sol$coefficients[8]*x7t+
      nids_sol$coefficients[9]*x8t+
      nids_sol$coefficients[10]*x9t+
      nids_sol$coefficients[11]*x10t+
      nids_sol$coefficients[12]*x11t+
      nids_sol$coefficients[13]*x12t+
      nids_sol$coefficients[14]*x13t+
      nids_sol$coefficients[15]*x14t+
      nids_sol$coefficients[16]*x15t+
      nids_sol$coefficients[17]*x16t+
      nids_sol$coefficients[18]*x17t+
      nids_sol$coefficients[19]*x18t
  ) ) 
maxit = max(y_estimt) #valor maximo de la ecuacion estimada
minit = min(y_estimt) #valor maximo de la ecuacion estimada
tresholdt = (minit+maxit)/2 #umbral de decision
treshold_1t = rep(c(tresholdt),each=length(y_estimt)) #umbral de decision en forma de vector
nids_predt = (y_estimt > tresholdt)*1 
difert = (yt == nids_predt)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errorest = length(difert) - sum(difert) #numero de errores cometidos
porc_errt = errorest/length(difert)*100 #porcentaje de error de prueba
print (porc_errt)   
#matriz de confusion y resultados
xtab <- table(nids_predt,yt)
print(confusionMatrix(xtab[2:1,2:1]))
# CURVAS ROC
rocPano <- plot.roc(nids_predt,yt , percent = TRUE, main="Curva Roc")
