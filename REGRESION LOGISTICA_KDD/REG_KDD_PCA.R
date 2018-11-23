path="C:/Users/Marco Ambuludi/Dropbox/Tesis Marco/DATASET/KDDPCAVARIANZAS.csv" 
nids = as.matrix(read.csv(file=path, header=FALSE, sep=";") )#dmat distance matrix (format=matrix)
y = nids[1:125973,25] 
y = (y=="anomaly")*1 #anomaly por 1
x1 = nids[1:125973,1] 
x1 = as.numeric(x1) 
x2 = nids[1:125973,2] 
x2 = as.numeric(x2) 
x3 = nids[1:125973,3] 
x3 = as.numeric(x3) 
x4 = nids[1:125973,4] 
x4 = as.numeric(x4) 
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
x10= as.numeric(x10) 
#TRAINING 
nids_sol = glm(y ~ x1 + x2 + x3 + x4 + x5+x6+x7+x8+x9 + x10 ,family=binomial()) #calculo de la regresion lineal (funcion directa)
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
    nids_sol$coefficients[11]*x10
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
    nids_sol$coefficients[11]*x10) ) 

maxi = max(y_estim) #valor maximo de la ecuacion estimada
mini = min(y_estim) #valor minimo de la ecuacion estimada
treshold = (mini+maxi)/2 #umbral de decision
treshold_1 = rep(c(treshold),each=length(y_estim)) #umbral de decision en forma de vector
survived_pred = (y_estim > treshold)*1 
difer = (y == survived_pred)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errores = length(difer) - sum(difer) #numero de errores cometidos
porc_err = errores/length(difer)*100 #porcentaje de error de entrenamiento
print (porc_err)   

### TEST

y2 = nids[125974:148516,25] 
y2 = (y2=="anomaly")*1 #anomaly por 1
x11 = nids[125974:148516,1] 
x11 = as.numeric(x11) 
x22 = nids[125974:148516,2] 
x22 = as.numeric(x22) 
x33 = nids[125974:148516,3] 
x33 = as.numeric(x33) 
x44 = nids[125974:148516,4] 
x44 = as.numeric(x44) 
x55 = nids[125974:148516,5] 
x55 = as.numeric(x55) 
x66 = nids[125974:148516,6] 
x66 = as.numeric(x66) 
x77 = nids[125974:148516,7] 
x77 = as.numeric(x77) 
x88 = nids[125974:148516,8] 
x88 = as.numeric(x88) 
x99 = nids[125974:148516,9] 
x99 = as.numeric(x99) 
x101 = nids[125974:148516,10] 
x101= as.numeric(x101) 

y_estim2 =exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x11+
    nids_sol$coefficients[3]*x22+
    nids_sol$coefficients[4]*x33+
    nids_sol$coefficients[5]*x44+
    nids_sol$coefficients[6]*x55+
    nids_sol$coefficients[7]*x66+
    nids_sol$coefficients[8]*x77+
    nids_sol$coefficients[9]*x88+
    nids_sol$coefficients[10]*x99+
    nids_sol$coefficients[11]*x101
  
) / (1+exp(
  nids_sol$coefficients[1]+
    nids_sol$coefficients[2]*x11+
    nids_sol$coefficients[3]*x22+
    nids_sol$coefficients[4]*x33+
    nids_sol$coefficients[5]*x44+
    nids_sol$coefficients[6]*x55+
    nids_sol$coefficients[7]*x66+
    nids_sol$coefficients[8]*x77+
    nids_sol$coefficients[9]*x88+
    nids_sol$coefficients[10]*x99+
    nids_sol$coefficients[11]*x101
  ) ) 
maxi2 = max(y_estim2) #valor maximo de la ecuacion estimada
mini2 = min(y_estim2) #valor minimo de la ecuacion estimada
treshold2 = (mini2+maxi2)/2 #umbral de decision
treshold_11 = rep(c(treshold2),each=length(y_estim2)) #umbral de decision en forma de vector
nids_predt = (y_estim2 > treshold2)*1 
difer2 = (y2 == nids_predt)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errores2 = length(difer2) - sum(difer2) #numero de errores cometidos
porc_err2 = errores2/length(difer2)*100 #porcentaje de error de prueba
print (porc_err2)

#matriz de confusion y resultados
xtab <- table(nids_predt,y2)
print(confusionMatrix(xtab[2:1,2:1]))
