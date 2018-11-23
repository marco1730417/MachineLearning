
path="C:/Users/Diego/Desktop/breast_cancer.csv" # datos titanic
titanic = as.matrix(read.csv(file=path, header=FALSE, sep=",") )#dmat distance matrix (format=matrix)

### TRAIN
y = titanic[2:100,11] #todos los elementos de la 11va columna menos el primero
y = as.numeric(y) #cambio de caracter a numerico
y = (y=="2")*1 #cambio de male o female a 0 o 1
x1 = titanic[2:100,2] #todos los elementos de la 2da columna menos el primero
x1 = as.numeric(x1) #cambio de caracter a numerico
x2 = titanic[2:100,3] #todos los elementos de la 3era columna menos el primero
x2 = as.numeric(x2) #cambio de caracter a numerico
x3 = titanic[2:100,4] #todos los elementos de la 4ta columna menos el primero
x3 = as.numeric(x3) #cambio de caracter a numerico
x4 = titanic[2:100,5] #todos los elementos de la 5ta columna menos el primero
x4 = as.numeric(x4) #cambio de caracter a numerico
x5 = titanic[2:700,6] #todos los elementos de la 6ta columna menos el primero
x5 = as.numeric(x4) #cambio de caracter a numerico
x6 = titanic[2:700,7] #todos los elementos de la 7ma columna menos el primero
x6 = as.numeric(x4) #cambio de caracter a numerico
x7 = titanic[2:700,8] #todos los elementos de la 8va columna menos el primero
x7 = as.numeric(x4) #cambio de caracter a numerico
x8 = titanic[2:700,9] #todos los elementos de la 9na columna menos el primero
x8 = as.numeric(x4) #cambio de caracter a numerico
x9 = titanic[2:700,10] #todos los elementos de la 10ma columna menos el primero
x9 = as.numeric(x4) #cambio de caracter a numerico
x10= titanic[2:700,5] #todos los elementos de la 5ta columna menos el primero
x10 = as.numeric(x4) #cambio de caracter a numerico


#par(mfrow=c(2,3)) # varios gráficos en una sola pantalla
#plot(x1, y, pch = 19, col=12, main="Diagrama de Dispersión: X1 vs. Y") #grafico de dispersion x1
tit = glm(y ~ x1 + x2 + x3 + x4,family=binomial()) #calculo de la regresion lineal (funcion directa)
print (summary(tit)) #resumen de los resultados de la regresion lineal 
y_estim = exp(tit$coefficients[1]+tit$coefficients[2]*x1+tit$coefficients[3]*x2+tit$coefficients[4]*x3+tit$coefficients[5]*x4) / (1+exp(tit$coefficients[1]+tit$coefficients[2]*x1+tit$coefficients[3]*x2+tit$coefficients[4]*x3+tit$coefficients[5]*x4)) #ecuacion de regresion logistica estimada
maxi = max(y_estim) #valor maximo de la ecuacion estimada
mini = min(y_estim) #valor maximo de la ecuacion estimada
treshold = (mini+maxi)/2 #umbral de decision
treshold_1 = rep(c(treshold),each=length(y_estim)) #umbral de decision en forma de vector
survived_pred = (y_estim > treshold)*1 #vector de sobrevivientes predecidos
difer = (y == survived_pred)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errores = length(difer) - sum(difer) #numero de errores cometidos
porc_err = errores/length(difer)*100 #porcentaje de error
print (porc_err)


### TEST
yy = titanic[101:699,11] #todos los elementos de la 11va columna menos el primero
yy = as.numeric(yy) #cambio de caracter a numerico
yy = (yy=="2")*1 #cambio de male o female a 0 o 1
x11 = titanic[101:699,2] #todos los elementos de la 2da columna menos el primero
x11 = as.numeric(x11) #cambio de caracter a numerico
x22 = titanic[101:699,3] #todos los elementos de la 3era columna menos el primero
x22 = as.numeric(x22) #cambio de caracter a numerico
x33 = titanic[101:699,4] #todos los elementos de la 4ta columna menos el primero
x33 = as.numeric(x33) #cambio de caracter a numerico
x44 = titanic[101:699,5] #todos los elementos de la 5ta columna menos el primero
x44 = as.numeric(x44) #cambio de caracter a numerico
# x5 = titanic[2:700,6] #todos los elementos de la 6ta columna menos el primero
# x5 = as.numeric(x4) #cambio de caracter a numerico
# x6 = titanic[2:700,7] #todos los elementos de la 7ma columna menos el primero
# x6 = as.numeric(x4) #cambio de caracter a numerico
# x7 = titanic[2:700,8] #todos los elementos de la 8va columna menos el primero
# x7 = as.numeric(x4) #cambio de caracter a numerico
# x8 = titanic[2:700,9] #todos los elementos de la 9na columna menos el primero
# x8 = as.numeric(x4) #cambio de caracter a numerico
# x9 = titanic[2:700,10] #todos los elementos de la 10ma columna menos el primero
# x9 = as.numeric(x4) #cambio de caracter a numerico
# x10= titanic[2:700,5] #todos los elementos de la 5ta columna menos el primero
# x10 = as.numeric(x4) #cambio de caracter a numerico
y_estim2 = exp(tit$coefficients[1]+tit$coefficients[2]*x11+tit$coefficients[3]*x22+tit$coefficients[4]*x33+tit$coefficients[5]*x44) /
  
  (1+exp(tit$coefficients[1]+tit$coefficients[2]*x11+tit$coefficients[3]*x22+tit$coefficients[4]*x33+tit$coefficients[5]*x44)) #ecuacion de regresion logistica estimada

maxi2 = max(y_estim2) #valor maximo de la ecuacion estimada
mini2 = min(y_estim2) #valor maximo de la ecuacion estimada
treshold2 = (mini2+maxi2)/2 #umbral de decision
treshold_11 = rep(c(treshold2),each=length(y_estim2)) #umbral de decision en forma de vector
survived_pred2 = (y_estim2 > treshold2)*1 #vector de sobrevivientes predecidos
difer2 = (yy == survived_pred2)*1 #compara los valores predecidos con los reales (1 si acierta 0 si no acierta)
errores2 = length(difer2) - sum(difer2) #numero de errores cometidos
porc_err2 = errores2/length(difer2)*100 #porcentaje de error
print (porc_err2)

