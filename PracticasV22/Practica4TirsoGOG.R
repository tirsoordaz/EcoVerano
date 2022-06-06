# 31/05/2022
# Tirso G. Ordaz García
# Econometría I
# Práctica 4

mate <- c(39,43,21,64,57,47,28,75,34,52)
calculo <- c(65,78,52,82,92,89,73,98,56,75)

x <- mate
y <- calculo

xcuad <- x^2
ycuad <- y^2

n <- length(x)
xy <- x*y
xmean <- mean(x)
ymean <- mean(y)

SCxy <- sum(xy)-n*xmean*ymean
SCxy
SCx <- sum(xcuad)-n*(xmean)^2
SCx
SCy <- sum(ycuad)-n*(ymean)^2
SCy

beta1 <- SCxy/SCx
beta1
beta0 <- ymean-beta1*xmean
beta0

###################################################

## Estimación de S^2

SSE <- SCy - beta1*SCxy
SSE

Scuad <- SSE/(n-2)
Scuad

S <- sqrt(Scuad)
S

CV <- 100*S/ymean
CV
# Recordar que el cv no debe superar 10%

##################################################
#### Inferencia sobre Beta_1

## H_0: beta1=0 vs H_1: beta1 ≠ 0
alpha <- 0.05 # nivel de confianza
df <- n-2 # grados de libertad de la dist t-student
talfad <- qt(alpha/2,df,lower.tail = FALSE)
talfai <- qt(alpha/2,df,lower.tail = TRUE)
talfad
talfai

tc <- beta1/(S/sqrt(SCx))
tc

# como tc es mayor que talfad, entonces significa que cae
# dentro de la regíon de rechazo y rechazamos la hipótesis nula

pvalor <- 2*pt(-abs(tc),df)
pvalor
if(pvalor<alpha){
  print("Se rechaza H_0 a favor de H_1, es decir
        la variable predictora X aporta información al modelo")
}else{
  print("No hay suficiente evidencia para rechazar H_0
        es decir que la variable X no aporta información al modelo")
}

##################

# intervalos de confianza para beta1

Sbeta1 <- S/SCx
Linf <- beta1-(talfad)*Sbeta1
Linf

Lsup <- beta1+(talfad)*Sbeta1
Lsup

r <- SCxy/sqrt(SCx*SCy)
r #Fuerte de manera positiva

rcuad <- (1-SSE/SCy)*100
rcuad
# Porcentaje de variabilidad de la recta a los datos

############################

model <- lm(calculo~mate)
model
# Always call summary
# Remember to check number of *s in the p value (1, 2, 3)
summary(model)
anova(model)

########################################################
# Análisis de residuales

residuo <- model$residuals
head(residuo)

plot(residuo, type="b",col="deeppink")
lines(1:n,rep(0,n), col="blue")

# boxplot a residuos e histograma a residuos
boxplot(residuo, col="deeppink")
#boxplot sirve para identificar outliers fuera de la caja
hist(residuo, col="deeppink")
# cuantiles teóricos vs de la muestra
# Realizar un análisis de residuos de forma gráfica
qqnorm(residuo,col="deeppink",lwd="2");qqline(residuo,col="blue")

####################################

## Pruebas de normalidad
# H_0: Los residuales siguen una distribución normal
# vs H_1: Los residuales NO siguen una distribución normal

shapiro.test(residuo)
# si p value es mayor a 0.05, no se rechaza entonces no existe evidencia para
#decir que los errores no siguen una distribución normal


# si pvalue > 0.05 entonces los datos siguen dist normal

# install.packages("nortest")
library(nortest)
ad.test(residuo)

# install.packages("MASS")
library("MASS")
require(MASS)
fitdistr(residuo,"normal")


#####################################
## Calcular intervalos de confianza

par(mfrow=c(1,2))
pronostico <- model$fitted.values
#pronostico2
#pronostico1 <- beta0+beta1*mate
#plot(pronostico2, col="deeppink")
#plot(pronostico1,col="blue")
Sy <-  S*sqrt((1/n)+((mate-xmean)^2/SCx))
Sy

Spredict <- S*sqrt(1+(1/n)+((mate-xmean)^2/SCx))
Spredict

# ic para el promedio de y
Liy <- pronostico-talfad*Sy
Lsy <- pronostico+talfad*Sy
Liy
Lsy
# ic para valor particular dado un x_p

Lipredict <- pronostico - talfad*Spredict
Lipredict
Lspredict <- pronostico + talfad*Spredict
Lspredict
# Por qué usamos talfad?
# Para que siempre se quede positivo, sino se inver-
# tirían los límites

datosx <- seq(from=min(mate),to=max(mate),
              length.out=10)
# crea secuencia del min de mate hasta el max de mate con 100 puntos

# Hacer pronósticos del modelo
Intervalo <- predict(object=model,
                     newdata = data.frame(mate=datosx),
                     interval = "confidence", level=0.95)
head(Intervalo)

plot(mate,calculo,col="deeppink",pch=20,lwd=3)
abline(model,col="blue")

lines(x=datosx,y=Intervalo[,2],col="lightskyblue",lty=3)
lines(x=datosx,y=Intervalo[,3],col="lightskyblue",lty=3) 

####################

datosx <- seq(from=min(mate),to=max(mate),
              length.out=100)
# crea secuencia del min de mate hasta el max de mate con 100 puntos

# Hacer pronósticos del modelo
IntervaloPred <- predict(object=model,
                     newdata = data.frame(mate=datosx),
                     interval = "predict", level=0.95)
head(IntervaloPred)

plot(mate,calculo,col="deeppink",pch=20,lwd=2)
abline(model,col="blue")

lines(x=datosx,y=IntervaloPred[,2],col="lightskyblue",lty=3)
lines(x=datosx,y=IntervaloPred[,3],col="lightskyblue",lty=3)
