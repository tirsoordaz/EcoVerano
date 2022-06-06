# Reporte Reg Lineal


# https://www.kaggle.com/datasets/surajjha101/stores-area-and-sales-data



stores <- read_csv("PracticasV22/Stores.csv")
head(stores)

items <- stores$Items_Available
sales <- stores$Store_Sales

x <- items
y <- sales
n <- length(items)

# 1 intro
#####################################################################
# 2 análisis descriptivo
summary(x)
sd(x)
summary(y)
sd(y)

hist(y)
plot(x,y)
boxplot(x)
boxplot(y)

###################################################################
# 3 modelo de regresión

model <- lm(y~x)

plot(x, y, col='deeppink2',pch=20,lwd=0.1,
     abline(model, col="blue", lwd=3),
     xlab = "Número de ítems",
     ylab = "Ventas")
################################################################
# 4 análisis de residuales con prueba de hipótesis

residuo <- model$residuals
head(residuo)

plot(residuo,col="deeppink")
lines(1:n,rep(0,n), col="blue")

# boxplot a residuos e histograma a residuos
boxplot(residuo, col="deeppink")
#boxplot sirve para identificar outliers fuera de la caja
hist(residuo, col="deeppink")
# cuantiles teóricos vs de la muestra
# Realizar un análisis de residuos de forma gráfica
qqnorm(residuo,col="deeppink",lwd="2");qqline(residuo,col="blue")

## Pruebas de normalidad
# H_0: Los residuales siguen una distribución normal
# vs H_1: Los residuales NO siguen una distribución normal

shapiro.test(residuo)
#### TENEMOS pvalue < 0.05!!! Loque indica que los datos no tienen una distribución normal
# si pvalue > 0.05 entonces los datos siguen dist normal
##############################################################################
# 5 Estimación de S cuadrada y coeficiente de variación

anova(model)
SSE <- 261910000000
Scuad <-  SSE/(n-2)
S <- sqrt(Scuad)

coef_var <- 100*S/mean(y)
########################################################
# 6 prueba de hipótesis para B1
summary(model)

alpha <- 0.05
pvalor <- 0.00306

if(pvalor<alpha){
  print("Se rechaza H_0 a favor de H_1, es decir
        la variable predictora X aporta información al modelo")
}else{
  print("No hay suficiente evidencia para rechazar H_0
        es decir que la variable X no aporta información al modelo")
}
#######################################################
# 7 intervalo de confianza para B1, coef de correlación y de determinación con su interpretación

datosx <- seq(from=min(x),to=max(x),
              length.out=100)
# crea secuencia del min de x hasta el max de mate con 100 puntos

# Hacer pronósticos del modelo
Intervalo <- predict(object=model,
                     newdata = data.frame(x=datosx),
                     interval = "confidence", level=0.95)
head(Intervalo)

plot(x,y,col="deeppink",pch=20,lwd=0.01)
abline(model,col="blue")

lines(x=datosx,y=Intervalo[,2],col="lightskyblue",lty=3,lwd=2)
lines(x=datosx,y=Intervalo[,3],col="lightskyblue",lty=3, lwd=2)

r <- cor(x,y)
r
Rcuad <- summary(model)$r.squared




