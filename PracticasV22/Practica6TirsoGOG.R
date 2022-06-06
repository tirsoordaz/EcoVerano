library(readxl)
#subasta <- read_excel("Ejemplo subasta (1).xlsx")
#head(subasta)

library(readxl)
subasta <- read_excel("PracticasV22/Ejemplo subasta (1).xlsx")


edad <- subasta$Edad # x1 predictora
postores <- subasta$`Num Postores` # x2 predictora
precio <- subasta$`Precio de subasta` # variable de respuesta

datos <- data.frame(edad, postores, precio)

pairs(datos)

##### precio ~ edad

model1 <- lm(precio ~ edad)
summary(model1)

plot(edad, precio,col="deeppink",pch=20,lwd=2)
abline(model1, col="lightblue")

##### precio ~ postores

model2 <- lm(precio ~ postores)
summary(model2)

plot(postores, precio,col="deeppink",pch=20,lwd=2)
abline(model2, col="lightblue")


modelmult <- lm(precio~edad + postores)
summary(modelmult)
anova <-  anova(modelmult)

library(nortest)

error <- modelmult$residuals
error

shapiro.test(error)
ad.test(error)

boxplot(error, col="deeppink")
hist(error, col="deeppink")

library(MASS)
fitdistr(error,"normal")

###########################################################

### Continuación práctica 6

# Dado que se han cumplido todos los supuestos para este modelo,
# es viable usarlo para realizar pronósticos en el precio

# y= -1339 + 12.74x1 + 85.95x2

# si x1 = 185 añps y x2=15 postores, entonces, en promedio se espera
# tener

x1 <- 185
x2 <- 15

precio_predic <- -1339 + 12.74*x1 + 85.95*x2
precio_predic

anova(modelmult)
summary(modelmult)

###
# PREGUNTA DE EXAMEN

# interpretación del f estadistico es que si el pvalor
# es menor que alfa (0.05), entonces al menos un coeficiente
# de las variables predictoras es diferente de cero. Lo cual
# indica que se puede incluir al modelo.

# cuantiles

alpha <- 0.05

##########
# Ejercicio de clase (examen)


# -1339 + 12.74*x1 + 85.95*x2

# F-estdaístico = 120.2
f_est <- 120.2
f_est
# F_alfa

# 2 variables predictoras, 3 betas estimando, 32 observaciones-3=29
# en prueba F siempre se trabaja con cuantil derecho (FALSE)
f_alfa <- qf(0.05, 2, 29, lower.tail = FALSE)
f_alfa

# regla de rechazo cola superior
# f_est > f-alfa ---> cierto ---> se rechaza H0 y no todos los
# coeficientes serán iguales a 0
f_est>f_alfa

# si la comparación resulta verdadera, entonces se rechaza H_0,
# obteniendo que al menos un beta_i es distinto de cero

### Conclusión del modelo

# El modelo es confiable ya que se han comprobado varios factores
# que favorecen su precisión. Al realizar el análisis de la varianza
# se encontró que ambas variables proveen información útil al 
# modelo para predecir el precio final de la subasta.
# 
# Finalmente se comprueba que los errores
# satisfacen los supuestos de normalidad con las pruebas. Tampoco
# Se observan datos especiales en el boxplot.
# 
# R cuadrada ajustadade .88 que dice que el 88% de los datos se ajustan al model.offset(
#   Si se ocupa una sola variable predictora...
# 
# Reportar los pvalores globales e individuales.
# 
# R cuadrada ajustado, pvalors individuales significativos, prueba global significativa.
# 
# Validación de los supuestos del modelo.
# - Valor esperado 0
# - Desviación estándar finita (fitdistr)
# - Normalidad qqplot y pruebas









