# 26/05/2022
# Tirso G. Ordaz García
# Econometría I
# Práctica 1

mate <- c(39,43,21,64,57,47,28,75,34,52)
calculo <- c(65,78,52,82,92,89,73,98,56,75)

plot(mate,calculo,col='deeppink4',pch=19,lwd=3,
     main='Mate vs Calculo')

hist(calculo)

# Estimación de Beta_0 y Beta_1

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

plot(mate, beta0+beta1*mate, type="l", col="blue", lwd=3)

plot(mate,calculo,col='deeppink2',pch=19,lwd=3,
     main='Mate vs Calculo')
# Juntar 2 gráficos en la misma ventana (buscar como se
# hace en ggplot)
par(new=TRUE)
plot(mate, beta0+beta1*mate, type="l", col="blue", lwd=3)

############################################################

# Función lm

model <- lm(calculo~mate)
model
# Always call summary
# Remember to check number of *s in the p value (1, 2, 3)
summary(model)
plot(mate, calculo, col='deeppink2',pch=18,lwd=3,
     abline(model, col="green", lwd=3))

# To check other graphics
plot(model)

############################################################









