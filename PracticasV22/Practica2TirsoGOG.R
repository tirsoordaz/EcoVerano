# 26/05/2022
# Tirso G. Ordaz García
# Econometría I
# Práctica 2

# install.packages("readr)
library(readr)

BIMBO <- read_csv("PracticasV22/BIMBOA.MX.csv")
names(BIMBO)

# Select close column
close <- BIMBO$Close

head(close)

n <- length(close)

plot(close, type="s", col="deeppink",
     main = "Bimbo close", lwd=1)

##################################################
# Modelo de regresión simple

x <- 1:n

model <- lm(close~x)

plot()


