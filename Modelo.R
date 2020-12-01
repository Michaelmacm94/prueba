library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(TSA)
library(tseries)
library(lubridate)
library(car)
library(plm)
library(foreign)
data_modelo <- read_csv("https://raw.githubusercontent.com/jorgehsaavedra/20201124-test-convocatoria/main/02.%20modelacion_datos/data_modelo.csv")

plot(diff(diff(data_modelo$KmT)),type = "line")

data_modelo$Fecha=as.Date(paste0(substr(data_modelo$Fecha,1,4),"-",substr(data_modelo$Fecha,5,6),"-",substr(data_modelo$Fecha,7,8)))

data_modelo$mes=month(data_modelo$Fecha,locale = Sys.getlocale("LC_TIME"),label = T)

#Modelo lineal
m1=lm(Gal~tipologia+KmT+Pas,data = data_modelo)

summary(m1)

#' Graficos normalidad
par(mfrow=c(1,3))
boxplot(m1$residuals)
hist(m1$residuals)
qqPlot(m1$residuals)

#'Pruebas de normalidad
shapiro.test(m1$residuals)

#No se cumple el supuesto de normalidad

#Serie

coplot(Gal ~ mes|tipologia, type = "l", data = data_modelo)

scatterplot(Gal ~ mes|tipologia, boxplots = FALSE, reg.line = FALSE,data = data_modelo)

library(gplots)
plotmeans(Gal ~ tipologia, main = "Heterogeneidad entre años", data = data_modelo)

plotmeans(Gal ~ mes, main = "Heterogeneidad a traves del tiempo", data = data_modelo)

mco <- lm(Gal ~ KmT, data = data_modelo)
summary(mco)

yhat <- mco$fitted.values
plot(data_modelo$KmT, data_modelo$Gal, pch = 16, xlab = "KmT", ylab = "Gal")
abline(mco, col = "red", lwd = 3)

#Modelo Dumy

fixed.dum <- lm(Gal ~ KmT + factor(mes) - 1 , data = data_modelo,)
summary(fixed.dum)

yhat <- fixed.dum$fitted
scatterplot(yhat~data_modelo$KmT|data_modelo$mes, boxplots=FALSE, xlab="KmT", ylab="yhat",smooth=FALSE)
abline(lm(data_modelo$Gal~data_modelo$KmT),lwd=3, col="red")
