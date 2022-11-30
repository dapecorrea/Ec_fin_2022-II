#Modelos VAR 
# Tarea 4 
# Diego Andres Penaranda Correa - 201910807

### 
# 1) Datos
# 2) Visualización
# 3) Caracterización
# 4) Construcción Modelos y Selección
# 5) Revisión Residuos
# 6) Pronóstico
if (!require("MTS")){install.packages('MTS');library(MTS)}else{library(MTS)}
library(MTS)
if (!require("devtools")){install.packages("devtools");library(devtools)}else{library(devtools)}
library(fpp3)

library(forecast)
library(tidyverse)
library(fBasics)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(ggplot2)
library(tseries)
library(dygraphs)
library(dplyr)
library(stats)
options(warn = - 1) 


install.packages("MTS_VERSION.tar.gz",repos=NULL,type="source")
library(MTS)

## 1) Datos
##VAR con funciOn VAR()

#da=read.table("q-gdp-ukcaus.txt",header=T)
start<-format(as.Date("2019-05-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-01"),"%Y-%m-%d")

#--------- Función para bajar precios y generar rendimientos:
pc<-function(simbolo) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo,  from=start, to= end, auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  #rend<-periodReturn(datos, period = "daily", type='arithmetic')                         
  #------ --Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, datos, envir = .GlobalEnv)
}

#------- Llamar la función para cada activo particular:

pc('JPY=X')
pc("SPY")
pc("AAPL")
pc("GOOGL")
#
in1=`JPY=X`
in2= `SPY`
in3=AAPL
in4=GOOGL


prices<-merge.xts(in1, in2, in3, in4) %>% na.omit()
dim(prices)
head(prices)
periodicity(prices)
?dygraph
?dyOptions
colnames(prices)<-c("JPY",  "SPY", "AAPL", "GOOGL")
dygraph(prices, main = "JPY=X,SPY, AAPL & GOOGL") %>%
  dyAxis("y", label = "Prices") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) 
#it only allows three stakes
library(vars)
# ------- Nivel Autorregresivo:
datos=merge.xts( in1, in2, in3, in4, join='inner')%>% na.omit()
nivelk=VARselect(datos, lag.max = 3 , type = "const")
nivelk$selection # se enuentra sin incluir el lag el resultante es 3

johatest=ca.jo(datos, type = "trace", K=2, ecdet ="none", spec = "longrun") #K is the number of lags to use in the vector autoregressive model and is set this to the minimum, K=2.
summary(johatest)  


###############------Parte 1
####------Modelo VECM
#Literales A), B) y C) 
if (!require("tsDyn")){install.packages('tsDyn');library(tsDyn)}else{library(tsDyn)}
library(urca)
vecm1 = VECM(datos, lag=2, r=1, estim = ("ML"))
summary(vecm1)

# Diagnóstico: Las pruebas para ello funcionan con VAR, hay que transformar:
varmod1 = vec2var(johatest, r=1)

# correlación:
ade1 = serial.test(varmod1, lags.pt = 5, type = "BG")
ade1   # H0: datos independientes, H1: datos dependientes. 

hete1 = arch.test(varmod1, lags.multi = 15, multivariate.only = TRUE)
hete1  #efecto arch H0: no efecto arch, H1: hay efecto arch


# Análisis Impulso - Respuesta: Impulse response
library(vars)

m1irf = irf(varmod1, n.ahead = 5, boot = TRUE)
plot(m1irf)  #predicción eje Y: es la var dependiente, acorde el impulso X.


# Con OIR (orthogonal- Imp-Resp) Se descompone la matriz de vari-cov a una matriz triangular inferior con elementos positivos diagonales
suma = summary(vecm1)
# suma
choles = t(chol(suma$sigma))
choles  # Ej. un choque en JPY genera un efecto contemporáneo en Rusell, pero no viceversa.
# o, un choque en el rendimiento de los bonos del tesoro genera un efecto contemporáneo en divisa JPY y en Rusell
# Para ver los otros efectos, cambiar el orden de los datos.

## Prediction:
pred=predict(varmod1, n.ahead = 5, ci = 0.95)
par(mar = c(1, 1, 1, 1))
plot(pred)


###################
#D) Modelo ARDL para S&P500
library(ARDL)
library(TSstudio)
#install.packages("ARDL")

#Datos:

#da=read.table("q-gdp-ukcaus.txt",header=T)
start<-format(as.Date("2019-05-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-01"),"%Y-%m-%d")

#--------- Función para bajar precios y generar rendimientos:
pc<-function(simbolo) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo,  from=start, to= end, auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  #rend<-periodReturn(datos, period = "daily", type='arithmetic')                         
  #------ --Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, datos, envir = .GlobalEnv)
}

#------- Llamar la función para cada activo particular:

pc('JPY=X')
pc("SPY")
pc("AAPL")
pc("GOOGL")
#
in1=`JPY=X`
in2= SPY
in3=AAPL
in4=GOOGL


prices<-merge.xts(in1, in2, in3, in4) %>% na.omit()

dd<-zooreg(prices)

dim(prices)
head(prices)
str(prices)

data(dd)
data
?data_frame
str(prices)
head(prices)
precios=merge.xts( in1, in2, in3, in4, join='inner')
precios=na.omit(precios)
#A data frame with 55 rows and 5 variables. Time period from 1974:Q1 until 1987:Q3.
# LRM: logarithm of real money, M2
# LRY: logarithm of real income
# LPY: logarithm of price deflator
# IBO: bond rate
# IDE: bank deposit rate

## Formato ts
y <- in2
ri <- in1
pd <- in3
br <- in4
#dr <- ts(denmark$IDE, start = c(1974,1), frequency = 4)
#Plots:
y<-ts(in2, frequency=252)
ri<-ts(in1, frequency=252)
pd<-ts(in3, frequency=252)
br<-ts(in4,frequency=252)
prices=merge.xts( `JPY=X`,SPY, AAPL, GOOGL, join='inner')
prices=merge.xts( in1,in2, in3, in4, join='inner')
#ts.plot(dr)

##Prueba estacionariedad
library(fUnitRoots)
pacf(in2) #para hallar el lag de esa serie
adfTest(in2, lags=10)
adfTest(in1, lags=12)
adfTest(in3, lags = 12)
adfTest(in4, lags =12)
#adfTest(dr, lags=12)


##Modelo ARDL
library(ARDL)
y <- ts(prices$SPY, start = c(2019,1), frequency = 7)
ri <- ts(prices$AAPL, start = c(2019,1), frequency = 7)
pd <- ts(prices$GOOGL, start = c(2019,1), frequency = 7)
br <- ts(prices$JPY, start = c(2019,1), frequency = 7)
prices$GOOGL
precios<-ts.union(y,ri,pd,br)

###Obtener el optimo global del modelo ARDL/#Seleccion automatica:
model_grid <- auto_ardl(y ~ ri + pd + br, data = precios,
                        max_order = c(5,4,4,4), grid = TRUE)
model_grid

##Modelo optimo
mod1 <- ardl(y ~ ri + pd + br, data = precios, order = c(5,1,1,0))
summary(mod1)



