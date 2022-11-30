#Modelos VAR 

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
  datos<-datos[,6]
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



dd<-zooreg(prices)

dim(prices)
head(prices)
str(prices)

data(dd)
data
?data_frame
str(prices)
head(prices)
prices=merge.xts( in1, in2, in3, in4, join='inner')%>% na.omit()
#A data frame with 55 rows and 5 variables. Time period from 1974:Q1 until 1987:Q3.
# LRM: logarithm of real money, M2
# LRY: logarithm of real income
# LPY: logarithm of price deflator
# IBO: bond rate
# IDE: bank deposit rate

## Formato ts
ts()
y <- ts(in2)
ri <- in1
pd <- in3
br <- in4
y <- ts(in2, frequency = 4)
ri <- ts(in1, frequency = 4)
pd <- ts(in3, frequency = 4)
br <- ts(in4, frequency = 4)
#dr <- ts(denmark$IDE, start = c(1974,1), frequency = 4)
#Plots:
ts.plot(y)
ts.plot(ri)
ts.plot(pd)
ts.plot(br)
#ts.plot(dr)

#dr <- ts(denmark$IDE, start = c(1974,1), frequency = 4)
#Plots:
ts.plot(in2)
ts.plot(in1)
ts.plot(in3)
ts.plot(in4)
#ts.plot(dr)

##Prueba estacionariedad
library(fUnitRoots)
pacf(in2) #para hallar el lag de esa serie
adfTest(y, lags=10)
adfTest(in1, lags=12)
adfTest(in3, lags = 12)
adfTest(in4, lags =12)
#adfTest(dr, lags=12)

##Modelo ARDL
library(ARDL)
head(prices)
data(datos)
data %>% na.omit()
colnames(prices) <- c("JPY","SPY","AAPL", "GOOGL")
data<-data(datos)
data(package = .packages(all.available = TRUE))
mod1 <- ardl(SPY ~ JPY + AAPL + GOOGL, data=prices, order = c(2,1,1,2),
             na.action=na.exclude)

mod1 <- ardl(SPY.Close ~ JPY.X.Close + AAPL.Close + GOOGL.Close, data=datos,
             order = c(2,1,1,2), na.action=na.exclude)
summary(mod1)
?ardl
##Con tendencia, poque Y tenia tendencia:
mod1_ten <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM),
                 data = denmark, order = c(3,1,3,2))
summary(mod1_ten)


#Selección automatica:
mod_best <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                      max_order = c(5,4,4,4))
mod_best$top_orders

###Obtener el optimo global del modelo ARDL:
model_grid <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                        max_order = c(5,4,4,4), grid = TRUE)
model_grid
help(auto_ardl)

#------------
prices=merge.xts( in1, in2, in3, in4, join='inner')%>% na.omit()
y <- ts(in2, frequency = 4)

ri <- ts(in1, frequency = 4)
pd <- ts(in3, frequency = 4)
br <- ts(in4, frequency = 4)
str(ri)
#dr <- ts(denmark$IDE, start = c(1974,1), frequency = 4)
#Plots:
ts.plot(y)
ts.plot(ri)
ts.plot(pd)
ts.plot(br)
#ts.plot(dr)

##Prueba estacionariedad
library(fUnitRoots)
adfTest(y, lags=3)
adfTest(ri, lags=12)
adfTest(pd, lags = 12)
adfTest(br, lags =12)
#adfTest(dr, lags=12)
colnames(prices) <- c("JPY","SPY","AAPL", "GOOGL")
prices<-merge.xts(`JPY=X`,SPY,AAPL,GOOGL) %>% na.omit()
##Modelo ARDL
library(ARDL)
pc
mod1 <- ardl(SPY ~ JPY+AAPL+GOOGL,
             data = prices, order = c(3,1,3,2))
summary(mod1)

##Con tendencia, poque Y tenia tendencia:
mod1_ten <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM),
                 data = denmark, order = c(3,1,3,2))
summary(mod1_ten)


#Selección automatica:
mod_best <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                      max_order = c(5,4,4,4))
mod_best$top_orders

###Obtener el optimo global del modelo ARDL:
model_grid <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark,
                        max_order = c(5,4,4,4), grid = TRUE)
model_grid


#############-------Parte II
#######-----VaR
start<-format(as.Date("2019-05-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-01"),"%Y-%m-%d")
#--------- Funcion para bajar precios y generar rendimientos:
rend<-function(simbolo) {
  ##---------Obtener precios de yahoo finance:
  precios<-getSymbols(simbolo, src = "yahoo", from= start, to= end, auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  precios<-na.omit(precios)
  ##--------Mantener el precio de interis:
  precios<-precios[,6]
  ##--------Rendimientos log:
  rend<-periodReturn(precios, period="daily",  type='arithmetic')
  rend<-na.omit(rend)
  #--------Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, rend, envir = .GlobalEnv)
}
rend<-function(simbolo) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo,  from=start, to= end, auto.assign = TRUE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,6]
  #--------Rendimientos simples:
  rend<-periodReturn(datos, period = "daily", type='arithmetic')                         
  #------ --Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, datos, envir = .GlobalEnv)
}
#--------Llamar la funcion para cada activo particular:
rend("JPY=X")
str(`JPY=X`)
head(`JPY=X`)
rend("SPY")
rend("AAPL")
rend("GOOGL")
head(AAPL)
#Paso 2. Combinar datos de rendimientos en un solo objeto
ret.data<-cbind(AAPL,IBM,AMZN)
ret.data[c(1:3,nrow(ret.data)),]
names(ret.data) = paste(c("AAPL.Ret","IBM.Ret","AMZN.Ret"))
head(ret.data)
tail(ret.data)

#Paso 2. Combinar datos de rendimientos en un solo objeto
ret.data<-cbind(`JPY=X`,SPY,AAPL,GOOGL)%>%na.omit()
ret.data[c(1:4,nrow(ret.data)),]%>% na.omit()

names(ret.data) = paste(c("JPY",  "SPY", "AAPL", "GOOGL"))
head(ret.data)
tail(ret.data)

##Paso 2: PRecios
precios<-function(simbolo) {
  ##---------Obtener precios de yahoo finance:
  precios<-getSymbols(simbolo, src = "yahoo", from= start, to= end, auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  precios<-na.omit(precios)
  ##--------Mantener el precio de interis:
  precios<-precios[,6]
  #--------Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo,precios, envir = .GlobalEnv)
}
pc('JPY=X')
pc("SPY")
pc("AAPL")
pc("GOOGL")
precios('JPY=X')
precios("SPY")
precios("AAPL")
precios("GOOGL")

tail=length(precios)
precios = cbind(`JPY=X`,SPY,AAPL,GOOGL) %>%na.omit()
tail(precios)
#Paso 2. Combinar datos de rendimientos en un solo objeto
ret.data<-cbind(`JPY=X`,SPY,AAPL,GOOGL) %>%na.omit()
ret.data[c(1:4,nrow(ret.data)),]
names(ret.data) = paste(c("JPY.Ret","SPY.Ret","AAPL.Ret","GOOGL.Ret"))

head(ret.data)
tail(ret.data)

#Paso 3 Identificar valores finales de precios de cada activo en el portafolio al 1ro dic 2013 (o su fecha mas cercana laboral)
#Suponer que se tiene posiciones para `JPY=X` 'o moneda,SPY,AAPL,GOOGL
##Money invested in each security:
inv.moneda= 25000
inv.SPY=35000
inv.AAPL=23000
inv.GOOGL=17000

last.idx<-c(precios$JPY.X.Adjusted['2020-12-01',]*25000, 
            precios$SPY.Adjusted['2020-12-01',]*35000,
            precios$AAPL.Adjusted['2020-12-01',]*23000,
            precios$GOOGL.Adjusted['2020-12-01',]*23000)
last.idx
port.v = c(110.535*inv.moneda, 420.3325*inv.SPY, 135.7679*inv.AAPL, 
           122.0895*inv.GOOGL)
port.v
port.valor <- sum(port.v)
port.valor
#Weights de cada activo:
ws = port.v/port.valor
ws

# Paso 4: P&L:Necesitamos calcuclar rendimientos del portafolio, aplicando los pesos actuales a los retornos históricos.
#Suponemos que el valor actual de cada activo en el portafolio permanece cte en el horizonde del VaR. Por tanto, a cada rendimiento
#se le aplica el valor actual del activo: 
# P&L= SUMA[Valor activo(i)*Rendimiento(i,t)], donde Valor activo (i) = $invertido(i)*Precio(T) = Valor portafolio*w(i)
# last.id = Valor activo (i)*ws
last.idx = port.valor * ws
last.idx

#Paso 4. Simulamos rendimientos  aplicando ponderaciones actuales del portafolio a los rendimintos historicos, para calcular PnL
portPnL = last.idx[1]*ret.data$JPY.Ret +last.idx[2]*ret.data$SPY.Ret+ last.idx[3]*ret.data$AAPL.Ret + last.idx[4]*ret.data$GOOGL.Ret
head(portPnL)
names(portPnL)<-paste("Port.PnL")
portPnL[c(1:4,nrow(portPnL)),]

#Paso 5: Calcular cuantiles para el 1 y 5% VaR, comando: cuantile. primer argumento es el negativo del PnL 
#el segundo argumento es 1-alfa, anexamos format para leer mejor los datos.
##Notar que a mayor nivel de signficancia, (5% vs 1%), o a menor el nivel de confianza (95% vs 99%), menor el monto del VaR.
VaR01.Historical=quantile(-portPnL$Port.PnL,0.99)
VaR01.Historical<-format(VaR01.Historical,big.mark = ',')
VaR01.Historical
VaR05.Historical=quantile(-portPnL$Port.PnL,0.95)
VaR05.Historical<-format(VaR05.Historical,big.mark = ',')
VaR05.Historical

#Graficar el VaR en relacion a la densidad de PnL
ret.d=density(portPnL$Port.PnL)
ret.d

plot(ret.d,xlab="Profit & Loss, PnL",ylab="",yaxt="n",
     main="Densidad del Portafolio Simulado PnL en 3y ; VaR al 1% y 5%, 
     historico - 1 dma")
abline(v=-quantile(-portPnL$Port.PnL,0.99),col="gray",lty=1)
abline(v=-quantile(-portPnL$Port.PnL,0.95),col="black",lty=2)

x<-seq(min(portPnL$Port.PnL),max(portPnL$Port.PnL),lenght=100000)
head(x)
tail(x)
y<-dnorm(x,mean=mean(portPnL$Port.PnL),sd=sd(portPnL$Port.PnL))
head(y)
tail(y)
lines(x,y,type='l',col="black",lwd=1,lty=3)
legend("topright",c("Simulated P&L Distribution o Perdidas&Ganancias","Normal Distribution","1% 1-Day VaR", "5% 1-Day VaR"),col=c("black","black","gray","black"),lty=c(1,3,1,2))

##Si se desea calcular el VaR a diferentes horizontes, se multiplica por la raiz de t (u horizonte).
#Ejemplo, el Var al 99% a 10 dmas:
VaR01.10dias = quantile(-portPnL, 0.99)*sqrt(10)
VaR01.10dias


########Expected Shortfall Historico
#Paso 1: Identificacisn del lmmite del VaR histsrico
VaR01.hist=-quantile(-portPnL$Port.PnL,0.99)
VaR01.hist
VaR05.hist=-quantile(-portPnL$Port.PnL,0.95)
VaR05.hist
#Paso 2:Identificar el portafolio de perdidas  en exceso del VaR, generandoles dummies
ES.PnL<-portPnL$Port.PnL
ES.PnL[c(1:3,nrow(ES.PnL)),]
ES.PnL$dummy01<-ifelse(ES.PnL$Port.PnL<VaR01.hist,1,0)
ES.PnL$dummy05<-ifelse(ES.PnL$Port.PnL<VaR05.hist,1,0)
ES.PnL[c(1:100,nrow(ES.PnL)),]
tail(ES.PnL)
#Paso3 Exraccion portafolio de perdidas en exceso del VaR
shortfall01<-subset(ES.PnL,ES.PnL$dummy01==1)
head(shortfall01)
shortfall05<-subset(ES.PnL,ES.PnL$dummy05==1)
head(shortfall05)
#Calcular el promedio de pirdidas en exceso del VaR
avg.ES01<--mean(shortfall01$Port.PnL)
avg.ES01
ES01.Historical<-format(avg.ES01,big.mark = ',')
ES01.Historical

avg.ES05<--mean(shortfall05$Port.PnL)
avg.ES05
ES05.Historical<-format(avg.ES05,big.mark = ',')
ES05.Historical

#COMPARACION VaR y ES
VaR.ES.Combined<-data.frame(rbind(cbind(VaR01.Historical,ES01.Historical[1]),cbind(VaR05.Historical,ES05.Historical[1])))
#.ES.Combined<-data.frame(rbind(cbind(VaR01.Historical,ES01.Historical[1]),cbind(VaR05.Historical,ES05.Historical[1])))

VaR.ES.Combined
names(VaR.ES.Combined)<-paste(c("VaR Historical","ES Historical"))
rownames(VaR.ES.Combined)<-paste(c("1% 1-Day","5% 1-Day"))
VaR.ES.Combined


#######################################################################################################
##########VaR Gaussiano
head(ret.data)
cov.ret=cov(ret.data)
cov.ret
ws
sigma= (t(ws)%*%cov.ret)%*%ws
sigma #desv estandar
#P2: Calcular media histsrica y desv estandar histsrica de los rendimientos
port.mean<-mean(ret.data)
port.mean
port.risk<-sigma
port.risk
#P3: calclar el vaR al 1% y 5%, donde 1210000 es la inversión total en el portafolio.
VaR01.Gaussian<--(port.mean+port.risk*qnorm(0.01))*1210000
VaR01.Gaussian<-format(VaR01.Gaussian,big.mark = ',')
VaR01.Gaussian
VaR05.Gaussian<--(port.mean+port.risk*qnorm(0.05))*1210000
VaR05.Gaussian<-format(VaR05.Gaussian,big.mark = ',')
VaR05.Gaussian

### EXPECTED SHORTFALL##################
#Se calcula el ES Expected Shortfall gaussiano, asumiendo dist normal, con dato del valor del portafolio previo calculado.
ES01.Gaussian<-1210000*(port.mean+port.risk*(dnorm(qnorm(.01))/0.01))
ES01.Gaussian<-format(ES01.Gaussian,big.mark = ',')
ES01.Gaussian

ES05.Gaussian<-1210000*(port.mean+port.risk*(dnorm(qnorm(.05))/0.05))
ES05.Gaussian<-format(ES05.Gaussian,big.mark = ',')
ES05.Gaussian

##Implica que hay el 1%(o el 5%) de chances que nuestra perdida exceda el VaR, pero cuando pasa, 
#esperamos, en promedio $56,000 ($43000), dependiendo del nivel de confianza. 


#COMPARACION VaR e ES
VaR.ES.Combined<-data.frame(rbind(cbind(VaR01.Historical,ES01.Historical[1],VaR01.Gaussian,ES01.Gaussian[1]),cbind(VaR05.Historical,ES05.Historical[1],VaR05.Gaussian,ES05.Gaussian[1])))
VaR.ES.Combined
names(VaR.ES.Combined)<-paste(c("VaR Historical","ES Historical","VaR Gaussian","ES Gaussian"))
rownames(VaR.ES.Combined)<-paste(c("1% 1-Day","5% 1-Day"))
VaR.ES.Combined

