###  seasonal models
library(forecast)
install.packages("Metrics")
library(Metrics)
library(fUnitRoots)
library(tseries)
###
setwd("C:/Users/Home/Documents/Universidad de los Andes/2022-II/Econometr??a Financiera IIND 4415/S6")
da=read.table("coke-earns.txt",header=T)
head(da)
rend=da$value #used to model building (Auto.Arima, AIRline & proposed ones) 
plot(rend, type='l')
log_rend=log(rend)

year=c(1:107)/4+1983 # crea indices para graficar tiempo (trimestrales)
log_rend.ts=ts(log_rend,frequency=4,start=c(1982,1)) #Asigna fechas a serie acorde columna "pends" (trimestral), haciendo objeto ts time series.
plot(year, rend, type='l')
plot(year, log_rend.ts,type='l')

##Analisis
acf(log_rend)
dlog_rend=diff(log_rend)
acf(dlog_rend)  ##Se ve el patron estacional en MA
pacf(dlog_rend)
## Diferenciación estacional
slog_rend=diff(log_rend,4) #Diferencia estacional log_rend
par(mfcol=c(1,2))
par(mfcol=c(1,3))
plot(slog_rend, type='l')
acf(slog_rend,lag=20)
pacf(slog_rend, lag=20)
library(tseries)
adf.test(slog_rend) #no es estacionaria (en dif estacional)

#Diferenciación simple
dslog_rend=diff(slog_rend) #Diferencia simple a la diferenciada estacional
par(mfcol=c(3,1))
plot(dslog_rend)
acf(dslog_rend)
pacf(dslog_rend)
adf.test(dslog_rend) # es estacionaria 


## Gráficos series Resumen:
par(mfcol=c(3,2))
plot(slog_rend, type='l')
acf(slog_rend,lag=20)
pacf(slog_rend,lag=20)
plot(dslog_rend, type='l')
acf(dslog_rend, lag=20)
pacf(dslog_rend, lag=20)

###############################################################
##Construcción Modelo 
##Observando las ACF y la PACF, vamos a proponer un modelo diferente al airline; se propone modelos iniciales parte estacional y no estacional
#Candidatos: de la ACF: MA(2) y AR(3) estacional (ds)
# De la ACF: MA(3) o MA(5) no estacional (s).
##Con autoarima:
rend.ts=ts(rend, frequency=4,start=c(1982,1))
m0=auto.arima(rend.ts, lambda="auto", seasonal = TRUE) #upon seasonal data
summary(m0)
#Procedemos con candidatos de las gráficas
#In this case ARMA(1,1) for diff no seasonal (s), ARMA(2,1) for diff seasonal (s_logrend)
mod1=Arima(rend,order=c(1,1,1),seasonal=list(order=c(2,1,1),period=4),lambda="auto", method = "ML")
mod1
library(lmtest)
coeftest(mod1)
tsdiag(mod1,gof=20)
##Modelo AIrline:  # El que mejor AIC arroja
mod3=Arima(rend, order = c(0,1,1), seasonal=list(order=c(0,1,1), period=4,lambda="auto", method="ML"))
mod3

#####################################################################Particionando Modelo##########################
#Partiendo la serie.
da=read.table("coke-earns.txt",header=T)
rend<-da$value
length(rend)
rendts<-ts(rend, start=c(1985,1,1), frequency = 4)
rendts_train=rendts[1:100] #dejamos 7 datos para la prueba
rendts_test<-rendts[101:107]

## Modelos:
#se toma el mejor modelo de ls anteriores evaluados sobre la serie original
#para ese caso el AIRline fue el de mejor desempe??o, por tanto se usa ese orden
#se aplica adem??s sobre la parte de entrenamiento
m11=Arima(rendts_train,order=c(0,1,1),seasonal=list(order=c(0,1,1),lambda="auto", period=4))
m11
tsdiag(m11)
#autorarima sobre la parte de entrenamiento
m22=auto.arima(rendts_train, seasonal=TRUE, lambda = "auto")
m22
tsdiag(m22)

#Prediccion a 7 trimestres adelante
futurValm11 <- forecast(m11,h=7)
plot(futurValm11)

futurValm22 <- forecast(m22,h=7)
plot(futurValm22)


library(Metrics)
rmse(rendts_test, futurValm11$mean)
rmse(rendts_test, futurValm22$mean) #autoarima sobre test fue el de menor error

## Con paquete de ASTSA (Shumway)
library(astsa)
mod_shum= sarima(rendts, 0,1,1,0,1,1,4)
mod_shum

pron3=sarima.for(rendts, 10,0,1,1,0,1,1,4 ) #El primer término es el h de pronóstico, lo demás, el modelo sarima.
pron3$pred



##############################################################################################
###################Ejemplo 2 Datos trimestrales Ejercicio rápido
x=read.csv("PErrin_freres.csv", sep=';', header=TRUE) %>% na.omit()
head(x)
colnames(x)=c("Month", "Sales")
sales=x$Sales
sales.ts=ts(sales, start = c(1964,01), frequency = 12)
plot(sales.ts)
library(fUnitRoots)
adfTest(sales.ts, lags = 12)
acf(sales, lag=70)
pacf(sales, lag=70)
dsales=diff(sales) #no es estacionaria
acf(dsales, lag=70) #MA(0)
pacf(dsales, lag=50) #AR(2)

##Diferenciación Seasonal:
ssales=diff(dsales,12) #Diferencia estacional rendimientos trimestrales simples
adfTest(ssales, lags = 12) #ya es estacionaria
par(mfrow=c(2,1))
acf(ssales, 36) #MA(1)
pacf(ssales,36) #AR(2)

ms1=auto.arima(sales.ts, seasonal = TRUE) #upon already seasonal ts
ms1
#Now, we follow with AIRline model upon original series without ts
ms2=Arima(sales, order = c(0,1,1),seasonal =list(order=c(0,1,1), period=12,lambda="auto", method="ML"))
ms2
#AIRline got the best fit on this case compared to Autoarima
# autoplot(ms2) 
# this plot is not needed on this analysis
###Partiendo serie
library(TSstudio)
l<-length(sales)
l
sales=x$Sales
sales.ts=ts(sales, start = c(1964,01), frequency = 12)
#split= ts_split(sales.ts, sample.out = 12)
#train=split$train
#test=split$test
#mismo proceso:
rendts<-ts(sales, start = c(1964,01), frequency = 12)
rendts_train=rendts[1:(l-12)] #dejamos 12 datos para la prueba
rendts_test<-rendts[(l-12+1):l]
rendts_test #we check array data
#Regresamos serie original y diferenciamos:
adfTest(sales.ts, lags = 12)
dsales=diff(sales) #no es estacionaria
acf(dsales, lag=70) #MA(0), lags 2 and 4 (marginal) with 8 and 12 significative
pacf(dsales, lag=50) #AR(2)

#Modelos:
#we use the fittest model from previous analysis (AIRLine) for the order upon trained data split
ms1=Arima(rendts_train, order = c(0,1,1),seasonal =list(order=c(0,1,1), period=4,lambda="auto", method="ML"))
ms1
fms1=forecast(ms1, 12)
ms2=auto.arima(rendts_train, seasonal = TRUE)
ms2
fms2=forecast(ms2, 12)
##Desempeño pronóstico:
rmse(rendts_test, fms1$mean)
rmse(rendts_test, fms2$mean) #sobreespecificado
#Pronostico con el 1ro.
#quarterly seasonality
ms_all=Arima(sales.ts, order = c(0,1,1),seasonal =list(order=c(0,1,1), period=12,lambda="auto", method="ML"))
ms_all
pron_all=forecast(ms_all, 12)
autoplot(pron_all)

