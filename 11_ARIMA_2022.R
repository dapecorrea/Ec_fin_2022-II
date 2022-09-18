###ARIMA
# Bajar paqueter???as
library(fUnitRoots)
library(forecast)  #est??? Arima()
install.packages("Metrics")
library(FitAR)
install.packages('forecast', dependencies = TRUE)
library(tseries)


library(Metrics)
##Datos:
library(readr)
#da=read_csv("Universidad de los Andes/2022-II/Econometr??a Financiera IIND 4415/S6/gdp_2019.txt")
da=read.table("Universidad de los Andes/2022-II/Econometr??a Financiera IIND 4415/S6/gdp_2019.txt", header=T)
head(da)
gdp=da$gdp
gdp
str(gdp)
loggdp=log(gdp)
diflog_gdp=diff(loggdp)
library(tseries)
adf.test(diflog_gdp, k=10)
dif2log_gdp=diff(diflog_gdp)
adf.test(dif2log_gdp, k=10) #ya queda estacionaria en la segunda diferenciaci??n

##Para graficar, datos parten de 1947, datos trimestrales, son 248 datos
tdx=c(1:248)/4+1947 # crea indices para graficar tiempo (trimestrales)
par(mfcol=c(2,2))
plot(tdx,gdp,xlab = 'year',ylab='GNP_Value',type='l')
plot(tdx,loggdp,type='l',xlab='year',ylab='log_GDP')
plot(tdx[2:248],diflog_gdp,type='l',xlab='year',ylab='1ra Dif_log_GDP')  #Como se elimina un dato en la diferenciaci???n, se ajsuta el eje x.
plot(tdx[3:248],dif2log_gdp,type='l',xlab='year',ylab='2da Dif_log_GDP')
##
par(mfrow=c(3,4))
plot(tdx,gdp,xlab='year',ylab='GNP_Value',type='l')
plot(tdx,loggdp,type='l',xlab='year',ylab='log_GDP')
plot(tdx[2:248],diflog_gdp,type='l',xlab='year',ylab='1ra Dif_log_GDP') #Com se elimina un dato en la diferenciaci???n, se ajsuta el eje x
plot(tdx[3:248],dif2log_gdp,type='l',xlab='year',ylab='2da Dif_log_GDP')
acf(gdp)
acf(loggdp)
acf(diflog_gdp)
acf(dif2log_gdp)
pacf(gdp)
pacf(loggdp)
pacf(diflog_gdp)
pacf(dif2log_gdp)
##
#
adf.test(gdp,k=10)
adf.test(loggdp,k=10)
adf.test(diflog_gdp,k=10)
adf.test(dif2log_gdp,k=10)
###
##Vemos que la serie ya estacionaria, ser???a la doble diferenciada.
## La acf corta en el 3er lag, la pacf corta en el 1ro y luego en el 8vo de manera restrictiva. 
#Viendo el EACF, quedar??an arima(0,2,1) y arima(8,2,4):
#install.packages('TSA')
library(TSA)
plot(eacf(dif2log_gdp, 10,10))
meacf<-eacf(dif2log_gdp, 10,10)
##MOdelos candidatos: arima(2,2,2), arima(8,2,4) y otros: arima(0,2,5)
############################################
## Vamos a modelar estos candidatos:
library(lmtest)
arima_mod1<-Arima(gdp, order=c(2,2,2), method="ML")
arima_mod1
coeftest(arima_mod1) 

arima_mod2<-Arima(gdp, order=c(8,2,4), fixed=c(NA,0,NA,0,NA,NA,0,NA,NA,NA,NA,NA), transform.pars = FALSE, method="ML")
arima_mod2
coeftest(arima_mod2)

arima_mod3<-Arima(gdp, order=c(0,2,5), method="ML", lambda=TRUE)
arima_mod3
coeftest(arima_mod3)
####
####Se elige el mejor modelo, en este caso Arima(8,2,4).
##Suponiendo que el 2do es el mejor:
library(FitAR)
#install.packages('forecast', dependencies = TRUE)
library(tseries)
library(forecast)

Pron_m2<-predict(arima_mod2, n.ahead = 10) #pron??stico modelo 2
par(mfrow=c(1,1))
plot(Pron_m2$pred)
##Con librer???a forecast()
plot(forecast(arima_mod2))



######Con auto.arima:
m1=auto.arima(gdp, seasonal = TRUE)
summary(m1)

futurVal <- forecast(m1,h=10)
plot(futurVal)


#############################################################################################
#?#####Partiendo el modelo para un ejercicio de pron???stico
length(gdp)
h=12
train=gdp[1:(length(gdp)-h)]
test=gdp[(length(gdp)-h+1):length(gdp)]
#train = gdp[1:174]
#test = gdp[175:248]

estacio<-diff(diff(log(train)))
###Modelo test con autoarima
mm11=auto.arima(train)
summary(mm11)
tsdiag(mm11)

#training ?modelo con EACF
otro<-eacf(estacio, 10,10)
##Ser??a un ARMA(2,2)
mm2<-Arima(train,order=c(2,2,2), method="ML" )
summary(mm2)
# forecasting
forecast.mm1 = predict(mm11,74)
forecast.mm2 = predict(mm2,74)

# evaluacion, con respecto a la muestra separada de valida.
library(Metrics)
rmse(test, forecast.mm1$pred)
rmse(test, forecast.mm2$pred)

##Predicciones: Elegimos el mejor modelo.
predict(mm2,n.ahead = 12)
futurVal_2 <- forecast(mm2,h=12, level=c(99.5))
plot(futurVal_2)
