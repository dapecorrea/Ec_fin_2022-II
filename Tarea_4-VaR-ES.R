#VaR Histsrico Expected-Shortfall Comparaciones 
#Parte II
#Punto 1.
#VaR HISTORICO
#Paso 1:Importar serie de rend AAPL,  IBM y AMZ
library(quantmod)
library(xts)

start<-format(as.Date("2019-05-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-01"),"%Y-%m-%d")

#--------- Funcisn para bajar precios y generar rendimientos:
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
#--------Llamar la funcion para cada activo particular:
rend("JPY=X")
str(`JPY=X`)
head(`JPY=X`)
rend("SPY")
rend("AAPL")
rend("GOOGL")
head(AAPL)
#Paso 2. Combinar datos de rendimientos en un solo objeto
ret.data<-cbind(`JPY=X`,SPY,AAPL,GOOGL)
ret.data[c(1:4,nrow(ret.data)),]%>% na.omit()
names(ret.data) = paste(c("JPY",  "SPY", "AAPL", "GOOGL"))
head(ret.data)
tail(ret.data)
ret.data<-na.omit(ret.data)
head(ret.data)

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
precios('JPY=X')
precios("SPY")
precios("AAPL")
precios("GOOGL")

ntail=length(precios)
ntail
precios = cbind(`JPY=X`,SPY,AAPL,GOOGL) %>%na.omit()
precios<-na.omit(precios)
tail(precios)

#Paso 3 Identificar valores finales de precios de cada activo en el portafolio al 1ro dic 2013 (o su fecha mas cercana laboral)
#Suponer que se tiene posiciones para APPL, IBM y AMZN de: 7000, 15000 y	30000 respectivamente
##Money invested in each security:
inv.moneda= 25000
inv.SPY=35000
inv.AAPL=23000
inv.GOOGL=17000
#last.idx<-c(precios$AAPL.Adjusted['2011-12-01',]*7000,precios$IBM.Adjusted['2011-12-01',]*15000,precios$AMZN.Adjusted['2011-12-01',]*30000)
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
#se le aplica el valor actual del activo (ultimo valor): 
# P&L= SUMA[Valor activo(i)*Rendimiento(i,t)], donde Valor activo (i) = $invertido(i)*Precio(T) = Valor portafolio*w(i)
# last.id = Valor activo (i)*ws
last.idx = port.valor * ws
last.idx
head(ret.data)
#Paso 4. Simulamos rendimientos  aplicando ponderaciones actuales del portafolio a los rendimintos historicos, para calcular PnL
portPnL = last.idx[1]*ret.data$JPY +last.idx[2]*ret.data$SPY+ last.idx[3]*ret.data$AAPL + last.idx[4]*ret.data$GOOGL

head(portPnL)
names(portPnL)<-paste("Port.PnL")
portPnL[c(1:3,nrow(portPnL)),]

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

plot(ret.d,xlab="Profit & Loss, PnL",ylab="",yaxt="n",main="Densidad del Portafolio Simulado PnL en 3 años; VaR al 1% y 5%, histsrico - 1 dma")
abline(v=-quantile(-portPnL$Port.PnL,0.99),col="gray",lty=1)
abline(v=-quantile(-portPnL$Port.PnL,0.95),col="black",lty=2)

x<-seq(min(portPnL$Port.PnL),max(portPnL$Port.PnL),lenght=1000)
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
ES.PnL[c(1:5,nrow(ES.PnL)),]
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
#P3: calclar el vaR al 1% y 5%, donde 100000 es la inversión total 
#en el portafolio.
VaR01.Gaussian<--(port.mean+port.risk*qnorm(0.01))*100000
VaR01.Gaussian<-format(VaR01.Gaussian,big.mark = ',')
VaR01.Gaussian
VaR05.Gaussian<--(port.mean+port.risk*qnorm(0.05))*100000
VaR05.Gaussian<-format(VaR05.Gaussian,big.mark = ',')
VaR05.Gaussian

### EXPECTED SHORTFALL##################
#Se calcula el ES Expected Shortfall gaussiano, asumiendo dist normal, con dato del valor del portafolio previo calculado.
ES01.Gaussian<-100000*(port.mean+port.risk*(dnorm(qnorm(.01))/0.01))
ES01.Gaussian<-format(ES01.Gaussian,big.mark = ',')
ES01.Gaussian

ES05.Gaussian<-100000*(port.mean+port.risk*(dnorm(qnorm(.05))/0.05))
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

########
####Punto 2. VaR Test para VaR historico 
library(rugarch)
library(car)
library(FinTS)

num_obs = nrow(portPnL$Port.PnL)
w_e = 1000           #Ventana estimación
w_t = num_obs - w_e  #Ventana de testing
alpha = 0.99

# generamos ciclo sobre la muestra, calcular VaR de estas 3 maneras y almacenar datos
backTest_VaR = function(x, p = 0.99) {
  normal_VaR = as.numeric(VaR(x, p=p, method="gaussian"))
  historical_VaR = as.numeric(VaR(x, p=p, method="historical"))
  modified_VaR = as.numeric(VaR(x, p=p, method="modified"))
  resp = c(normal_VaR, historical_VaR, modified_VaR)
  names(resp) = c("Normal", "Historic", "Modified")
  return(resp)
}

# Estimación, pronóstico VaR: rolling 1-step ahead
?rollapply
VaR_results = rollapply(as.zoo(portPnL$Port.PnL), width=w_e, 
                        FUN = backTest_VaR, p=0.99, by.column = FALSE,
                        align = "right")

VaR_results = lag(VaR_results, k=-1)
chart.TimeSeries(merge(portPnL$Port.PnL, VaR_results), legend.loc="bottomright")

##Vemos cuantas veces supero el VaR, o violaciones a éste, generamos primero el grid:
violations_mat = matrix(0, 3, 5)
rownames(violations_mat) = c("Normal", "Historic", "Modified")
colnames(violations_mat) = c("Exn1", "n1", "1-alpha", "Percent", "VR")  #En:expected number, n1: number of exceedances, Porcentaje y Valor en porcentaje

violations_mat[, "Exn1"] = (1-alpha)*w_t
violations_mat[, "1-alpha"] = 1 - alpha

# Violaciones VaR:
normalVaR_violations = as.zoo(portPnL$Port.PnL[index(VaR_results), ])< VaR_results[, "Normal"]
violation_dates = index(normalVaR_violations[which(normalVaR_violations)])

# plot violations
plot(as.zoo(portPnL$Port.PnL[index(VaR_results),]), col="blue", ylab="Return")
abline(h=0)
lines(VaR_results[, "Historic"], col="black", lwd=2)
lines(as.zoo(AAPL_ret[violation_dates,]), type="p", pch=16, col="red", lwd=2)

for(i in colnames(VaR_results)) {
  VaR_violations = as.zoo(AAPL_ret[index(VaR_results), ]) < VaR_results[, i]
  violations_mat[i, "n1"] = sum(VaR_violations)
  violations_mat[i, "Percent"] = sum(VaR_violations)/w_t
  violations_mat[i, "VR"] = violations_mat[i, "n1"]/violations_mat[i, "Exn1"]
}
violations_mat

# Se emplea rugarch, para VaRTest() function
##Prueba VaR
?VaRTest

VaR_test = VaRTest(1-alpha,actual=coredata(portPnL$Port.PnL[index(VaR_results),]),
                   VaR=coredata(VaR_results[,"Historic"]))
names(VaR_test)
# LR para num correcto de excesos
VaR_test[1:7]
# LR para excesos independientes, Chrisptophensen
VaR_test[8:12]

# Aquí, se debe ver qué modelo de GARCH se aplica a la serie univaraida.
library(fGarch)
mod=auto.arima(AAPL_ret)
summary(mod)
re=mod$residuals
acf(re)  #HAY EFECTO ARCH GARCH, PROBEMOS CON GARCH(1,1)
m4=garchFit(~arma(1,1)+garch(1,1),data=AAPL_ret,include.mean=TRUE, cond.dist= "norm", trace=F)
summary(m4)

resi=residuals(m4,standardize=T) # Standardized residuals
res=ts(resi,frequency=12,start=c(2002,1))
par(mfcol=c(2,1))  # 
plot(res,xlab='year',ylab='resi_estand',type='l') 
par(mfcol=c(2,2)) # para  ACF & PACF
acf(resi,lag=24)
pacf(resi,lag=24)
acf(resi^2,lag=24)
pacf(resi^2,lag=24)
##Modelo captura bien media y volatilidad

#acf(AAPL_ret)
#pacf(AAPL_ret)
#library(TSA)     # carga paquete
#m_arma.ts=eacf(AAPL_ret,5,10)      # genera tabla simplificada

#library(fUnitRoots)
#adfTest(AAPL_ret,lags=10,type=c("c"))
##h0: PRESENCIA RAIZ UNIRTARIA SE RECHAZA,POR TANTO, NO HAY RAIZ UNITARIA

########################## Proceso de Rolling, empleamos paquetería rugarch#
#Se crea primero la especificación previo al ajuste.
#se genera una especificación estándar, GARCH(1,1), se debe hacer antes para la función de rolloing.

garchspec = ugarchspec(variance.model=list(model= "sGARCH", garchOrder=c(1,1)),mean.model=list(armaOrder=c(1,1),
                                                                                               include.mean=TRUE), distribution.model = "norm")


##La función ugarchroll genera pronosticos tipo rolling de modelos arma-garch, posee opción de reajuste cada n periodos
?ugarchroll
#library(parallel)


AAPL_garch_roll = ugarchroll(garchspec, AAPL_ret, n.ahead=1,forecast.length = w_t, refit.every=20, 
                             refit.window="moving", calculate.VaR=TRUE, VaR.alpha=0.01)

class(AAPL_garch_roll)
dev.off()
#plot(AAPL_garch_roll)

#### Gráfica del VaR, la opción 4
plot(AAPL_garch_roll, which=4)

#  Gráfica de los coeficientes modelo:`
plot(AAPL_garch_roll, which=5)

# reporte backtesting 
?report
report_AAPL = report(AAPL_garch_roll, type="VaR")
report(AAPL_garch_roll, type="fpm") ##signature(object = "uGARCHforecast"): Forecast performance measures.


##Reporte de violaciones al modelo VaR GARCH
aa = VaRTest(alpha=0.01, actual=AAPL_garch_roll@forecast$VaR[,2], 
             VaR=AAPL_garch_roll@forecast$VaR[,1])
aa

##Tan tan