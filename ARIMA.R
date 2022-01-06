library(ggplot2)
library(forecast)
library(tseries)
library(readxl)
library(dplyr)
library(tidyverse) 
library(hrbrthemes)
library(stargazer)

## Cargar Datos
Datos <- read_excel("C:/Users/Enrique Nieto/Desktop/Precios GasTexas/DATOS.xlsx", 
                      col_types = c("date", "numeric", "numeric"))

## Cambiar nombres columnas 

names<-c("Fecha","HSC","Waha")
colnames(Datos)<-names

## Visualización preliminar

DatosLong<-Datos %>% gather("Tipo","USD/MMBTU" ,-Fecha)

DatosLong$Fecha<- as.Date(DatosLong$Fecha)

ggplot(DatosLong, aes(Fecha, `USD/MMBTU`, col=Tipo)) + geom_line(size=1.2) + theme_ipsum() + labs(title = "Precio spot Waha y Houston Ship Channel 2000-2021*", caption = "*Junio de 2020 a marzo de 2021")   + scale_x_date(date_labels = "%m/%Y", date_breaks = "3 years")

#### Empezar con Waha

#Removiendo outliers con tsclean

count_ts = ts(Datos[, c('Waha')])

Datos$Waha = tsclean(count_ts)

ggplot() + 
  geom_line(data = Waha, aes(x = Fecha, y = Waha), col="darkred", size=1) + ylab('$USD/MMBTU') + labs(title="Waha sin outliers") + theme_ipsum() 

## Desastacionalización 

count_maWaha = ts(na.omit(Datos$Waha), frequency=30) 

decomp = stl(count_maWaha, s.window="periodic")

deseasonal_Waha <- seasadj(decomp,  allow.multiplicative.trend=TRUE)

plot(decomp, main="Tendencia y estacionalidad Waha spot por mes (01/01/2000-01/03/2021)") 

## Augementend Dicky-Fuller TESadf.test(count_ma, alternative = "stationary")

adfWaha<-adf.test(count_maWaha, alternative = "stationary")

adfWaha

## Creando diferenciación Diff

difWaha1<-diff(deseasonal_Waha, differences = 1)

plot(difWaha1)

adfWahaS<-adf.test(difWaha1, alternative = "stationary")

adfWahaS ## DIferenciación a (d) 1

## AR y Ma
difWaha1<-diff(deseasonal_Waha, differences = 12)

Pacf(difWaha1, main='PACF Waha') ## Diferenciación (q) Será de uno

Acf(difWaha1, main='ACF Waha') #Autoregresivo (p), lo hace bien arriba de dos

## Auto ARIMA (5,1,0)

auto.arima(Datos$Waha, seasonal = FALSE)

## Aikaike Information Criteria y  Baysian information criteria (BIC)

# 1) AIC -2753 , BIC=-2712

arima(Datos$Waha,order = c(7,1,0))

#ARIMA (5,1,0) Muestra mejores que resultados que varios configuraciones

##Forecast

WahaArima<-auto.arima(Datos$Waha)

WahaArima

WahaFor<-forecast(WahaArima)

##Acercamiento

plot(WahaFor, xlim=c(7700,7750), main="Pronóstico ARIMA Precio Spot Waha", submain="Observaciones de 2020 a 2021", ylab = "$/MMBTU", xlab="Fecha", sub="*Muestra de datos en gráfico de 2020 a 2021")

##  ###






#### El turno del Houston ShipChannel 

#Removiendo outliers con tsclean

count_ts = ts(Datos[, c('HSC')])

Datos$HSC = tsclean(count_ts)

ggplot() + 
  geom_line(data = Datos, aes(x = Fecha, y = HSC), col="steelblue", size=1.1) + ylab('$USD/MMBTU') + labs(title="Houston Ship Channel") + theme_ipsum() 

## Desastacionalización 

count_maHSC = ts(na.omit(Datos$HSC), frequency=30) 

decompHSC = stl(count_maHSC, s.window="periodic")

deseasonal_HSC <- seasadj(decomp,  allow.multiplicative.trend=TRUE)

plot(decomp, main="Tendencia y estacionalidad HSC spot por mes (01/01/2000-01/03/2021)") 

## Augementend Dicky-Fuller TESadf.test(count_ma, alternative = "stationary")

adfHSC<-adf.test(count_maHSC, alternative = "stationary")

adfHSC

## Creando diferenciación Diff

difHSC1<-diff(deseasonal_HSC, differences = 1)

plot(difHSC1)

adfHSCs<-adf.test(difHSC, alternative = "stationary")

adfHSCs ## DIferenciación a (d) 1

## AR y Ma
#difWaha1<-diff(deseasonal_Waha, differences = 12)

Pacf(difHSC1, main='PACF Waha') ## Diferenciación (q) Será de uno

Acf(difHSC1, main='ACF Waha') #Autoregresivo (p), lo hace bien arriba de dos

## Auto ARIMA (5,1,0)

auto.arima(Datos$HSC, seasonal = FALSE)

## Aikaike Information Criteria y  Baysian information criteria (BIC)

# 1) AIC -2753 , BIC=-2712

arima(Datos$HSC,order = c(7,1,0))

#ARIMA (5,1,0) Muestra mejores que resultados que varios configuraciones

##Forecast

HSCArima<-auto.arima(Datos$HSC)

HSCFor<-forecast(HSCArima)


##Acercamiento

plot(HSCFor, xlim=c(7700,7750), main="Pronóstico ARIMA Precio Spot HSC", submain="Observaciones de 2020 a 2021", ylab = "$/MMBTU", xlab="Fecha", sub="*Muestra de datos en gráfico de 2020 a 2021")

##  ###
