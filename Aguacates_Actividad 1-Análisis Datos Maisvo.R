### ACTIVIDAD GRUPAL 1 ###

aguacate <- avocado_updated_2020 

### 1) tipo de variables que componen mi base de datos ###

#Realizamos un análisis exploratorio de las variables numéricas 

#Ispeccionamos la estructura de los datos, para ver los tipos de variables (chr, num o date)
str(aguacate)

#se realiza un resumen de los datos (media, min, max, celdas vacias, ...)
summary(aguacate)

#Calculamos la varianza y la dimension

var(aguacate)

dim(aguacate)

#CUadre de caja del precio
boxplot(aguacate)

boxplot(aguacate$average_price)

boxplot(aguacate$average_price~aguacate$type)

#histograma
hist(aguacate$average_price)


### 2) Extraccion del precio de venta (la variable) 
#de los aguacates orgánicos vendidos en Albany y de Boston.

#En primer lugar extraemos las areas de Albany y Boston
albany <- aguacate[aguacate$geography=="Albany",]
boston <- aguacate[aguacate$geography=="Boston",]

#Una vez extraídas las áreas geográficas procedemos a calcular 
#la media de precios para los aguacates ORGÁNICOS (type organic)
#Tendremos que volver a filtrar para quedarnos solo los de tipo orgánico (organic)

albany_organico <- albany[albany$type=="organic",]
boston_organico <- boston[boston$type=="organic",]

#Ya podemos sacar la media del precio de venta en cada area para el tipo organico

mean(albany_organico$average_price)
mean(boston_organico$average_price)

### 3)calcule la covarianza y la matriz de correlación  
# del precio de los aguacates orgánicos, convencionales y su volumen de ventas

#extraemos por tipo de aguacate para toda la base datos y NO por zona geográfica

aguacate_organico <- aguacate[aguacate$type=="organic",]
aguacate_convencional <- aguacate[aguacate$type=="conventional",]

#Analizamos la realcion entre average pric y volume total según el tipo de agucate

cov(aguacate_organico$average_price, aguacate_organico$total_volume)
cov(aguacate_convencional$average_price, aguacate_convencional$total_volume)

# Matriz de correlacion

cor(aguacate_organico$average_price, aguacate_organico$total_volume)
cor(aguacate_convencional$average_price, aguacate_convencional$total_volume)

plot(aguacate_organico$average_price, aguacate_organico$total_volume)
plot(aguacate_convencional$average_price, aguacate_convencional$total_volume)

### 4)Determinar  relación existente entre dichos precios y su volumen de ventas
### pero con LOGARITMOS

#Para ello realizamo un análisi de regresion por niveles (varaible dependiente e indpendiente)

lm(aguacate_organico$average_price~aguacate_organico$total_volume)
lm(aguacate_convencional$average_price~aguacate_convencional$total_volume)

#Ahora hacemos la Regresiion por LOGARTIMOS

lm(log(aguacate_organico$average_price)~log(aguacate_organico$total_volume))
lm(log(aguacate_convencional$average_price)~log(aguacate_convencional$total_volume))


### 5)Predicciónde precio de ventade los aguacates orgánicos vendidos en Albany a 3 meses. 

#Instalamos paquete/libreria para hacer predicciones "forecast"

install.packages("forecast")

#una vez isntalados llamamos al paquete/libreria

library(forecast)

#ya podemos utilizar el paquete descargado

#PERO antes de la prediccion es necesario darle un formato de SERIE TEMPORAL
#Extraemos de albany el precio de los aguacates organicos 

albany_organicos_precio <- albany_organico$average_price

#ahora podemos darle formato de serie temporal
albany_organicos_precio_st <- ts(albany_organico$average_price, start = c(2015,1), end = c(2020,11),frequency = 52,143)

#dibujamos la serie temporal
plot(albany_organicos_precio_st)

#para reducir ruido descomponemos la serie temporal
decompose(albany_organicos_precio_st)

#dibujamos serie temporal descompuesta
plot(decompose(albany_organicos_precio_st))

#guardamos los componenetes
componentes_albany_organicos_precio_st <- decompose(albany_organicos_precio_st)

#sacamos solo el componente tendencia (trend) 
tendencia_albany_organicos_precio_st <- componentes_albany_organicos_precio_st$trend
plot(tendencia_albany_organicos_precio_st)


###PREDICCION ARIMA, comando --> auto.arima (sirve para predecir)

modelo_albany_organicos <- auto.arima(albany_organicos_precio_st)

prediccion_albany_organicos <- forecast(modelo_albany_organicos,13,035)

plot(prediccion_albany_organicos)

#Mucho ruido, no se aprecia la prediccion a 3 meses
#procedemos a utilizar la tendencia para eliminar ruido

modelo_tendencia_albany_organicos <- auto.arima(tendencia_albany_organicos_precio_st)

#ya podemos hacer la prediccion para la tendencia

prediccion_tendencia_albany_organicos <- forecast(modelo_tendencia_albany_organicos,13,035)

plot(prediccion_tendencia_albany_organicos)


###############################################

#CONTRASTE DE HIPOTESIS

lm(log(aguacate_organico$average_price)~log(aguacate_organico$total_volume))


estimacionorganico<-lm(log(aguacate_organico$average_price)~log(aguacate_organico$total_volume))
summary(estimacionorganico)


lm(log(aguacate_convencional$average_price)~log(aguacate_convencional$total_volume))
estimacionconvencional<- lm(log(aguacate_convencional$average_price)~log(aguacate_convencional$total_volume))


summary(estimacionconvencional)


### FINAL ###

################################################

#PRUEBAS

install.packages("dplyr")

library("dplyr")

Datos%>% select (-date)

aguacate_1 <- aguacate%>% select(-date, -type, -geography, -year, -small_bags, -xlarge_bags, - large_bags, - `4046`, - `4225`, - `4770`, - total_bags)

boxplot(aguacate_1)

boxplot(aguacate$average_price)

boxplot(aguacate$average_price~aguacate$type)
        