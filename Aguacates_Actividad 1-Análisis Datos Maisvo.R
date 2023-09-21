

aguacate <- avocado_updated_2020 

### 1) Tipo de variables que componen mi base de datos ###

# Realizamos un análisis exploratorio de las variables numéricas 

# Inspeccionamos la estructura de los datos, para ver los tipos de variables (chr, num o date)
str(aguacate)

# Se realiza un resumen de los datos (media, min, max, celdas vacias, ...)
summary(aguacate)

# Calculamos la varianza y la dimension

var(aguacate)

dim(aguacate)

# Gráfico de caja y bigotes del precio
boxplot(aguacate)

boxplot(aguacate$average_price)

boxplot(aguacate$average_price~aguacate$type)

# Histograma
hist(aguacate$average_price)


### 2) Extracción del precio de venta (la variable) de los aguacates orgánicos vendidos en Albany y de Boston.

# En primer lugar extraemos las áreas de Albany y Boston
albany <- aguacate[aguacate$geography=="Albany",]
boston <- aguacate[aguacate$geography=="Boston",]

# Una vez extraídas las áreas geográficas procedemos a calcular la media de precios para los aguacates ORGÁNICOS (type organic)
# Tendremos que volver a filtrar para quedarnos solo los de tipo orgánico (organic)

albany_organico <- albany[albany$type=="organic",]
boston_organico <- boston[boston$type=="organic",]

# Ya podemos sacar la media del precio de venta en cada área para el tipo orgánico

mean(albany_organico$average_price)
mean(boston_organico$average_price)

### 3) Cálculo la covarianza y la matriz de correlación  del precio de los aguacates orgánicos, convencionales y su volumen de ventas

# Extraemos por tipo de aguacate para toda la base datos y NO por zona geográfica

aguacate_organico <- aguacate[aguacate$type=="organic",]
aguacate_convencional <- aguacate[aguacate$type=="conventional",]

# Analizamos la relación entre average price y volume total según el tipo de aguacate

cov(aguacate_organico$average_price, aguacate_organico$total_volume)
cov(aguacate_convencional$average_price, aguacate_convencional$total_volume)

# Matriz de correlación

cor(aguacate_organico$average_price, aguacate_organico$total_volume)
cor(aguacate_convencional$average_price, aguacate_convencional$total_volume)

plot(aguacate_organico$average_price, aguacate_organico$total_volume)
plot(aguacate_convencional$average_price, aguacate_convencional$total_volume)

### 4)Determinar  relación existente entre dichos precios y su volumen de ventas pero con LOGARITMOS

# Para ello realizamos un análisis de regresión por niveles (variable dependiente e independiente)

lm(aguacate_organico$average_price~aguacate_organico$total_volume)
lm(aguacate_convencional$average_price~aguacate_convencional$total_volume)

# Ahora hacemos la Regresión por LOGARTIMOS

lm(log(aguacate_organico$average_price)~log(aguacate_organico$total_volume))
lm(log(aguacate_convencional$average_price)~log(aguacate_convencional$total_volume))


### 5) Predicción del precio de venta de los aguacates orgánicos vendidos en Albany a 3 meses. 

# Instalamos paquete/libreria para hacer predicciones "forecast"

install.packages("forecast")

# Una vez instalados llamamos al paquete/libreria

library(forecast)

# Ya podemos utilizar el paquete descargado

# Antes de la predicción es necesario darle un formato de SERIE TEMPORAL
# Extraemos de Albany el precio de los aguacates organicos 

albany_organicos_precio <- albany_organico$average_price

# Ahora podemos darle formato de serie temporal
albany_organicos_precio_st <- ts(albany_organico$average_price, start = c(2015,1), end = c(2020,11),frequency = 52,143)

# Dibujamos la serie temporal
plot(albany_organicos_precio_st)

# Para reducir ruido descomponemos la serie temporal
decompose(albany_organicos_precio_st)

# Dibujamos la serie temporal descompuesta
plot(decompose(albany_organicos_precio_st))

# Guardamos los componenetes
componentes_albany_organicos_precio_st <- decompose(albany_organicos_precio_st)

# Sacamos solo el componente tendencia (trend) 
tendencia_albany_organicos_precio_st <- componentes_albany_organicos_precio_st$trend
plot(tendencia_albany_organicos_precio_st)


###PREDICCION ARIMA, comando --> auto.arima (sirve para predecir)

modelo_albany_organicos <- auto.arima(albany_organicos_precio_st)

prediccion_albany_organicos <- forecast(modelo_albany_organicos,13,035)

plot(prediccion_albany_organicos)

# Mucho ruido, no se aprecia la predicción a 3 meses, procedemos a utilizar la tendencia para eliminar ruido

modelo_tendencia_albany_organicos <- auto.arima(tendencia_albany_organicos_precio_st)

# Ya podemos hacer la predicción para la tendencia

prediccion_tendencia_albany_organicos <- forecast(modelo_tendencia_albany_organicos,13,035)

plot(prediccion_tendencia_albany_organicos)


###############################################

# CONTRASTE DE HIPOTESIS

lm(log(aguacate_organico$average_price)~log(aguacate_organico$total_volume))


estimacionorganico<-lm(log(aguacate_organico$average_price)~log(aguacate_organico$total_volume))
summary(estimacionorganico)


lm(log(aguacate_convencional$average_price)~log(aguacate_convencional$total_volume))
estimacionconvencional<- lm(log(aguacate_convencional$average_price)~log(aguacate_convencional$total_volume))


summary(estimacionconvencional)



