# Proyecto final
# Paquetes a ayudar

library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(neuralnet)
library(nnet)
library(rccmisc)

#https://datos.cdmx.gob.mx/dataset/servicios-para-la-poblacion-en-general/resource/59af003e-042e-4aeb-b4f0-8ca9a6600ec4

#Lectura de datos (NOTA: TambiÃ©n hay API, revisar!)
#url_datos <- "https://datos.cdmx.gob.mx/api/3/action/datastore_search?resource_id=59af003e-042e-4aeb-b4f0-8ca9a6600ec4"
#url_datos_sql <- 'https://datos.cdmx.gob.mx/api/3/action/datastore_search_sql?sql=SELECT * from "59af003e-042e-4aeb-b4f0-8ca9a6600ec4"'
#download.file(url = url_datos_sql, destfile = "./nuevos_datos.csv", mode = "wb")
datos <- read.csv("./servicios-para-la-poblacion-en-general.csv")
head(datos)
summary(datos)
datos <- lownames(datos)
names(datos)

# Limpieza de datos
# Valores "" a NA
datos <- datos %>% mutate_if(is.character, list(~na_if(.,""))) 
# Data set sin NA
datos_limpios <- datos[complete.cases(datos[, 4:22]), ]
levels(factor(datos_limpios$servicio))

# Todo el data set sin NA, valores "" (problema porque ya no muetsra los 3 servicios)
# quitar -1 
datos_limpios_2 <- datos %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

# AnÃ¡lisis exploratorio
# Histograma de edad (analizar relaciÃ³n con sexo, escolaridad)
# Investigar la relacion edad, genero y escolaridad
# Hora del dÃ­a en la que es mÃ¡s probable que suceda un caso de violencia
# 
datos %>%
  ggplot() + 
  aes(edad) +
  geom_histogram(binwidth = 5, col="black", fill = "blue") + 
  ggtitle("Histograma de edades por gÃ©nero") +
  ylab("Frecuencia") +
  xlab("Mediciones") + 
  facet_wrap("sexo")
  theme_light()

datos %>%
    ggplot() + 
    aes(edad) +
    geom_histogram(binwidth = 5, col="black", fill = "blue") + 
    ggtitle("Histograma de edades por problema registrado") +
    ylab("Frecuencia") +
    xlab("Mediciones") + 
    facet_wrap("servicio") +
  theme_light()

datos_limpios %>%
  ggplot() + 
  aes(edad) +
  geom_histogram(binwidth = 5, col="black", fill = "blue") + 
  ggtitle("Histograma de edades por problema registrado") +
  ylab("Frecuencia") +
  xlab("Mediciones") + 
  facet_wrap("servicio") +
  theme_light()


# HistÃ³rico de datos: fecha
class(datos$fecha_alta)
class(datos$hora_alta)

conta_registro_dia <- count(datos, fecha = datos$fecha_alta)
conta_registro_hora <- count(datos, hora = datos$hora_alta)

head(conta_registro_dia)
head(conta_registro_hora)

# Por dÃ­a
df_registro_dia <- data.frame(eje_x=as.Date(conta_registro_dia$fecha), n = conta_registro_dia$n)
head(df_registro_dia)
# Por hora
df_registro_hora <- data.frame(eje_x=conta_registro_hora$hora, n = conta_registro_hora$n)
head(df_registro_hora)

# DF a graficar 
df_registro <- df_registro_dia
dim(df_registro)
names(df_registro)

p <- ggplot(df_registro, aes(x=eje_x, y=n)) + 
  geom_line( color="blue") + 
  geom_point() +
  labs(x = "Fecha", 
       y = "Acumulado de casos",
       title = paste("Casos de violencia en MÃ©xico:", 
                     format(Sys.time(), 
                            tz="America/Mexico_City", 
                            usetz=TRUE))) +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  # color, Ã¡ngulo y estilo de las abcisas y ordenadas 

p <- p  + scale_x_date(labels = date_format("%d-%m-%Y")) # paquete scales

p <- p +
  theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
  annotate("text", x = df_registro$eje_x[round(dim(df_registro[1])*0.4)], y = max(df_registro$n), colour = "blue", size = 5, label = paste("Ãšltima actualizaciÃ³n: ", df_registro$n[dim(df_registro)[1]]))
p
tail(df_registro)
class(df_registro$eje_x)

#Sesion 6
ts_por_mes <- df_registro %>% group_by(fecha = format(eje_x, "%Y-%m")) %>% summarise(frecuencia_llamadas = sum(n))
head(ts_por_mes)
tail(ts_por_mes)
(datos_ts <- ts(ts_por_mes$frecuencia_llamadas, start = c(2016, 11), end = c(2021, 1), frequency = 12))
plot(datos_ts, 
     main = "Acumulado de casos", 
     xlab = "Tiempo",
     ylab = "Frecuencia de llamadas",
     sub = "Serie mensual: Noviembre de 2016 - Enero de 2021")

# Se debe elegir entre modelo aditivo o modelo multiplicativo cuando sea razonable suponer la descomposición
decom_add <- decompose(datos_ts)
plot(decom_add, xlab = "Tiempo", 
     sub = "Descomposición de los datos de cantidad llamadas")

decom_mul <- decompose(datos_ts, type = "mult")
plot(decom_mul, xlab = "Tiempo", 
     sub = "Descomposición de los datos de cantidad llamadas")

Tendencia <- decom_add$trend
Estacionalidad <- decom_add$seasonal
Aleatorio <- decom_add$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de Producción de Electricidad", 
        ylab = "Producción de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
datos_ts[20]

Time <- 1:length(datos_ts)
Imth <- cycle(datos_ts)
Elec.lm <- lm(log(datos_ts) ~ Time + I(Time^2) + factor(Imth))
acf(resid(Elec.lm), main = "")
title(main = "Correlograma de la serie de residuales del modelo de regresión",
      sub = "Serie de producción de electricidad")

plot(resid(Elec.lm), type = "l", main = "", xlab = "", ylab = "")
title(main = "Serie de residuales del modelo de regresión ajustado",
      sub = "Serie de producción de electricidad",
      xlab = "Tiempo",
      ylab = "Residuales")

best.order <- c(0, 0, 0)
best.aic <- Inf
for(i in 0:2)for(j in 0:2){
  model <- arima(resid(Elec.lm), order = c(i, 0, j))
  fit.aic <- AIC(model)
  if(fit.aic < best.aic){
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(Elec.lm), order = best.order)
    best.aic <- fit.aic
  }
}

best.order
acf(resid(best.arma), main = "")
title(main = "Serie de residuales del modelo ARMA(2, 0) ajustado",
      sub = "Serie de residuales del modelo de regresión ajustado a los datos de electricidad")

new.time <- seq(length(datos_ts)+1, length = 12)
new.data <- data.frame(Time = new.time, Imth = rep(1:12, 1))
predict.lm <- predict(Elec.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 12)
elec.pred <- ts(exp(predict.lm + predict.arma$pred), start = 2021, freq = 12)

ts.plot(cbind(datos_ts, elec.pred), lty = 1:2, 
        col = c("blue", "red"), xlab = "Tiempo", 
        ylab = "Producción de electricidad",
        main = "Predicción de los datos de producción de electricidad",
        sub = "Predicción de 36 meses")

# Sesion 5
Global.ts <- ts(df_registro$n, st = c(2010,11,1), end = c(2021, 1, 10), fr = 365)
plot(Global.ts, xlab = "Tiempo", ylab = "Promedio de la suma de goles", main = "Serie del promedio por mes de la suma de goles de partidos disputados",
     sub = "Serie mensual: Agosto de 2010 a Diciembre de 2019")

plot(df_registro$eje_x, df_registro$n, xlab = "Fecha", 
     ylab = "Frecuencia de llamadas", pch = 16)

m1 <- lm(df_registro$n~df_registro$eje_x)
summary(m1)

plot(df_registro$eje_x, df_registro$n, xlab = "Fecha", 
     ylab = "Frecuencia de llamadas", pch = 16)
RunSize <- df_registro$eje_x
RunTime <- df_registro$n
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresiÃ³n estimada
mtext(expression(paste('Modelo de regresiÃ³n lineal simple:',
                       ' ',
                       y[i] == beta[0] + beta[1]*x[i] + e[i])),
      side = 3, adj=1, font = 2)

# Recta de regresiÃ³n poblacional

text(x = 200, y = 240, expression(paste('Recta de regresiÃ³n:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)


# Recta de regresiÃ³n estimada

text(x = 350, y = 180, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresiÃ³n estimada

text(x = 350, y = 160, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == 149.74770 + 0.25924*x[i])),
     adj = 1, font = 2)

round(confint(m1, level = 0.95), 3)


#21 de marzo de 2020 - Inicio de cuarentena

# Limpieza donde estado_hechos != Null
#Determinar mejores variables, implementar redes neuronales

# Analizar relaciÃ³n entre algunas propiedades

#1.Establecer datos de entrenamiento y prueba

#(dim(datos_limpios[datos_limpios$servicio == "JURÃƒ\u008dDICO",])[1] + dim(datos_limpios[datos_limpios$servicio == "MÃƒâ€°DICO",])[1] + dim(datos_limpios[datos_limpios$servicio == "PSICOLÃƒâ€œGICO",])[1])
plt1 <- ggplot(datos_limpios, aes(x = edad, y = ocupacion, colour = as.factor(servicio))) +
  geom_point(size=3) +
  ggtitle("Wines")
plt1

train <- cbind(datos_limpios, class.ind(as.factor(datos_limpios$servicio)))
# Set labels name
names(train) <- c(names(datos_limpios),"l1","l2","l3")
train

datos_jur <- train[train$l1 == 1,]
datos_med <- train[train$l2 == 1,]
datos_psi <- train[train$l3 == 1,]
#minimo_datos <- as.numeric(min(dim(datos_jur)[1], dim(datos_med)[1], dim(datos_psi)[1]))
minimo_datos <- 1200
indice <- ceiling(minimo_datos * .8)

rows_jur <- sample(nrow(datos_jur))
rows_med <- sample(nrow(datos_med))
rows_psi <- sample(nrow(datos_psi))

datos_train <- bind_rows(datos_jur[rows_jur[1:indice-1],], datos_med[rows_med[1:indice-1],], datos_psi[rows_psi[1:indice-1],])
names(datos_train)

datos_test <- bind_rows(datos_jur[rows_jur[indice:minimo_datos],], datos_med[rows_med[indice:minimo_datos],], datos_psi[rows_psi[indice:minimo_datos],])
names(datos_test)

# Generar factores para valores categÃ³ricos
columnas_a_factor <- c("hora_alta","sexo","estado_civil","ocupacion","escolaridad", "estado_usuaria", "tematica1", "estado_hechos")
datos_train[columnas_a_factor] <-lapply(datos_train[columnas_a_factor], as.factor) %>% data.frame()
datos_train[columnas_a_factor] <-lapply(datos_train[columnas_a_factor], as.numeric) %>% data.frame()
datos_train[columnas_a_factor] <-lapply(datos_train[columnas_a_factor], scale) %>% data.frame()
head(datos_train)
names(datos_train)

# 4. ML

#set.seed(123)
# Add l1,l2,l3
new_cols <- c(columnas_a_factor, c("l1","l2","l3"))
#Remove cols if necessary
names(datos_train[new_cols])
nom <- names(train[new_cols])
f <- as.formula(paste("l1 + l2 + l3 ~", paste(nom[!nom %in% c("l1","l2","l3")], collapse = " + ")))
f

corr <- lm(f, data = datos_train)
corr

nn1 <- neuralnet(f,
                data = datos_train,
                hidden = c(9,6,4),
                act.fct = "logistic",
                linear.output = FALSE,
                threshold = 0.3,
                rep = 3,
                lifesign = "minimal")

# Generate percentage
nn1$result.matrix[,3]["error"] <- nn1$result.matrix[,3]["error"] / 100
plot(nn1)
plot(nn1, rep = 'best')

# 5. Check performance / compute predictions

#Test the resulting output
datos_test[columnas_a_factor] <-lapply(datos_test[columnas_a_factor], as.factor) %>% data.frame()
datos_test[columnas_a_factor] <-lapply(datos_test[columnas_a_factor], as.numeric) %>% data.frame()
datos_test[columnas_a_factor] <-lapply(datos_test[columnas_a_factor], scale) %>% data.frame()
head(datos_test)

pr.nn <- compute(nn1, datos_test)
head(pr.nn$net.result)

for (index in 1:length(datos_test)){
  main_ind <- which.max(pr.nn$net.result[index,])
  pr.nn$net.result[index,main_ind] <- 1
}
head(pr.nn$net.result)

# Sample mean of l1
original_values <- max.col(datos_test[, c("l1","l2","l3")])
pr.nn_2 <- max.col(pr.nn$net.result)
mean(pr.nn_2 == original_values)

#Confusion matrix
results <- data.frame(actual = datos_test$l3, prediction = pr.nn$net.result[,3])
row_test <- datos_test[1,]
as.numeric(factor(row_test$servicio))

head(results)
roundedresults<-sapply(results,round,digits=0)
head(roundedresults)
roundedresultsdf=data.frame(roundedresults)
tabla_res <- table(actual=roundedresultsdf$actual,prediction=roundedresultsdf$prediction)
tabla_res
precision <- tabla_res[2,2] / (tabla_res[2,2] + tabla_res[1,2])

# Intervalo de confianza
#vector_40_pruebas <- sample(4:10, 40, replace=T)
vector_40_pruebas <- seq(1,40,by=1)
rows_mixed <- sample(nrow(datos_test))

for (i in 1:length(vector_40_pruebas)){
  count <- 0
  for (j in 1:10){
    row_test <- datos_test[rows_mixed[i],]
    pr.nn_aux <- compute(nn1, row_test)
    index_final <- which.max(pr.nn_aux$net.result)
    if (as.numeric(factor(row_test$servicio)) == as.numeric(index_final) ){
      count <- count + 1
    }
  }
  print(count)
  vector_40_pruebas[i] <- 1
}
vector_40_pruebas
#vector_40_pruebas <- sample(4:10, 40, replace=T)

# Intervalos de confianza
muestra <- c(5,4,5,9,10,7,9,5,9,5,5,10,8,7,7,7,4,5,5,10,6,7,8,8,8,7,8,10,4,6,6,7,10,4,8,9,10,6,9,10) / 10
head(muestra)
#YA QUE TENEMOS 40 OBSERVACIONES DE NUESTRA VARIABLE ALEATORIA, PODEMOS CREAR UN
#INTERVALO DE CONFIANZA A NIVEL 90% PARA LA MEDIA.

sigma_gorro <- sd(muestra)
mu_gorro    <- mean(muestra)
n           <- length(muestra)

#Dado que se utilizó una estimación de la varianza y no
#la varianza real, entonces nuestra variable pivotal tiene
#una distribución t-student con n-1 grados de libertad.

a           <- qt(.05,39)  
b           <- qt(.95,39)
conf_int    <- c(0,0)
conf_int[1] <- mu_gorro - (b*sigma_gorro/sqrt(n))
conf_int[2] <- mu_gorro - (a*sigma_gorro/sqrt(n))
