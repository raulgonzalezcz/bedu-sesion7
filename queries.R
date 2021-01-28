# Reto1

# Comenzaremos con las librerias necesarias para realizar la conexión

library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)

# Una vez que se tengan las librerias necesarias se procede a la conexión con la base de datos de Shiny

MyDataBase <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest")

# Exploramos la BD

dbListTables(MyDataBase)

# Ahora usamos la tabla correcta

dbListFields(MyDataBase, 'CountryLanguage')

# Para realizar una consulta tipo MySQL sobre la tabla seleccionada haremos lo 
# siguiente

DataDB <- dbGetQuery(MyDataBase, "select * from CountryLanguage")

# Observemos que el objeto DataDB es un data frame, por lo tanto ya es un objeto 
# de R y podemos aplicar los comandos usuales

class(DataDB)
head(DataDB)
names(DataDB)

# Incluso podemos hacer unos de otros comandos de busqueda aplicando la 
# libreria dplyr
# Porcentaje de personas que hablan español en todos los países
cl.spanish <-  DataDB %>% filter(Language == "Spanish")   
class(cl.spanish)
head(cl.spanish)

cl.spanish %>% ggplot(aes( x = CountryCode, y=Percentage, fill = IsOfficial )) + 
  geom_bin2d() +
  coord_flip()
