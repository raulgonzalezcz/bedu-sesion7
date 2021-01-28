#Reto2

install.packages('rvest')
library(rvest)

# Leemos el html
file <- read_html("https://www.glassdoor.com.mx/Sueldos/data-scientist-sueldo-SRCH_KO0,14.htm")    

tables<-html_nodes(file, "table")
tables
# Con bae en xml_nodeset
table1 <- html_table(tables[1], fill = TRUE)


table <- na.omit(as.data.frame(table1))
str(table)
head(table)

#Remover caracteres para tener números 
a <- gsub("MXN","",table$Sueldo)
a <- gsub("[^[:alnum:][:blank:]?]", "", a)
a <- gsub("mes", "", a)
a <- as.numeric(a)
table$Sueldo <- a

#Removiendo caracteres para saber empresa
empresa <- gsub("Sueldos para Data Scientist en ", "", table$Cargo)
empresa <- gsub("-.*", "", empresa)
table$Empresa <-empresa
table$Empresa
#Máximo sueldo
max.sueldo <- which.max(table$Sueldo)
paste("La empresa que mejor paga es ", table[max.sueldo,]$Empresa, "con un sueldo de ", table[max.sueldo,]$Sueldo)

#Mínimo sueldo
min.sueldo <- which.min(table$Sueldo)
paste("La empresa que peor paga es ", table[min.sueldo,]$Empresa, "con un sueldo de ", table[min.sueldo,]$Sueldo)
