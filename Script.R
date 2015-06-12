###################################
####  Analisis Matriculas EPN  ####
###################################

library(readxl)
library(ggplot2)
library(data.table)

list.files()
ls("package:readxl")

## Datos Año 2014
data14 <- data.table(read_excel("data_vice.xlsx", sheet=2))
str(data14)
dim(data14)
unlist(lapply(data14, class))
colnames(data14)

data14$MIEMBROS <- as.numeric(data14$MIEMBROS)
# Numero de miembros
table(data14[,MIEMBROS])
round(100*prop.table(table(data14[,MIEMBROS])), 2)

data14$ING_ESTUDIANTE <- as.numeric(data14$ING_ESTUDIANTE)
data14$ING_CONYUGE <- as.numeric(data14$ING_CONYUGE)
data14$ING_PADRE <- as.numeric(data14$ING_PADRE)
data14$ING_MADRE <- as.numeric(data14$ING_MADRE)
data14$MIEMBROS <- as.numeric(data14$MIEMBROS)

# Ingreso total
data14[,ING_TOTAL:=ING_ESTUDIANTE+ING_CONYUGE+ING_PADRE+ING_MADRE]
summary(data14[,.(ING_ESTUDIANTE, ING_CONYUGE, ING_PADRE, ING_MADRE, ING_TOTAL)])
# Ingreso por persona
data14[, ING_PERSONA:= ING_TOTAL/MIEMBROS]
summary(data14[,ING_PERSONA])
round(quantile(data14[,ING_PERSONA], probs = seq(0.1, 0.9, by=0.1)), 2)


## Datos Año 2015
data15 <- data.table(read_excel("data_vice.xlsx", sheet=1))
str(data15)
dim(data15)


