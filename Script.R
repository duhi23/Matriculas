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
Ing <- data.frame(Percentil=seq(10,90, by=10), Ingreso=round(quantile(data14[,ING_PERSONA], probs = seq(0.1, 0.9, by=0.1)), 2))

ggplot(Ing, aes(x=Percentil, y=Ingreso)) + geom_line(size=1.3) + geom_point(size=4, shape=21, fill="white") + 
      labs(title='Ingreso por Miembro Familiar')


ggplot(data14, aes(x=factor(TIP_COLEGIO), y=ING_PERSONA)) +
      geom_boxplot(outlier.size=1.5, outlier.shape=21)

ggplot(data14, aes(x=factor(PROVINCIA), y=ING_PERSONA)) +
      geom_boxplot(outlier.size=1.5, outlier.shape=21)

data14[,EDAD:=floor((as.Date("2015-06-16") - as.Date(data14$NACIMIENTO))/365)]

summary(as.numeric(data14[,EDAD]))

## Datos Año 2015
data15 <- data.table(read_excel("data_vice.xlsx", sheet=1))
str(data15)
dim(data15)


