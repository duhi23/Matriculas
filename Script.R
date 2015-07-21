###################################
####  Analisis Matriculas EPN  ####
###################################

install.packages('readxl', dependencies=TRUE)
library(readxl)
library(ggplot2)
library(data.table)
library(dplyr)

list.files()
ls("package:readxl")
ls("package:data.table")

## Datos Año 2014
data14 <- data.table(read_excel("data_vice.xlsx", sheet=2))
str(data14)
dim(data14)
head(data14)
unlist(lapply(data14, class))
colnames(data14)

data14$MIEMBROS <- as.numeric(data14$MIEMBROS)
data14$ING_ESTUDIANTE <- as.numeric(data14$ING_ESTUDIANTE)
data14$ING_CONYUGE <- as.numeric(data14$ING_CONYUGE)
data14$ING_PADRE <- as.numeric(data14$ING_PADRE)
data14$ING_MADRE <- as.numeric(data14$ING_MADRE)
data14$MIEMBROS <- as.numeric(data14$MIEMBROS)

data14[, ING_TOTAL:=ING_ESTUDIANTE+ING_CONYUGE+ING_PADRE+ING_MADRE]
data14[, ING_PERSONA:= ING_TOTAL/MIEMBROS]

# Filtro Previo - Solteros e ingresos superiores a cero
datos14 <- data14 %>% select(MIEMBROS, EST_CIVIL, GENERO, TIP_COLEGIO, ING_TOTAL, ING_PERSONA) %>% 
      filter(ING_TOTAL > 0, ING_TOTAL < 30000, EST_CIVIL == "S")

## Estadisticos descriptivos
table(datos14[,MIEMBROS])
round(100*prop.table(table(datos14[,MIEMBROS])), 2)

ggplot(datos14, aes(x=1, y=ING_TOTAL)) + geom_boxplot(outlier.size=1.5, outlier.shape=21, outlier.colour="red") +
      labs(y="Ingreso Familiar Total") + stat_boxplot(geom ='errorbar')

quantile(datos14[,ING_TOTAL], probs = seq(0.01, 0.99, by= 0.01))

# Base acotada al percentil 99
datos14 <- data14 %>% select(MIEMBROS, EST_CIVIL, GENERO, TIP_COLEGIO, ING_TOTAL, ING_PERSONA) %>% 
      filter(ING_TOTAL > 0, ING_TOTAL < 4300, EST_CIVIL == "S")

ggplot(datos14, aes(x=1, y=ING_TOTAL)) + geom_boxplot(outlier.size=1.5, outlier.shape=21, outlier.colour="red") +
      labs(y="Ingreso Familiar Total") + stat_boxplot(geom ='errorbar')

quantile(datos14[,ING_TOTAL], probs = seq(0.01, 0.99, by= 0.01))

# Ingreso total & persona
summary(datos14[,.(ING_TOTAL, ING_PERSONA)])
# Ingreso persona en quintiles
quantile(datos14[,ING_PERSONA], probs = seq(0.2, 0.8, by= 0.2))

# Ingreso por edad
datos14[,EDAD:=floor((as.Date("2015-06-16") - as.Date(data14$NACIMIENTO))/365)]
summary(as.numeric(data14[,EDAD]))



## Datos Año 2015
data15 <- data.table(read_excel("data_vice.xlsx", sheet=1))
str(data15)
dim(data15)
head(data15)
unlist(lapply(data15, class))
colnames(data15)

data15$MIEMBROS <- as.numeric(data15$MIEMBROS)
data15$ING_ESTUDIANTE <- as.numeric(data15$ING_ESTUDIANTE)
data15$ING_CONYUGE <- as.numeric(data15$ING_CONYUGE)
data15$ING_PADRE <- as.numeric(data15$ING_PADRE)
data15$ING_MADRE <- as.numeric(data15$ING_MADRE)
data15$MIEMBROS <- as.numeric(data15$MIEMBROS)

data15[, ING_TOTAL:=ING_ESTUDIANTE+ING_CONYUGE+ING_PADRE+ING_MADRE]
data15[, ING_PERSONA:= ING_TOTAL/MIEMBROS]

# Filtro Previo - Solteros e ingresos superiores a cero
datos15 <- data15 %>% select(MIEMBROS, EST_CIVIL, GENERO, TIP_COLEGIO, ING_TOTAL, ING_PERSONA) %>% 
      filter(ING_TOTAL > 0, ING_TOTAL < 30000, EST_CIVIL == "S")

## Estadisticos descriptivos
table(datos15[, MIEMBROS])
round(100*prop.table(table(datos15[,MIEMBROS])), 2)

ggplot(datos15, aes(x=1, y=ING_TOTAL)) + geom_boxplot(outlier.size=1.5, outlier.shape=21, outlier.colour="red") +
      labs(y="Ingreso Familiar Total") + stat_boxplot(geom ='errorbar')

quantile(datos14[,ING_TOTAL], probs = seq(0.01, 0.99, by= 0.01))

# Base acotada al percentil 99
datos15 <- data15 %>% select(MIEMBROS, EST_CIVIL, GENERO, TIP_COLEGIO, ING_TOTAL, ING_PERSONA) %>% 
      filter(ING_TOTAL > 0, ING_TOTAL < 3500, EST_CIVIL == "S")

ggplot(datos15, aes(x=1, y=ING_TOTAL)) + geom_boxplot(outlier.size=1.5, outlier.shape=21, outlier.colour="red") +
      labs(y="Ingreso Familiar Total") + stat_boxplot(geom ='errorbar')

quantile(datos15[,ING_TOTAL], probs = seq(0.01, 0.99, by= 0.01))

# Ingreso total & persona
summary(datos15[,.(ING_TOTAL, ING_PERSONA)])
# Ingreso persona en quintiles
quantile(datos15[,ING_PERSONA], probs = seq(0.2, 0.8, by= 0.2))
