##################################
#####     Nivelacion EPN     #####
##################################

library(readxl)
library(dplyr)
library(ggvis)

list.files()

data <- read_excel("datosnivelacion.xls", sheet = 1)
glimpse(data)
colnames(data)

data %>% select(PROMEDIO) %>% summary()

# Var APRUEBA (0-NO, 1-SI)

data <- data %>% mutate(APRUEBA=ifelse(PROMEDIO>0,1,0))
data %>% select(APRUEBA) %>% table()

# Var EDAD
data <- data %>% rename(FENAC=`FECHA NACIMIENTO`, TIPO=`TIPO UNIDADEDUCATIVA`, INGRESO=`INGRESO FAMILIAR`,
                MIEMBROS=`NUMERO DE MIEMBROS`, QUINTIL=`QUINTIL PERIODOACTUAL`, 
                P_INICIO=`PERIODO INICIO NIVELACION`, P_FIN=`PERIODO FIN NIVELACION`, 
                N_PER=`NUMERO PERIODOS NIVELACION`, P_INI_CARR=`PERIODO INICIO CARRERA`,
                P_ULT_CARR=`PERIODO ULTIMA MATRICULA CARRERA`, FAC_PRE=`FACULTAD DE PREFERENCIA`,
                CARR_PRE=`CARRERA PREFERENCIA`, GRADO_COL=`AÑO GRADO UNIDADEDUCATIVA`)

# Genero
data %>% ggvis(~factor(GENERO), fill=~factor(GENERO), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="GENERO") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="GENERO")

# Quintil
data$QUINTIL <- as.numeric(data$QUINTIL)

data %>% ggvis(~factor(QUINTIL), fill=~factor(QUINTIL), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="QUINTIL") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="QUINTIL")

by(data, data$QUINTIL, summary)

# Region
data %>% ggvis(~factor(REGION), fill=~factor(REGION), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="REGION") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="REGION")

# Costa
data %>% filter(REGION=="COSTA") %>% ggvis(~factor(PROVINCIA), fill=~factor(PROVINCIA), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="PROVINCIA") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="PROVINCIA")

# Sierra
data %>% filter(REGION=="SIERRA") %>% ggvis(~factor(PROVINCIA), fill=~factor(PROVINCIA), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="PROVINCIA") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="PROVINCIA")

# Amazonia
data %>% filter(REGION=="AMAZONIA") %>% ggvis(~factor(PROVINCIA), fill=~factor(PROVINCIA), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="PROVINCIA") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="PROVINCIA")

# Extranjeros
data %>% filter(REGION=="EXTRANJEROS") %>% ggvis(~factor(PAIS), fill=~factor(PAIS), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="PAIS") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="PAIS")

# Tipo Colegio
data %>% mutate(TIPO_COL=ifelse(TIPO %in% c("PARTICULAR LAICO", "PARTICULAR RELIGIOSO"), "PARTICULAR", TIPO)) %>% 
      ggvis(~factor(TIPO_COL), fill=~factor(TIPO_COL), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="TIPO COLEGIO") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="TIPO COLEGIO")

# Numero de periodos
data$N_PER <- as.numeric(data$N_PER)

data %>% ggvis(~factor(N_PER), fill=~factor(N_PER), fillOpacity := 0.6) %>% layer_bars() %>% 
      add_axis("x",title="QUINTIL") %>% add_axis("y", title="ESTUDIANTES") %>% 
      add_legend("fill", title="QUINTIL")

# Calculo de edad
data %>% mutate(FECHA=ifelse(P_INICIO==""))



table(data$APRUEBA, data$GENERO)
