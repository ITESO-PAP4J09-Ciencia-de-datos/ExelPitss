
# librerías ---------------------------------------------------------------

# ciencia de datos
library(tidyverse)

# importación de datos
library(readxl)

# compresión de listas
library(comprehenr)

# datos -------------------------------------------------------------------

datos <- read_xlsx("Tiempos_de_respuesta/tiempos_respuesta.xlsx")

glimpse(datos)

datos <- datos %>% 
  mutate(across(contains("Tiempo"), as.numeric))

# EDA ---------------------------------------------------------------------

eda <- function(data){
  dqr <- tibble(
    "Columnas" = colnames(data),
    "Tipos de Datos" = as.character(as_tibble(sapply(datos, class))[1,]),
    "Datos Faltantes" = to_vec(for(i in 1:ncol(data)) sum(is.na(data[,i])) ),
    "Datos Presentes" = to_vec(for(i in 1:ncol(data)) sum(!is.na(data[,i])) ),
    "Valores ?nicos" = to_vec(for(i in 1:ncol(data)) count(distinct(data[,i]))),
    "Valor M?nimo" = to_vec(for(i in 1:ncol(data)) if(lapply(data[,i],class)=="numeric") min(data[,i], na.rm = T) else "NA"),
    "Valor M?ximo" = to_vec(for(i in 1:ncol(data)) if(lapply(data[,i],class)=="numeric") max(data[,i], na.rm = T) else "NA"),
    "Media" = to_vec(for(i in 1:ncol(data)) if(lapply(data[,i],class)=="numeric") max(data[,i]) else "NA")
  )
  print(dqr)
}

prueba <- eda(datos)