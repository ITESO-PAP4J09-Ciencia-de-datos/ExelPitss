
# librerías ---------------------------------------------------------------

# ciencia de datos
library(tidyverse)

# importación de datos
library(readxl)

# compresión de listas
library(comprehenr)

# datos -------------------------------------------------------------------

datos <- read_xlsx("tiempos_respuesta.xlsx")

cols.num <- c("Tiempo transcurrido", "Tiempo efectivo en sitio",
              "Tiempo de respuesta", "Limite de tiempo de respuesta",
              "Tiempo límite restante...24", "Tiempo efectivo", 
              "Limite tiempo de solución total","Tiempo límite restante...27")
datos[cols.num] <- sapply(datos[cols.num],as.numeric)

minimos <- to_vec(for(i in 1:ncol(datos)) if(lapply(datos[,i],class)=="numeric") min(datos[,i]) else "NA")

minimo <- if(lapply(datos[,23],class)=="numeric"){
  min(datos[,23])
}
  


# EDA ---------------------------------------------------------------------

eda <- function(data){
  dqr <- tibble(
    "Columnas" = colnames(data),
    "Tipos de Datos" = as.character(as_tibble(sapply(datos, class))[1,]),
    "Datos Faltantes" = to_vec(for(i in 1:ncol(data)) sum(is.na(data[,i])) ),
    "Datos Presentes" = to_vec(for(i in 1:ncol(data)) sum(!is.na(data[,i])) ),
    "Valores Únicos" = to_vec(for(i in 1:ncol(data)) count(distinct(data[,i]))),
    "Valor Mínimo" = to_vec(for(i in 1:ncol(data)) if(lapply(data[,i],class)=="numeric") min(data[,i], na.rm = T) else "NA"),
    "Valor Máximo" = to_vec(for(i in 1:ncol(data)) if(lapply(data[,i],class)=="numeric") max(data[,i], na.rm = T) else "NA"),
    "Media" = to_vec(for(i in 1:ncol(data)) if(lapply(data[,i],class)=="numeric") max(data[,i]) else "NA")
  )
  print(dqr)
}

prueba <- eda(datos)