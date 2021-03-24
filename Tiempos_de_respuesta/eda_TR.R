
# librerías ---------------------------------------------------------------

# importación de datos
library(readxl)

# ciencia de datos
library(tidyverse)

# transformación de datos
library(dplyr)

# compresión de listas
library(comprehenr)

# EDA
library(psych)
library(dlookr)

# visualización de datos
library(ggplot2)

# datos -------------------------------------------------------------------

datos <- read_xlsx("tiempos_respuesta.xlsx")

#glimpse(datos)

datos <- datos %>% 
  mutate(across(contains("Tiempo"), as.numeric)) %>% 
  rename_all(funs(make.names(.))) %>% 
  rename(Orden = N...de.orden,
         Serie = N...de.serie,
         Equipo = N...de.equipo,
         Num.Cliente = N..de.cliente,
         Distribuidor = Grupo.empresarial,
         Nivel = Nivel.de.servicio,
         Recepcion = `Fecha.recepción`,
         Ejecutivo = Usuario.captura,
         IS.asignado = `Técnico.asignado`,
         IS.visita = `Técnico.de.visita`,
         Visitas = N...de.visitas,
         Cierre = Fecha.cierre,
         TST = Tiempo.transcurrido,
         TMO = Tiempo.efectivo.en.sitio,
         TR = Tiempo.de.respuesta,
         TLR = Limite.de.tiempo.de.respuesta,
         TLR.restante = `Tiempo.límite.restante...24`,
         TSP = Tiempo.efectivo,
         TLS = `Limite.tiempo.de.solución.total`,
         TLS.restante = `Tiempo.límite.restante...27`) %>% 
  mutate(Clasificacion = if_else(TR > TLR,0,1))


# EDA ---------------------------------------------------------------------

dqr_numericos <-
  mutate(dlookr::describe(datos)) %>%
  mutate(dlookr::normality(datos)) %>% 
  select(everything(), -c(p00,p01,p05,p10,p20,p30,p40,p60,p70,p80,p100,vars))

# DIAGNOSIS NUMERICOS
diagnosis_numericos <- diagnose_numeric(datos)

categoricos <- datos %>% 
  select_if(is.character) 

dqr_categoricos <- tibble(
  Variables = colnames(categoricos),
  Tipo = as.character(as_tibble(sapply(categoricos, class))[1,]),
  Presentes = to_vec(for(i in 1:ncol(categoricos)) sum(!is.na(categoricos[,i])) ),
  Faltantes = to_vec(for(i in 1:ncol(categoricos)) sum(is.na(categoricos[,i])) ),
  Unicos = to_vec(for(i in 1:ncol(categoricos)) count(distinct(categoricos[,i])))
) 

# DIAGNOSIS CATEGORICOS
diagnosis_categoricos <- diagnose_category(datos)
 
#EDA.report <- dlookr::eda_report(datos, output_format = "html")


# Diagnosis ---------------------------------------------------------------


  
# DIAGNOSE REPORT
#reporte diagnose_report(output_format = )



