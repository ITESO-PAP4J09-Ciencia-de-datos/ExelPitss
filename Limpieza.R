
# Limpieza de datos 29/03/2021
# Equipo 1 PAP Ciencia de datos 
# Paqueterias -------------------------------------------------------------

# Librerias  --------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(timetk)
library(tsibble)
library(feasts)
# Limpieza ----------------------------------------------------------------

tiempos_os_tbl <- read_xlsx("Reporte_Tiempo_respuesta.xlsx")

tiempos_os_tidy_tbl <- tiempos_os_tbl %>%
  
  # Cambiar el nombre de las cols. de fecha a fecha-hora
  rename(
    Fecha_hora_recepcion = `Fecha recepción`,
    Fecha_hora_cierre    = `Fecha cierre`
  ) %>% 
  
  # Crear dos columnas para la pura fecha
  mutate(
    Fecha_recepion = as.Date(Fecha_hora_recepcion),
    Fecha_cierre   = as.Date(Fecha_hora_cierre)
  ) %>% 
  filter(Estatus == "RESUELTA",
         Categoría == "CORRECTIVO",
         !is.na(`Tiempo transcurrido`),
         !is.na(`Tiempo efectivo`)) %>% 
  
  # Cambiando algunos nombres por facilidad
  rename(
    OS           = `N.° de orden`,
    serie        = `N.° de serie`,
    num_equipo   = `N.° de equipo`,
    num_cliente  = `N.°de cliente`,
    cant_visitas = `N.° de visitas`,
    Tiempo_limite_restante_de_respuesta      = `Tiempo límite restante...24`,
    Tiempo_limite_restante_de_solución_total =`Tiempo límite restante...27`
  ) %>%
  
  # Agregar variable con solo el mes-año
  mutate(mes = tsibble::yearmonth(Fecha_recepion)) %>% 
  
  # Acomodar la fecha al inicio de la tabla
  relocate(mes, starts_with("Fecha")) %>% 
  
  # Separar las columnas de tiempo transcurrido y tiempo efectivo en horas y min.
  separate(`Tiempo transcurrido`, into = c("t_trans_horas", "t_trans_minutos"),
           sep = ":") %>% 
  
  separate(`Tiempo efectivo`, into = c("t_efect_horas", "t_efect_minutos")) %>% 
   
  # Crear variables de tipo duración
  mutate(
    t_transcurrido = duration(hours = as.integer(t_trans_horas), 
                              minutes = as.integer(t_trans_minutos)),
    t_efectivo     = duration(hours = as.integer(t_efect_horas), 
                              minutes = as.integer(t_efect_minutos))
  ) %>% 
  
  # Quitar cols. que contengan en el nombre "horas" o "minutos"
  select(-contains("horas"), -contains("minutos")) %>% 
  
  # Pasar a filas las columnas de tiempos
  pivot_longer(
    cols      = contains("Tiempo"),
    names_to  = "Tiempos",
    values_to = "Valor"
  ) %>% 
  
  # Separar la col. Valor en horas y minutos
  separate(Valor, into = c("Horas","Minutos"), sep = ":") %>% 
  
  # Crear columna con hora en formato decimal
  mutate(
    hora_decimal = as.numeric(Horas) + as.numeric(Minutos)/60
  ) %>% 
  
  # quitar cols. innecesarias
  select(-c(Horas, Minutos)) %>% 
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  mutate(
    Cociente_tiempo = as.numeric(`Tiempo de respuesta`)/
      as.numeric(`Limite de tiempo de respuesta`)
  ) %>% 
  
  # Se creó una columna de clasificación
  mutate(
    Estatus_de_Atencion = ifelse( Cociente_tiempo >= 1, 1 , 0)
  ) %>% 
  pivot_longer(
    cols      = contains("Tiempo"),
    names_to  = "Tiempos",
    values_to = "hora_decimal"
  ) 
