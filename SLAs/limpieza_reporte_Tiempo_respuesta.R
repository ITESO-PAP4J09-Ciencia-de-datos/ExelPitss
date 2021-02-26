# Limpieza de datos reporte de tiempos de respuesta
# Santi


# pkgs --------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(timetk)
library(tsibble)
library(feasts)

# DATA --------------------------------------------------------------------

# tiempos_os_tbl <- read_csv("SLAs/Reporte_Tiempo_respuesta.csv",
#                            locale = readr::locale(encoding = "latin1"),
#                            col_types = cols(
#                              `Tiempo transcurrido` = col_character(),
#                              `Tiempo efectivo`     = col_character()
#                            ))

tiempos_os_tbl <- read_xlsx("SLAs/Reporte_Tiempo_respuesta.xlsx")

tiempos_os_tbl %>% glimpse()


tiempos_os_tidy_tbl <- tiempos_os_tbl %>% 
  rename(
    Fecha_hora_recepcion = `Fecha recepción`,
    Fecha_hora_cierre    = `Fecha cierre`
  )
  # mutate_at(.vars = vars(starts_with("Fecha")),
  #           .funs = dmy) %>% 
  filter(Estatus == "RESUELTA",
         Categoría == "CORRECTIVO",
         !is.na(`Tiempo transcurrido`),
         !is.na(`Tiempo efectivo`)) %>% 
  rename(
    OS           = `N.° de orden`,
    serie        = `N.° de serie`,
    num_equipo   = `N.° de equipo`,
    num_cliente  = `N.°de cliente`,
    cant_visitas = `N.° de visitas`
  ) %>% 
  mutate(mes = tsibble::yearmonth(`Fecha recepción`)) %>% 
  relocate(mes, starts_with("Fecha")) %>% 
  separate(`Tiempo transcurrido`, into = c("t_trans_horas", "t_trans_minutos"),
           sep = ":") %>% 
  separate(`Tiempo efectivo`, into = c("t_efect_horas", "t_efect_minutos")) %>% 
  mutate(t_transcurrido = duration(hours = as.integer(t_trans_horas), minutes = as.integer(t_trans_minutos)),
         t_efectivo = duration(hours = as.integer(t_efect_horas), minutes = as.integer(t_efect_minutos))) %>% 
  select(-contains("horas"), -contains("minutos"))
