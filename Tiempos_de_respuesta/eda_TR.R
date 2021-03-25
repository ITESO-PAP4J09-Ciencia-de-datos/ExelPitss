
# librerías ---------------------------------------------------------------

# ciencia de datos
library(tidyverse)

# importación de datos
library(readxl)



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
  mutate(Recepcion = as.Date(Recepcion),
         Cierre    = as.Date(Cierre))

