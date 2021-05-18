# LIBRERIAS ---------------------------------------------------------------

# ciencia de datos
library(tidyverse)

# importación de datos
library(readxl)

# elementos de shiny 
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

# pronósticos
library(tsibble)
library(feasts)
library(fable)

# visualización de datos
library(plotly)

# DATOS -------------------------------------------------------------------

# COLORES
az_os <- rgb(0,0,51/255) # "#000033"
az_cl <- rgb(0,154/255,203/255) # "#009ACB"
negro <- rgb(51/255,51/255,51/255) # "#333333"
n_os <- rgb(226/255,120/255,49/255) # "#E27831"
n_cl <- rgb(248/255,148/255,56/255) # "#F89438"

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

# datos diarios
datos_tsbl <- datos %>% dplyr::filter(IS.visita != "Consultoria Dual",
                                      Estatus   == "RESUELTA",
                                      Categoría == "CORRECTIVO") %>% 
  group_by(Recepcion, Ruta) %>% 
  summarise(across(.cols = c(TR, TSP, TMO, TST), mean),
            .groups = "drop") %>% 
  pivot_longer(
    cols      = starts_with("T"), 
    names_to  = "SLA", 
    values_to = "Tiempos"
  ) %>% 
  as_tsibble(
    # index : la variable temporal
    index = Recepcion,
    # key : la(s) variable(s) que identifican a cada serie de tiempo
    key   = c(Ruta, SLA) 
  ) %>% 
  fill_gaps(.full = TRUE, Tiempos = mean(Tiempos)) %>% 
  filter_index("2018-02-12" ~ "2021-03-10")

# datos semanales
datos_week_tsbl <- datos_tsbl %>% 
  group_by_key() %>% 
  index_by(Semana = yearweek(Recepcion)) %>% 
  summarise(Tiempos = mean(Tiempos), .groups = "drop")

# datos mensuales
datos_month_tsbl <- datos_tsbl %>% 
  group_by_key() %>% 
  index_by(Mes = yearmonth(Recepcion)) %>%
  summarise(Tiempos = mean(Tiempos), .groups = "drop")

rutas <- datos %>%  
  distinct(Ruta) %>% 
  pull()

# frecuencias <- c("Diariamente","Semanalmente","Mensualmente")

modelos <- list(
  Media                     = MEAN(Tiempos), 
  Ingenuo                   = NAIVE(Tiempos),
  `Ingenuo Estacional`      = SNAIVE(Tiempos),
  Drift                     = RW(Tiempos ~ drift()),
  `Suavización Exponencial` = ETS(Tiempos),
  Arima                     = ARIMA(Tiempos)
  
)

# crear una lista con todas las tsibbles
tsbls <- list(
  Diaria  = datos_tsbl,
  Semanal = datos_week_tsbl,
  Mensual = datos_month_tsbl
)

frecuencias <- names(tsbls)

train <- tsbls %>% 
  map(. %>% filter_index("2018-03-01" ~ "2020-03-10"))

tiempos <- datos_tsbl %>% 
  distinct(SLA) %>% 
  pull()

colores <- c("#E27831","#009ACB","#EEB422","#00008B","#8B4500","#EE4000")

