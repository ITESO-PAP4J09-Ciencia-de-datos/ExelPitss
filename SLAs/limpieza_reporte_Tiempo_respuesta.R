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
library(fpp3)
library(easypackages)

#("tidyverse", "tidyquant", "lubridate", "patchwork", "fpp2","fpp3","scales")
# DATA --------------------------------------------------------------------

# tiempos_os_tbl <- read_csv("SLAs/Reporte_Tiempo_respuesta.csv",
#                            locale = readr::locale(encoding = "latin1"),
#                            col_types = cols(
#                              `Tiempo transcurrido` = col_character(),
#                              `Tiempo efectivo`     = col_character()
#                            ))

tiempos_os_tbl <- read_xlsx("Reporte_Tiempo_respuesta.xlsx")

# tiempos_os_tbl %>% glimpse()

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
  mutate(
    Estatus_de_Atencion = ifelse( Cociente_tiempo >= 1, 1 , 0)
  ) %>% 
  pivot_longer(
    cols      = contains("Tiempo"),
    names_to  = "Tiempos",
    values_to = "hora_decimal"
  ) 

# EDA ---------------------------------------------------------------------
tiempos_mensual_ruta_tsbl <- tiempos_os_tidy_tbl %>%
  group_by(Ruta, Tiempos) %>%
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "month",
    hora_decimal = mean(hora_decimal)
  ) %>%
  mutate(Fecha_recepion = as.character(Fecha_recepion) %>%
           yearmonth())%>%
  filter(Fecha_recepion >= yearmonth("2018-02-03")) %>%
  as_tsibble(index = Fecha_recepion,
             key = c(Ruta, Tiempos))


p <- tiempos_mensual_ruta_tsbl %>%
  filter(Ruta %in% c("JALISCO", "NUEVO LEON")) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Tiempos, scales = "free_y") +
  theme(legend.position = "none")+
  ggtitle("Comparación de los tiempos entre Jalisco y Nuevo Leon")


# Graficar tiempos de respuesta por ruta
p2 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Tiempo de respuesta"),
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  ggtitle("Tiempo de respuesta de cada Ruta")+
  theme(legend.position = "none")


# Graficar tiempo efectivo en sitio por ruta
p3 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Tiempo efectivo en sitio"),
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  theme(legend.position = "none")



tiempos_mensual_tecnicoV_tsbl <- tiempos_os_tidy_tbl %>%
  # Regresar a cols. separadas cada indicador
  group_by(`Técnico de visita`, Tiempos) %>%
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "month",
    hora_decimal = mean(hora_decimal)
  ) %>%
  mutate(Fecha_recepion = as.character(Fecha_recepion) %>%
           yearmonth()) %>%
  filter(Fecha_recepion >= yearmonth("2018-02-03")) %>%
  as_tsibble(index = Fecha_recepion,
             key = c(`Técnico de visita`, Tiempos))

# Graficar porcentajes de respuesta por ruta
p4 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo"),
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  theme(legend.position = "none")



# Gráfica de Baja Cal Sur coeficiente de tiempos Junio 2019 por técnico de visita
tiempos_ruta_tecnico_tsbl <- tiempos_os_tidy_tbl %>%
  filter(mes == yearmonth("2019 jun."),
         Ruta %in% c("BAJA CALIFORNIA SUR")) %>%
  group_by(`Técnico de visita`, Tiempos) %>%
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "day",
    hora_decimal = mean(hora_decimal)
  ) %>%
  as_tsibble(index = Fecha_recepion,
             key = c(`Técnico de visita`, Tiempos))

# Graficar Junio Baja California Sur
p5 <- tiempos_ruta_tecnico_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo")) %>%
  autoplot(hora_decimal) +
  theme(legend.position = "none")

 ## Solo hay una observacion, por lo que no se genera una grafica

#Estados donde el cociente fue mayor a 0.5
tiempos_cociente_M2_tsbl<- tiempos_os_tidy_tbl %>%
  filter(Fecha_recepion >= "2020-01-01",
         Tiempos %in% c("Cociente_tiempo"),
         hora_decimal>= 0.5) %>%
  group_by(`Técnico de visita`, Tiempos) %>%
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "day",
    hora_decimal = mean(hora_decimal)) %>%
  as_tsibble(index = Fecha_recepion,
             key = c(`Técnico de visita`, Tiempos))

# Graficar Junio Baja California Sur
p6 <- tiempos_cociente_M2_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo")) %>%
  autoplot(hora_decimal) +
  ggtitle("Graficar Junio Baja California Sur")+
  facet_wrap(~`Técnico de visita`, scales = "free_y") +
  theme(legend.position = "none")




#grafica de estados con cocientes mayores al 0.5
p7 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo"),
         hora_decimal >= 0.5,
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  theme(legend.position = "none")

# Correlaciones ---------------------------------------------------------

# Correlaciones de `Tiempo efectivo en sitio`,`Tiempo de respuesta` y `Limite tiempo de solución total`
## Correlaciones no atendidas en tiempo 
correlacion_tiempos1_Fals <- tiempos_os_tidy_tbl %>% 
  filter(
    !is.na(hora_decimal)
  ) %>% 
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  filter(
    Estatus_de_Atencion == 1
  )%>% 
  select(
    `Tiempo efectivo en sitio`,
    `Tiempo de respuesta`,
    `Limite tiempo de solución total`
  ) 
correlacion_tiempos2_Fals <- cor(correlacion_tiempos1_Fals, method= "pearson")
correlacion_tiempos3_Fals <- as_cordf(correlacion_tiempos2_Fals)

##Correlaciones atendidas a tiempo
correlacion_tiempos1_True <- tiempos_os_tidy_tbl %>% 
  filter(
    !is.na(hora_decimal)
  ) %>% 
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  filter(
    Estatus_de_Atencion == 0
  )%>% 
  select(
    `Tiempo efectivo en sitio`,
    `Tiempo de respuesta`,
    `Limite tiempo de solución total`
  ) 

correlacion_tiempos2_True <- cor(correlacion_tiempos1_True, method= "pearson")
correlacion_tiempos3_True <- as_cordf(correlacion_tiempos2_True)

# Correlaciones ruta-técnico de visita por tiempos-spearman 
Ruta_cor <- tiempos_os_tidy_tbl %>% 
  filter(
    !is.na(hora_decimal)
  ) %>% 
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  filter(
    Cociente_tiempo >= 1,
    !is.na(Ruta),
  ) %>% 
  select(
    `Tiempo efectivo en sitio`,
    `Tiempo de respuesta`,
    `Limite de tiempo de respuesta`,
    Tiempo_limite_restante_de_respuesta,
    Tiempo_limite_restante_de_solución_total,
    cant_visitas,
    Ruta,
    OS
  ) %>% 
  group_by(Ruta) %>% 
  summarise(
    LRespuesta_TRespuesta_Cor = cor(`Tiempo de respuesta`,`Limite de tiempo de respuesta`, 
                                    method = "spearman"),
    Canvisitas_TRespuesta_Cor = cor(cant_visitas,`Tiempo de respuesta`, 
                                    method = "spearman")
  )

Tecnico_visita_corr <- tiempos_os_tidy_tbl %>% 
  filter(
    !is.na(hora_decimal)
  ) %>% 
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  filter(
    Cociente_tiempo >= 1,
    !is.na(Ruta),
  ) %>% 
  group_by(`Técnico de visita`) %>% 
  summarise(
    LRespuesta_TRespuesta_Cor = cor(`Tiempo de respuesta`,`Limite de tiempo de respuesta`,
                                    method ="spearman"),
    Canvisitas_TRespuesta_Cor = cor(cant_visitas,`Tiempo de respuesta`,
                                    method = "spearman"),
  )

Modelo_corr <- tiempos_os_tidy_tbl %>% 
  filter(
    !is.na(hora_decimal)
  ) %>% 
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  filter(
    Cociente_tiempo >= 1,
    !is.na(Modelo),
  ) %>% 
  group_by(Modelo) %>% 
  summarise(
    LRespuesta_TRespuesta_Cor = cor(`Tiempo de respuesta`,`Limite de tiempo de respuesta`,
                                    method ="spearman"),
    Canvisitas_TRespuesta_Cor = cor(cant_visitas,`Tiempo de respuesta`,
                                    method = "spearman")
  )


# Tendencias --------------------------------------------------------------
#Serie de tiempo de cociente para jalisco del 2019 al 2021
Ten1Jal <- tiempos_mensual_ruta_tsbl %>%
  filter(
    Tiempos %in% c("Cociente_tiempo"),
    Ruta%in% c("MICHOACAN"),
    Fecha_recepion >= yearmonth("2019-01"),
    !is.na(hora_decimal)
  )

#descomposicion por el modelo STL
dcmp_Jal <- Ten1Jal %>%
  model(STL(hora_decimal))
#mostrar la ts
components(dcmp_Jal)


