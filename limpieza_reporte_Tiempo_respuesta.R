# Limpieza de datos reporte de tiempos de respuesta
# Santi


# pkgs --------------------------------------------------------------------

library(fpp3)          # Trabajar con ejemplos de libro
library(easypackages)  # Facilitar instalaciones
library(corrr)      # Libreria para la busqueda de correlaciones, para el analisis de datos
library(psych)      # Funciones de ayuda para analisis multivariable a diferenetes escalas 
library(ggcorrplot) # Forma de visualizar las correlaciones 

source("Limpieza.R", local = knitr::knit_global(),encoding = "utf-8") 

# EDA ---------------------------------------------------------------------

# Tsibble con ruta, técnico de visita y tiempos
tiempos_mensual_ruta_tsbl <- tiempos_os_tidy_tbl %>% 
  group_by(Ruta, Tiempos) %>%
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "month",
    hora_decimal = mean(hora_decimal)
  ) %>%
  mutate(Fecha_recepion = yearmonth(Fecha_recepion))%>%
  filter(Fecha_recepion >= yearmonth("2018-02-03")) %>%
  as_tsibble(index = Fecha_recepion,
             key = c(Ruta, Tiempos))

# Graficar y comparar tiempos entre Jalisco y Nuevo Leon
p <- tiempos_mensual_ruta_tsbl %>%
  filter(Ruta %in% c("JALISCO", "NUEVO LEON")) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Tiempos, scales = "free_y") +
  theme(legend.position = "none")+
  ggtitle("Grafica de Comparación de los tiempos entre Jalisco y Nuevo Leon")


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
  theme(legend.position = "none")+
  ggtitle("Grafica de los tiempos efectivos en sitio de cada ruta")


# # Tsibble con técnicos y tiempos
# tiempos_mensual_tecnicoV_tsbl <- tiempos_os_tidy_tbl %>%
#   group_by(`Técnico de visita`, Tiempos) %>%  #Regresar a cols. separadas cada indicador
#   summarise_by_time(
#     .date_var    = Fecha_recepion,
#     .by          = "month",
#     hora_decimal = mean(hora_decimal)
#   ) %>%
#   mutate(Fecha_recepion = yearmonth(Fecha_recepion)) %>%
#   filter(Fecha_recepion >= yearmonth("2018-02-03")) %>%
#   as_tsibble(index = Fecha_recepion,
#              key = c(`Técnico de visita`, Tiempos))

# Graficar porcentajes de respuesta por ruta
p4 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo"),
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  theme(legend.position = "none")+
  ggtitle("Grafica cociente del tiempo de respuesta/limite de tiempo de respuesta por ruta")

# # Gráfica de Baja Cal Sur coeficiente de tiempos Junio 2019 por técnico de visita
# tiempos_ruta_tecnico_tsbl <- tiempos_os_tidy_tbl %>%
#   filter(mes == yearmonth("2019 jun."),
#          Ruta %in% c("BAJA CALIFORNIA SUR")) %>%
#   group_by(`Técnico de visita`, Tiempos) %>%
#   summarise_by_time(
#     .date_var    = Fecha_recepion,
#     .by          = "day",
#     hora_decimal = mean(hora_decimal)
#   ) %>%
#   as_tsibble(index = Fecha_recepion,
#              key = c(`Técnico de visita`, Tiempos))

# #Estados donde el cociente fue mayor a 0.5
# tiempos_cociente_M2_tsbl<- tiempos_os_tidy_tbl %>%
#   filter(Fecha_recepion >= "2020-01-01",
#          Tiempos %in% c("Cociente_tiempo"),
#          hora_decimal>= 0.5) %>%
#   group_by(`Técnico de visita`, Tiempos) %>%
#   summarise_by_time(
#     .date_var    = Fecha_recepion,
#     .by          = "day",
#     hora_decimal = mean(hora_decimal)) %>%
#   as_tsibble(index = Fecha_recepion,
#              key = c(`Técnico de visita`, Tiempos))

#grafica de estados con medias cociente al 0.5 y menor a 1
p5 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo"),
         mean(hora_decimal) <= 1, mean(hora_decimal) >= 0.5,
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  theme(legend.position = "none")+
  ggtitle("Rutas con media de cociente entre 0.5 y 1")

#grafica de estados con medias cociente al 1
p6 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo"),
         mean(hora_decimal) >= 1,
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  theme(legend.position = "none")+
  ggtitle("Rutas con media de cociente mayor a 1")

#grafica de estados con medias cociente menores a 0.5
p7 <- tiempos_mensual_ruta_tsbl %>%
  filter(Tiempos %in% c("Cociente_tiempo"),
         mean(hora_decimal) <= 0.5,
         !is.na(Ruta)) %>%
  autoplot(hora_decimal) +
  facet_wrap(~Ruta, scales = "free_y") +
  theme(legend.position = "none")+
  ggtitle("Rutas con media de cociente menores a 0.5")
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
# Ruta_cor <- tiempos_os_tidy_tbl %>% 
#   filter(
#     !is.na(hora_decimal)
#   ) %>% 
#   pivot_wider(
#     names_from  = Tiempos,
#     values_from = hora_decimal
#   ) %>% 
#   filter(
#     Cociente_tiempo >= 1,
#     !is.na(Ruta),
#   ) %>% 
#   select(
#     `Tiempo efectivo en sitio`,
#     `Tiempo de respuesta`,
#     `Limite de tiempo de respuesta`,
#     Tiempo_limite_restante_de_respuesta,
#     Tiempo_limite_restante_de_solución_total,
#     cant_visitas,
#     Ruta,
#     OS
#   ) %>% 
#   group_by(Ruta) %>% 
#   summarise(
#     LRespuesta_TRespuesta_Cor = cor(`Tiempo de respuesta`,`Limite de tiempo de respuesta`, 
#                                     method = "spearman"),
#     Canvisitas_TRespuesta_Cor = cor(cant_visitas,`Tiempo de respuesta`, 
#                                     method = "spearman")
#   )
# 
# Tecnico_visita_corr <- tiempos_os_tidy_tbl %>% 
#   filter(
#     !is.na(hora_decimal)
#   ) %>% 
#   pivot_wider(
#     names_from  = Tiempos,
#     values_from = hora_decimal
#   ) %>% 
#   filter(
#     Cociente_tiempo >= 1,
#     !is.na(Ruta),
#   ) %>% 
#   group_by(`Técnico de visita`) %>% 
#   summarise(
#     LRespuesta_TRespuesta_Cor = cor(`Tiempo de respuesta`,`Limite de tiempo de respuesta`,
#                                     method ="spearman"),
#     Canvisitas_TRespuesta_Cor = cor(cant_visitas,`Tiempo de respuesta`,
#                                     method = "spearman"),
#   )

# Modelo_corr <- tiempos_os_tidy_tbl %>% 
#   filter(
#     !is.na(hora_decimal)
#   ) %>% 
#   pivot_wider(
#     names_from  = Tiempos,
#     values_from = hora_decimal
#   ) %>% 
#   filter(
#     Cociente_tiempo >= 1,
#     !is.na(Modelo),
#   ) %>% 
#   group_by(Modelo) %>% 
#   summarise(
#     LRespuesta_TRespuesta_Cor = cor(`Tiempo de respuesta`,`Limite de tiempo de respuesta`,
#                                     method ="spearman"),
#     Canvisitas_TRespuesta_Cor = cor(cant_visitas,`Tiempo de respuesta`,
#                                     method = "spearman")
#   )


# Tendencias --------------------------------------------------------------
#Serie de tiempo de cociente para jalisco del 2019 al 2021
Ten1Jal_tsb <- tiempos_mensual_ruta_tsbl %>%
  filter(
    Tiempos %in% c("Tiempo de respuesta"),
    Ruta%in% c("JALISCO"),
    Fecha_recepion >= yearmonth("2018-03"),
    !is.na(hora_decimal)
  )

#descomposicion por el modelo STL
dcmp_Jal <- Ten1Jal_tsb %>%
  model(STL(hora_decimal))
#mostrar la ts
components(dcmp_Jal)




