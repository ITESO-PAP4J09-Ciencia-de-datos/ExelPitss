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
  mutate(Fecha_recepion = as.character(Fecha_recepion) %>%
           yearmonth())%>%
  filter(Fecha_recepion >= yearmonth("2018-02-03")) %>%
  as_tsibble(index = Fecha_recepion,
             key = c(Ruta, Tiempos))

# Graficar y comparar tiempos entre Jalisco y Nuevo Leon
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


# Tsibble con técnicos y tiempos
tiempos_mensual_tecnicoV_tsbl <- tiempos_os_tidy_tbl %>%
  group_by(`Técnico de visita`, Tiempos) %>%  #Regresar a cols. separadas cada indicador
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

# Series de tiempo para los modelos ---------------------------------------

#series de tiempo de tecnico de visita, ruta y modelo de impresora para el Treain de modelos
Train_tsb <- tiempos_os_tidy_tbl %>%
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  select(`Tiempo de respuesta`,
         `Técnico de visita`, 
         Fecha_recepion,
         Modelo, 
         Ruta, 
         Cliente) %>% 
  rename( Tiempo_de_respuesta = `Tiempo de respuesta`) %>% 
  filter(Fecha_recepion >= "2018-03-01",Fecha_recepion < "2020-10-01") %>%
  group_by(`Técnico de visita`,Tiempo_de_respuesta, Modelo, Ruta, Cliente) %>%
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "day",
    Tiempo_de_respuesta = mean(Tiempo_de_respuesta)) %>%
  as_tsibble(index = Fecha_recepion,
             key = c(`Técnico de visita`, Tiempo_de_respuesta,Modelo,Ruta,Cliente))

# series de tiempo de tecnico de visita, ruta y modelo de impresora para el Test de modelos

# series de tiempo de tecnico de visita, ruta y modelo de impresora para el Validation 
#de modelos

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

