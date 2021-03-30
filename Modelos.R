# Librerias ---------------------------------------------------------------

 
# Datos  ------------------------------------------------------------------
## Datos de la limpieza de datos
source("limpieza_reporte_Tiempo_respuesta.R", 
       local = knitr::knit_global(),encoding = "utf-8")

# Ejemplo del profesor ----------------------------------------------------
fc <- tiempos_mensual_ruta_tsbl %>% 
  model(Drift = RW(hora_decimal~ drift())) %>% 
  forecast(h = "1 year")

fc %>% 
  filter(Ruta == "JALISCO") %>% 
  autoplot(tiempos_mensual_ruta_tsbl) +
  facet_wrap(~ Tiempos, scales = "free_y")


tiempos_mensual_ruta_tsbl %>% 
  filter(Tiempos == "Tiempo de respuesta",
         Ruta == "JALISCO") %>% 
  model(ARIMA(hora_decimal)) %>% 
  forecast(h = "3 month") %>% 
  autoplot(tiempos_mensual_ruta_tsbl)

#gg_tsdisplay(hora_decimal, plot_type = "partial")

# Entrenamiento del modelo ------------------------------------------------

Modelo_snaive <- TVisita_Ruta_Modelo_train_tsb %>%
  filter(Tiempos == "Tiempo de respuesta") %>%
  model(SNAIVE(hora_decimal)
        ) %>% 
  forecast(h = "1 month")
  
Modelo_snaive %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MsN Tiempo de respuesta CDMX")
  facet_wrap(~ Tiempos, scales = "free_y")

Modelo_drift <- TVisita_Ruta_Modelo_train_tsb %>%
  filter(Tiempos == "Tiempo de respuesta") %>%
  model(Drift = RW(hora_decimal~drift())
  ) %>% 
  forecast(h = "1 month")

Modelo_drift %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MD Tiempo de respuesta CDMX")+
  facet_wrap(~ Tiempos, scales = "free_y")

Modelo_mean <- TVisita_Ruta_Modelo_train_tsb %>%
  filter(Tiempos == "Tiempo de respuesta") %>%
  model(MEAN(hora_decimal)
  ) %>% 
  forecast(h = "1 month")

Modelo_mean %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MM Tiempo de respuesta CDMX")+
  facet_wrap(~ Tiempos, scales = "free_y")

Modelo_naive <- TVisita_Ruta_Modelo_train_tsb %>%
  filter(Tiempos == "Tiempo de respuesta") %>%
  model(NAIVE(hora_decimal)
  ) %>% 
  forecast(h = "1 month")

Modelo_naive %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MN Tiempo de respuesta CDMX")+
  facet_wrap(~ Tiempos, scales = "free_y")

# Test de los modelos  ----------------------------------------------------


# Validación de los modelos  ----------------------------------------------



