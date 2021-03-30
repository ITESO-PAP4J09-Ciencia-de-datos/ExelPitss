# Librerias ---------------------------------------------------------------


# Datos  ------------------------------------------------------------------
## Datos de la limpieza de datos
source("Limpieza.R", local = knitr::knit_global(),encoding = "utf-8")

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
  forecast(h = "1 year") %>% 
  autoplot(tiempos_mensual_ruta_tsbl)
gg_tsdisplay(hora_decimal, plot_type = "partial")
# Entrenamiento del modelo ------------------------------------------------


# Test de los modelos  ----------------------------------------------------


# Validación de los modelos  ----------------------------------------------




