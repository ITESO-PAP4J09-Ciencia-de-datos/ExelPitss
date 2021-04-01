# Librerias ---------------------------------------------------------------
library(tidymodels) # Utilizado para la creacion de modelos matemacticos
library(patchwork)# 
library(forecast) # Libreria con funciones para realizar predicciones 
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

Modelo_snaive <- Train_tsb %>%
  model(SNAIVE(`Tiempo_de_respuesta`)
        ) %>% 
  forecast(h = "1 month")
  
Modelo_snaive %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MsN Tiempo de respuesta CDMX")


Modelo_drift <- Train_tsb %>%
  model(Drift = RW(Tiempo_de_respuesta~drift())
  ) %>% 
  forecast(h = "1 month")

Modelo_drift %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MD Tiempo de respuesta CDMX")+
  facet_wrap(~ Tiempos, scales = "free_y")

Modelo_mean <- Train_tsb %>%
  model(MEAN(Tiempo_de_respuesta)
  ) %>% 
  forecast(h = "1 month")

Modelo_mean %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MM Tiempo de respuesta CDMX")+
  facet_wrap(~ Tiempos, scales = "free_y")

Modelo_naive <- TVisita_Ruta_Modelo_train_tsb %>%
  model(NAIVE(Tiempo_de_respuesta)
  ) %>% 
  forecast(h = "1 month")

Modelo_naive %>% 
  filter(Ruta == "CIUDAD DE MEXICO") %>% 
  autoplot(TVisita_Ruta_Modelo_train_tsb)+
  ggtitle("MN Tiempo de respuesta CDMX")+
  facet_wrap(~ Tiempos, scales = "free_y")

Modelos_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO",Fecha_recepion <= as.Date("2020-01-01" )) %>% 
  model(
    "Drift" = RW(log(Tiempo_de_respuesta) ~  drift()),
    "ETS"   = ETS(log(Tiempo_de_respuesta) ~ error("A")+ trend("A")+
    season("N")),
    "NAIVE" = NAIVE(log(Tiempo_de_respuesta)),
    "ARIMA" = ARIMA(log(Tiempo_de_respuesta)),
    "Seasonal naïve" = SNAIVE(log(Tiempo_de_respuesta))
  )
Modelos_fc <- Modelos_fit %>% 
  forecast(h = "8 month")

Modelos_fc %>% 
  autoplot(filter_index(Train_tsb, "2018-03-01" ~ .), level = NULL) +
  ggtitle("Entrenamiento") +
  xlab("Años") + ylab("horas") +
  guides(colour=guide_legend(title="Forecast")) +
  geom_vline(xintercept = as.Date("2020-02-01", color = "Red",
             linetype = "dashed"))+
  annotate("label", x = c(as.Date("2019-03-01"),as.Date("2020-07-01")),
                        y = 3.5, label = c("Train set", "Test set"),
                        color = c("black","blue"))

Error_modelos <- Modelos_fit %>% 
  accuracy()

 
# Test de los modelos  ----------------------------------------------------


# Validación de los modelos  ----------------------------------------------



