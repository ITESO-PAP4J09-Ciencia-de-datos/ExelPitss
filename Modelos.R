# Librerias ---------------------------------------------------------------
library(tidymodels) # Utilizado para la creacion de modelos matemacticos
library(patchwork)# Utilizado para la creación de modelos matematicos
library(forecast) # Libreria con funciones para realizar predicciones 
library(easypackages)
library(fpp3)
# Datos  ------------------------------------------------------------------
## Datos de la limpieza de datos

source("Limpieza.R", encoding = "utf-8")

## series de tiempo de tecnico de visita, ruta y modelo de impresora para el Treain de modelos
# Train_tsb <- tiempos_os_tidy_tbl %>%
#   pivot_wider(
#     names_from  = Tiempos,
#     values_from = hora_decimal
#   ) %>% 
#   select(`Tiempo de respuesta`,
#          `Técnico de visita`, 
#          Fecha_recepion,
#          Modelo, 
#          Ruta, 
#          Cliente) %>% 
#   rename( Tiempo_de_respuesta = `Tiempo de respuesta`) %>% 
#   filter(Fecha_recepion >= "2018-03-01",Fecha_recepion < "2020-10-01") %>%
#   group_by(`Técnico de visita`, Modelo, Ruta, Cliente) %>%
#   summarise_by_time(
#     .date_var    = Fecha_recepion,
#     .by          = "month",
#     Tiempo_de_respuesta = mean(Tiempo_de_respuesta))%>%
#   mutate(Fecha_recepion = as.character(Fecha_recepion) %>%
#            yearmonth())%>%
#   as_tsibble(index = Fecha_recepion,
#              key = c(`Técnico de visita`,Modelo,Ruta,Cliente))

# series de tiempo de RUta 
Train_tsb <- tiempos_os_tidy_tbl %>%
  # pivot_wider(
  #   names_from  = Tiempos,
  #   values_from = hora_decimal
  # ) %>% 
  # rename(Tiempo_de_respuesta = `Tiempo de respuesta`) %>% 
  filter(Fecha_recepion >= "2018-03-01",
         Tiempos == "Tiempo de respuesta") %>%
  group_by(Ruta, Tiempos) %>%
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "month",
    Tiempo_de_respuesta = mean(hora_decimal))%>% 
  ungroup() %>% 
  mutate(Fecha_recepion = yearmonth(Fecha_recepion))%>%
  as_tsibble(index = Fecha_recepion,
             key = c(Ruta, Tiempos))

# Entrenamiento del modelo ------------------------------------------------

Modelos_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO",Fecha_recepion <= as.Date("2020-01-01" )) %>% 
  model(
    "Drift" = RW(log(Tiempo_de_respuesta) ~  drift()),
    "ETS"   = ETS(log(Tiempo_de_respuesta) ~ error("A")+ trend("A")+
    season("N")),
    "NAIVE" = NAIVE(log(Tiempo_de_respuesta)),
    "ARIMA" = ARIMA(log(Tiempo_de_respuesta)),
    "SNAIVE" = SNAIVE(log(Tiempo_de_respuesta)),
    "MEAN" = MEAN(log(Tiempo_de_respuesta))
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
  geom_vline( xintercept = as.Date("2020-09-01", color = "Red",
                                  linetype = "dashed"))+
  annotate("label", x = c(as.Date("2019-03-01"),as.Date("2020-05-01"),as.Date("2020-11-01")),
                        y = 3.5, label = c("Train set", "Test set","Validation set"),
                        color = c("black","blue","green"))

Error_Train <- Modelos_fit %>% 
  accuracy()

Error_test <- accuracy(Modelos_fc, Train_tsb)
 Residios <- augment(Modelos_fit)
 
 Modelos_fit %>%
   select(Drift) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales Drift")
 
 Modelos_fit %>%
   select(ETS) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales ETS")
 
 Modelos_fit %>%
   select(NAIVE) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales NAIVE")
 
 Modelos_fit %>%
   select(ARIMA) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales ARIMA")
 
 Modelos_fit %>%
   select(SNAIVE) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales SNAIVE")
 
 Modelos_fit %>%
   select(MEAN) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales MEAN")
 
# Test de los modelos  ----------------------------------------------------


# Validación de los modelos  ----------------------------------------------



