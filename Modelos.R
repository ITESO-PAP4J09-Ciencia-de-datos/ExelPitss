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
## Optimisación de lamda para Box-Cox
lambda1 <- Train_tsb %>%
  filter(Ruta == "JALISCO") %>% 
  features(Tiempo_de_respuesta, features = guerrero) %>%
  pull(lambda_guerrero)

## Modelos de predicción
Modelos_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion <= yearmonth("2020-04-01")) %>% 
  model(
     "ETS"   = ETS(Tiempo_de_respuesta ~ error("A")+ trend("A")+
               season("N")),
    "ARIMA" = ARIMA(Tiempo_de_respuesta),
    
    "ETS_BC"   = ETS(box_cox(Tiempo_de_respuesta,lambda1) ~ error("A")+ trend("A")+
                 season("N")),
   "ARIMA_BC" = ARIMA(box_cox(Tiempo_de_respuesta,lambda1)),
   
    "ETS_AH" = ETS(box_cox(Tiempo_de_respuesta,lambda1) ~ error("M")+ trend("A")+
                   season("A")),
    "ETS_MH"= ETS(box_cox(Tiempo_de_respuesta,lambda1) ~ error("M")+ trend("A")+
          season("M")),
    "ETS_HD"= ETS(box_cox(Tiempo_de_respuesta,lambda1) ~ error("A")+ trend("Ad")+
          season("M"))
  )

Modelos_fc <- Modelos_fit %>% 
  forecast(h = "5 month")


## Grafica de las predicciones contra los datos 
Modelos_fc %>% 
  autoplot(filter_index(Train_tsb, "2018-03-01" ~ .), level = NULL) +
  ggtitle("Entrenamiento") +
  xlab("Años") + ylab("horas") +
  guides(colour=guide_legend(title="Forecast")) +
  geom_vline(xintercept = as.Date("2020-05-01", color = "Red",
             linetype = "dashed"))+
  geom_vline( xintercept = as.Date("2020-9-01", color = "Red",
                                  linetype = "dashed"))+
  annotate("label", x = c(as.Date("2019-03-01"),as.Date("2020-07-01"),as.Date("2020-11-01")),
                        y = 3.5, label = c("Train set", "Test set","Validation set"),
                        color = c("black","blue","green"))

## Analisis de las metricas estadisticas 
Error_Train <- Modelos_fit %>% 
  accuracy()

Error_test <- accuracy(Modelos_fc, Train_tsb)

# Error_test2 <- accuracy(Modelos_fc2, Train_tsb)
# Residios <- augment(Modelos_fit)
 

# ANALISIS DE RESIDUOS
 Modelos_fit %>%
   select(ETS) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales ETS")
 
 Modelos_fit %>%
   select(ARIMA) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales ARIMA")
 
 Modelos_fit %>%
   select(ETS_BC) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales ETS_BC")
 
 Modelos_fit %>%
   select(ARIMA_BC) %>% 
   gg_tsresiduals()+
   ggtitle("Residuales ARIMA_BC")
 

# Validación modelos  ----------------------------------------------



