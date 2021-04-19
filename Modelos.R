# Librerias ---------------------------------------------------------------
library(tidymodels) # Utilizado para la creacion de modelos matemacticos
library(patchwork)# Utilizado para la creación de modelos matematicos
library(forecast) # Libreria con funciones para realizar predicciones 
library(easypackages)
library(fpp3)
library(fable.prophet) # Modelo Prophet
# Datos  ------------------------------------------------------------------
## Datos de la limpieza de datos

#source("Limpieza.R", encoding = "utf-8")

# series de tiempo de RUta 
Train_tsb <- tiempos_os_tidy_tbl %>%
  filter(Fecha_recepion >= "2018-03-01",
         Tiempos == "Tiempo de respuesta") %>%
  group_by(Ruta, Tiempos) %>%
  summarise_by_time(
    .date_var           = Fecha_recepion,
    .by                 = "week",
    
    Tiempo_de_respuesta = mean(hora_decimal)) %>% 
  
  ungroup() %>% 
  mutate(Fecha_recepion = yearweek(Fecha_recepion))%>%
  #mutate(Tiempo_de_respuesta = difference(Tiempo_de_respuesta)) %>% 
  as_tsibble(index = Fecha_recepion,
             key = c(Ruta, Tiempos))


# Entrenamiento del modelo ------------------------------------------------

## Optimización de lambda para Box-Cox
lambda1 <- Train_tsb %>%
  filter(Ruta == "JALISCO") %>% 
  features(Tiempo_de_respuesta, features = guerrero) %>%
  pull(lambda_guerrero)

## Modelos de predicción
Modelos_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  model(
     # "ARIMA_BC"          = ARIMA(box_cox(Tiempo_de_respuesta,lambda1)),
     # "ARIMA_fourier2"    = ARIMA(Tiempo_de_respuesta ~ pdq(d = 1) + PDQ(0,0,0) + 
     #                               fourier(K = 2)),
    # "ARIMA213"          = ARIMA(Tiempo_de_respuesta ~ pdq(2, 1, 3) + PDQ(0,0,0)),
    # "Prophet"           = prophet(Tiempo_de_respuesta ~ season(order = 2)),
    #"SN_42"              = SNAIVE(Tiempo_de_respuesta ~ lag(42)),
    #"SN_44"              = SNAIVE(Tiempo_de_respuesta ~ lag(44)),
    #"Dr"                 = RW(Tiempo_de_respuesta ~ drift()),
    "SN_44"           = SNAIVE(Tiempo_de_respuesta~ lag(44) + drift()),
    "SN_42"           = SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()),
    # "FORSADO1"    = (ARIMA(Tiempo_de_respuesta ~ pdq(d = 1) + PDQ(0,0,0) +  fourier(K = 2))         + 
    #                 SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()) +
    #                 SNAIVE(Tiempo_de_respuesta~ lag(44) + drift()) +
    #                 RW(Tiempo_de_respuesta ~ drift())      
    #                 )/4,
    # "FORSADO2"    = (ARIMA(Tiempo_de_respuesta ~ pdq(d = 1) + PDQ(0,0,0) +  fourier(K = 2))         + 
    #                    SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()) +
    #                    SNAIVE(Tiempo_de_respuesta~ lag(44) + drift())      
    #                   )/3,
    # "NAI"    = NAIVE(Tiempo_de_respuesta),
    # "M"     = MEAN(Tiempo_de_respuesta),
    #"ETS2"  = ETS(Tiempo_de_respuesta ~ error("A") + trend("N") + season("N")),
    # "ETS1"  = ETS(Tiempo_de_respuesta ~ error("A") + trend("M") + season("N"))
    
  ) %>% 
  mutate(
     # SN_42_44 = (SN_42 + SN_44)/2,
     # SN_Dr_ARBC = (SN_42_44+ Dr + ARIMA_BC)/3,
     # SN_42_44_Dr = (SN_42 + SN_44+ Dr )/3,
     # SN_44_DR_Mean = (SN_44+Dr)/2,
     # SN_42_DR_Mean = (SN_42+Dr)/2,
     # SNB_Dr_ARF2 = (SN_42_44+ Dr + ARIMA_fourier2)/3,
     # SN_44_Dr_ARF2 = (SN_44+Dr+ARIMA_fourier2 )/3,
     # SN_44_Dr_ARBC = (SN_44+Dr+ ARIMA_BC)/3,
     SN_42_44 = (SN_42 + SN_44)/2,
     # Best_ARF2 = (SN_42_44+ ARIMA_fourier2)/2,
     # Best_ARBC = (SN_42_44+ARIMA_BC)/2
  )


Modelos_fc <- Modelos_fit %>% 
  forecast(h = "1 month")



## Analisis de las metricas estadisticas 

Error_test <- accuracy(Modelos_fc, Train_tsb)


# descomposicion por el modelo STL con transformacion Box Cox

# Descomposicion
dcmp_Jal_sem <- Train_tsb %>%
  filter(Ruta == "JALISCO") %>% 
  model(STL(Tiempo_de_respuesta))
# components(dcmp_Jal) %>% autoplot()+ xlab("Semanas")

# ANALISIS DE RESIDUOS

# Modelos_fit %>%
#   select(ARIMA) %>% 
#   gg_tsresiduals()+
#   ggtitle("Residuales ARIMA")
# 
# # Modelos_fit %>%
# #   select(ETS_BC) %>% 
# #   gg_tsresiduals()+
# #   ggtitle("Residuales ETS_BC")
# 
# Modelos_fit %>%
#   select(ARIMA_BC) %>% 
#   gg_tsresiduals()+
#   ggtitle("Residuales ARIMA_BC")


# Validación modelos  ----------------------------------------------

Modelos_Val_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  model(
    "SN_44"           = SNAIVE(Tiempo_de_respuesta~ lag(44) + drift()),
    "SN_42"           = SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()),
  ) %>% 
  mutate(
    SN_42_44 = (SN_42 + SN_44)/2
  )

Modelos_val_fc <- Modelos_Val_fit %>% 
  forecast(h = "11 week")

Error_Val <- accuracy(Modelos_val_fc, Train_tsb)

# Modelos_val_fc %>% 
#   filter(.model %in% c("SN_42_44")) %>% 
#   autoplot(filter_index(Train_tsb, "2020-01-01" ~ "2021-01-31")) +
#   ggtitle("Forecast") +
#   xlab("Semanas") + ylab("horas") +
#   guides(colour=guide_legend(title="Forecast"))+
#   geom_vline(xintercept = as.Date("2020-11-08"), color = "Red",
#              linetype = "dashed")+
#   geom_vline(xintercept = as.Date("2020-12-01"), color = "Red",
#              linetype = "dashed")+
#   annotate("label", x = c(as.Date("2020-08-01"),as.Date("2020-12-01"),as.Date("2021-03-01")),
#            y = 3.5, label = c("Train set", "Test set","Val set"),
#            color = c("black","blue", "green"))
