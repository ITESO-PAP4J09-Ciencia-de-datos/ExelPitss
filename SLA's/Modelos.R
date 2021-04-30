# Librerias ---------------------------------------------------------------
##library(tidymodels) # Utilizado para la creacion de modelos matemacticos
library(patchwork)# Utilizado para la creación de modelos matematicos
##library(forecast) # Libreria con funciones para realizar predicciones 
##library(easypackages)
library(fpp3)
library(tsibble)
library(feasts)
library(fable)
library(fable.prophet) # Modelo Prophet

# Datos  ------------------------------------------------------------------
## Datos de la limpieza de datos

source("Limpieza.R", encoding = "utf-8")

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
## Descomposicion SLT para modelos 
dcmp <- Train_tsb %>%
  filter(Ruta == "JALISCO") %>% 
  model(STL(box_cox(Tiempo_de_respuesta,lambda1) ~ trend(window = 7)+ season(window = "periodic")
            ,  robust=TRUE)) %>%
  components() %>%
  select(-.model)

dcmp_Jal_sem <- Train_tsb %>%
  filter(Ruta == "JALISCO") %>% 
  model(STL(Tiempo_de_respuesta ~ trend(window = 7)+ season(window = "periodic") ,  robust=TRUE))

## Modelos de predicción
Modelos_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  model(
     # "ARIMA_BC"          = ARIMA(box_cox(Tiempo_de_respuesta,lambda1)),
      # "ARIMA_fourier2"    = ARIMA(Tiempo_de_respuesta ~ pdq(d = 1) + PDQ(0,0,0) + 
      #                               fourier(K = 2)),
    # "ARIMA213"          = ARIMA(Tiempo_de_respuesta ~ pdq(2, 1, 3) + PDQ(0,0,0)),
    # "Prophet"           = prophet(Tiempo_de_respuesta ~ season(order = 2)),
   # "SN_42"              = SNAIVE(Tiempo_de_respuesta ~ lag(42)),
    #"SN_44"              = SNAIVE(Tiempo_de_respuesta ~ lag(44)),
    # "Dr"                 = RW(Tiempo_de_respuesta ~ drift()),
    
    # "SN_44"           = SNAIVE(Tiempo_de_respuesta~ lag(44) + drift()),

     "SN_42_44"            = (SNAIVE(Tiempo_de_respuesta~ lag(44) + drift())+
                          SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()))/2 , 
    # "ARMAFurierK2_SN44"   =  decomposition_model(
    #      STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
    #      ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
    #      SNAIVE(season_year~ lag(44) + drift())),
     # "ARMAFurierK2_SN44"   =  decomposition_model(
     #   STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
     #   ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
     #   SNAIVE(season_year~ lag(44) + drift())),
    # "Drift_SN44"   =  decomposition_model(
    #   STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
    #   RW(season_adjust ~ drift()),
    #   SNAIVE(season_year~ lag(44) + drift())),
    # "NAIVE_SN44"   =  decomposition_model(
    #   STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
    #   NAIVE(season_adjust),
    #   SNAIVE(season_year~ lag(44) + drift())),
    # "ETS_Holt’sL_SN44"   =  decomposition_model(
    #   STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
    #   ETS(season_adjust ~error("A") + trend("A") + season("N") ),
    #   SNAIVE(season_year~ lag(44) + drift())),
     # "SN_44"           = SNAIVE(Tiempo_de_respuesta~ lag(44) + drift()),
     # "SN_42"           = SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()),
     # "ETS"  = ETS(Tiempo_de_respuesta ~ error("A") + trend("A") + season("N"))
    # "Des_ARF2"  = decomposition_model(
    #   STL(Tiempo_de_respuesta ~ trend(window = 7)+ season(window = "periodic") , robust = TRUE),
    #   ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
    #   SNAIVE(season_year~ lag(44) + drift()),
    # )
     "Com3_T5"  =  (decomposition_model(
       STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
       ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
       SNAIVE(season_year~ lag(44) + drift())) +
         SNAIVE(Tiempo_de_respuesta ~ lag(42)) + 
         RW(Tiempo_de_respuesta ~ drift())
    )/3
    

  ) %>% 
  mutate(
     # SN_44_42 = (SN_42 + SN_44)/2,
     # SN_Dr_ARBC = (SN_42_44+ Dr + ARIMA_BC)/3,
     # SN_44_42_Dr = (SN_42 + SN_44+ Dr )/3,
     # SN_44_42_AFK2 = (SN_42 + SN_44+ ARIMA_fourier2 )/3,
     # DSTLAFK2SN44L_SN42 = (ARMAFurierK2_SN44 + SN_42 )/2 ,
     # DSTLAFK2SN44L_SN_44 = (ARMAFurierK2_SN44 +SN_44 )/2 , 
     # DSTLAFK2SN44L_SN_42_44 = (ARMAFurierK2_SN44 + SN_44_42 )/2 ,
     # DSTLAFK2SN44L_SN_42_44_Dr = (ARMAFurierK2_SN44 + SN_44_42 + Dr)/3 , #Casi el Com3_T5
     # SN_44_DR_Mean = (SN_44+Dr)/2,
     # SN_42_DR_Mean = (SN_42+Dr)/2,
     # SNB_Dr_ARF2 = (SN_42_44+ Dr + ARIMA_fourier2)/3,
     # SN_44_Dr_ARF2 = (SN_44+Dr+ARIMA_fourier2 )/3,
     # SN_44_Dr_ARBC = (SN_44+Dr+ ARIMA_BC)/3,
     # SN_42_44 = (SN_42 + SN_44)/2,
     # SN_42_44_ARF2 = (SN_42_44+ ARIMA_fourier2)/2,
     # SN_42_44_ARBC = (SN_42_44+ARIMA_BC)/2
     # Com2 = (Des_ARF2 + SN_42)/2, 
  )


Modelos_fc <- Modelos_fit %>% 
  forecast(h = "1 month")



## Analisis de las metricas estadisticas 

Error_test <- accuracy(Modelos_fc, Train_tsb)


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
 #   select(Com3_T5) %>% 
 #   gg_tsresiduals()+
 #   ggtitle("Residuales Com3_T5")

# Validacion Cruzada  -----------------------------------------------------
# Time series cross-validation accuracy
Train_tsb_tr <-  Train_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  stretch_tsibble(.init = 46, .step = 1) %>%
  relocate(Fecha_recepion, Ruta, .id)


# TSCV accuracy
ValCru_Ts <- Train_tsb_tr %>%
  model(
    "SN_44_42"=(SNAIVE(Tiempo_de_respuesta~ lag(44) + drift())+
           SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()))/2,
    "Com3_T5"  =  (decomposition_model(
      STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
      ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
      SNAIVE(season_year~ lag(44) + drift())) +
        SNAIVE(Tiempo_de_respuesta ~ lag(42)) + 
        RW(Tiempo_de_respuesta ~ drift())
    )/3
    ) %>%
  forecast(h = "11 week") %>%
  accuracy(Train_tsb)

# Training set accuracy
ValCru_Tr <- Train_tsb %>%
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  model( 
    "SN_44_42"=(SNAIVE(Tiempo_de_respuesta~ lag(44) + drift())+
                   SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()))/2,
    "Com3_T5"  =  (decomposition_model(
      STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
      ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
      SNAIVE(season_year~ lag(44) + drift())) +
        SNAIVE(Tiempo_de_respuesta ~ lag(42)) + 
        RW(Tiempo_de_respuesta ~ drift())
    )/3
          ) %>%
  accuracy()

# Previsión de la precisión del horizonte con validación cruzada
fc <- Train_tsb_tr %>%
  model(
    "Com3_T5"  =  (decomposition_model(
      STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
      ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
      SNAIVE(season_year~ lag(44) + drift())) +
        SNAIVE(Tiempo_de_respuesta ~ lag(42)) + 
        RW(Tiempo_de_respuesta ~ drift())
                  )/3
    ) %>%
  forecast(h = "8 week") %>%  
  group_by(.id ) %>%
  mutate(h = row_number()) %>%
  ungroup() 

RMSE1 <- fc %>%
  fabletools::accuracy(Train_tsb, by = c("h", ".model","Ruta")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  ggtitle("RMSE vs h del Modelo Com3_T5")+
  geom_point()

fc2 <- Train_tsb_tr %>%
  model(
     "SN_44_42"=(SNAIVE(Tiempo_de_respuesta~ lag(44) + drift())+
                       SNAIVE(Tiempo_de_respuesta~ lag(42) + drift()))/2
  ) %>%
  forecast(h = "8 week") %>%  
  group_by(.id ) %>%
  mutate(h = row_number()) %>%
  ungroup() 

RMSE2 <- fc2 %>%
  fabletools::accuracy(Train_tsb, by = c("h", ".model","Ruta")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  ggtitle("RMSE vs h del Modelo SN_42_44")+
  geom_point()
# Validación modelos  ----------------------------------------------

Modelos_Val_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  model(
    "Com3_T5"  =  (decomposition_model(
      STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
      ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
      SNAIVE(season_year~ lag(44) + drift())) +
        SNAIVE(Tiempo_de_respuesta ~ lag(42)) + 
        RW(Tiempo_de_respuesta ~ drift())
    )/3
  )
Modelos_val_fc <- Modelos_Val_fit %>% 
  forecast(h = "11 week")

Error_Val <- accuracy(Modelos_val_fc, Train_tsb)


# Prediccion final  -------------------------------------------------------

MFinal_fit <- Train_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2021-01-21")) %>% 
  model(
    "Com3_T5"  =  (decomposition_model(
      STL(Tiempo_de_respuesta ~ trend(window = 5)+ season(window = "periodic") , robust = TRUE),
      ARIMA(season_adjust ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2)),
      SNAIVE(season_year~ lag(44) + drift())) +
        SNAIVE(Tiempo_de_respuesta ~ lag(42)) + 
        RW(Tiempo_de_respuesta ~ drift())
    )/3
  )

MFinal_fc <- MFinal_fit %>% 
  forecast(h = "3 month")

