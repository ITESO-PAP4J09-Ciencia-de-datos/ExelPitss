
# Librerias ----------------------------------------------------------------
library(tidymodels) # Utilizado para la creacion de modelos matemacticos
library(patchwork)# Utilizado para la creación de modelos matematicos
library(forecast) # Libreria con funciones para realizar predicciones 
library(easypackages)
library(fpp3)
 
# Datos  ------------------------------------------------------------------
#source("SLA's/Limpieza.R", encoding = "utf-8")

Train_D_tsb <- tiempos_os_tidy_tbl %>%
  filter(Fecha_recepion >= "2018-03-01",
         Tiempos == "Tiempo de respuesta") %>%
  group_by(Ruta, Tiempos) %>%
  summarise_by_time(
    .date_var           = Fecha_recepion,
    .by                 = "week",
    Tiempo_de_respuesta = mean(hora_decimal),
    Q_OS                = n())%>% 
  ungroup() %>% 
  mutate(Fecha_recepion = yearweek(Fecha_recepion))%>%
  #mutate(Tiempo_de_respuesta = difference(Tiempo_de_respuesta)) %>% 
  as_tsibble(index = Fecha_recepion,
             key = c(Ruta, Tiempos))

# Transformaciones  -------------------------------------------------------
lambda1 <- Train_D_tsb %>%
  filter(Ruta == "JALISCO") %>% 
  features(Tiempo_de_respuesta, features = guerrero) %>%
  pull(lambda_guerrero)


# Modelos Dinamicos de resgresion -----------------------------------------------------------------
Modelos_D_fit <- Train_D_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  model(
    
    dynamic_reg       = ARIMA(box_cox(Tiempo_de_respuesta, lambda1) ~ pdq(d = 1) + PDQ(0,0,0) + fourier(K = 2) + Q_OS)
  ) 

Q_OS_last_year <- Train_D_tsb %>% 
  filter(Ruta == "JALISCO", Fecha_recepion < yearweek("2020-11-15")) %>% 
  filter_index("2019 W46" ~ "2019 W49") %>% pull(Q_OS)

escenarios <- scenarios(
  last_year = new_data(Train_D_tsb %>% 
                         filter(Ruta == "JALISCO", 
                                Fecha_recepion < yearweek("2020-11-15")), 4) %>% 
    mutate(Q_OS = Q_OS_last_year),
  real_val  = new_data(Train_D_tsb %>% 
                         filter(Ruta == "JALISCO", 
                                Fecha_recepion < yearweek("2020-11-15")), 4) %>% 
    mutate(Q_OS = Train_D_tsb %>% filter(Ruta == "JALISCO") %>%  
             filter_index("2020 W46" ~ "2020 W49") %>% pull(Q_OS))
)

Modelos_D_fc <- Modelos_D_fit %>% 
  forecast(new_data = escenarios)



