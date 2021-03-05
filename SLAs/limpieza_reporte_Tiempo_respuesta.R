# Limpieza de datos reporte de tiempos de respuesta
# Santi


# pkgs --------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(timetk)
library(tsibble)
library(feasts)

# DATA --------------------------------------------------------------------

# tiempos_os_tbl <- read_csv("SLAs/Reporte_Tiempo_respuesta.csv",
#                            locale = readr::locale(encoding = "latin1"),
#                            col_types = cols(
#                              `Tiempo transcurrido` = col_character(),
#                              `Tiempo efectivo`     = col_character()
#                            ))

tiempos_os_tbl <- read_xlsx("SLAs/Reporte_Tiempo_respuesta.xlsx")

tiempos_os_tbl %>% glimpse()


tiempos_os_tidy_tbl <- tiempos_os_tbl %>%
  # Cambiar el nombre de las cols. de fecha a fecha-hora
  rename(
    Fecha_hora_recepcion = `Fecha recepción`,
    Fecha_hora_cierre    = `Fecha cierre`
  ) %>% 
  # Crear dos columnas para la pura fecha
  mutate(
    Fecha_recepion = as.Date(Fecha_hora_recepcion),
    Fecha_cierre   = as.Date(Fecha_hora_cierre)
  ) %>% 
  filter(Estatus        == "RESUELTA",
         Categoría      == "CORRECTIVO",
         Fecha_recepion >= "2018-02-01",
         !is.na(`Tiempo transcurrido`),
         !is.na(`Tiempo efectivo`)
         ) %>% 
  # Cambiando algunos nombres por facilidad
  rename(
    OS           = `N.° de orden`,
    serie        = `N.° de serie`,
    num_equipo   = `N.° de equipo`,
    num_cliente  = `N.°de cliente`,
    cant_visitas = `N.° de visitas`
  ) %>%
  # Agregar variable con solo el mes-año
  mutate(mes = tsibble::yearmonth(Fecha_recepion)) %>% 
  # Acomodar la fecha al inicio de la tabla
  relocate(mes, starts_with("Fecha")) %>% 
  # Separar las columnas de tiempo transcurrido y tiempo efectivo en horas y min.
  separate(`Tiempo transcurrido`, into = c("t_trans_horas", "t_trans_minutos"),
           sep = ":") %>% 
  separate(`Tiempo efectivo`, into = c("t_efect_horas", "t_efect_minutos")) %>% 
  # Crear variables de tipo duración
  mutate(
    t_transcurrido = duration(hours = as.integer(t_trans_horas), 
                              minutes = as.integer(t_trans_minutos)),
    t_efectivo     = duration(hours = as.integer(t_efect_horas), 
                              minutes = as.integer(t_efect_minutos))
  ) %>% 
  # Quitar cols. que contengan en el nombre "horas" o "minutos"
  select(-contains("horas"), -contains("minutos")) %>% 
  # Pasar a filas las columnas de tiempos
  pivot_longer(
    cols      = contains("Tiempo"),
    names_to  = "Tiempos",
    values_to = "Valor"
  ) %>% 
  # Separar la col. Valor en horas y minutos
  separate(Valor, into = c("Horas","Minutos"), sep = ":") %>% 
  # Crear columna con hora en formato decimal
  mutate(
    hora_decimal = as.numeric(Horas) + as.numeric(Minutos)/60
  ) %>% 
  # quitar cols. innecesarias
  select(-c(Horas, Minutos)) %>% 
  # Regresar a cols. separadas cada indicador
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  )

tiempos_os_tidy_tbl_long <- tiempos_os_tidy_tbl %>% 
  pivot_longer(
    cols      = contains("Tiempo"),
    names_to  = "Tiempos",
    values_to = "hora_decimal"
  )
  
tiempos_os_tidy_tbl_long %>% 
  filter(Ruta == "JALISCO",
         Tiempos %in% c("Tiempo efectivo en sitio", "Tiempo de respuesta")) %>% 
  ggplot(aes(x = fct_reorder(`Técnico de visita`, hora_decimal), 
             y = hora_decimal, fill = Tiempos))+
  geom_boxplot() +
  facet_grid(Ruta ~ Tiempos, scales = "free_y") + 
  theme(legend.position = "none") +
  coord_flip()


tiempos_os_tidy_tbl %>% 
  filter(Tiempos %in% c("Tiempo de respuesta")) %>% 
  ggplot(aes(x = fct_reorder(Ruta, hora_decimal), y = hora_decimal, fill = Tiempos))+
  geom_boxplot() +
  facet_wrap(~ Tiempos, scales = "free_x") + 
  theme(legend.position = "none") +
  coord_flip() +
  labs(y = "Horas",
       x = "")


tiempos_mensual_tsbl <- tiempos_os_tidy_tbl %>% 
  group_by(Ruta, Tiempos) %>% 
  summarise_by_time(
    .date_var    = Fecha_recepion,
    .by          = "month",
    hora_decimal = mean(hora_decimal)
  ) %>% 
  mutate(Fecha_recepion = as.character(Fecha_recepion) %>% 
           yearmonth()) %>% 
  as_tsibble(index = Fecha_recepion, 
             key = c(Ruta, Tiempos))

p <- tiempos_mensual_tsbl %>% 
  filter(Ruta %in% c("JALISCO", "NUEVO LEON")) %>% 
  autoplot(hora_decimal) +
  facet_wrap(~Tiempos, scales = "free_y") +
  theme(legend.position = "none")

plotly::ggplotly(p)

#.