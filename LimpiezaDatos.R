library(tidyverse)
library(readxl)
library(tidyquant)
library(stringr)
library(cellranger)
library(sf)
library(ggplot2)
library(leaflet)


localidades <- read_xlsx("localidades.xlsx",col_names=c("is","zonas"),skip=1) %>%
  mutate(Apellidos = word(is,1,2)) %>%
  relocate(Apellidos, .after = is)  

datos <- read_xlsx("matriz_cursos.xlsx","Detalle de matriz",
                   range = cell_cols("A:I"),
)

datos <- as_tibble(datos) %>%
  mutate(IS = gsub("[0-9]+", "", IS), IS = gsub("[[:punct:]]", "", IS)) %>%
  mutate(Apellidos = word(IS,-2,-1)) %>% 
  relocate(Apellidos, .after = IS) %>% 
  select(everything(),-c("IS & Modelo","V"))# %>% 
  #mutate(Zona = VLOOKUP(Apellidos,localidades,apellidos,zonas))
  
datos <- left_join(datos,localidades, by="Apellidos")

datos <- datos %>% 
  select(everything(),-is) %>% 
  relocate(zonas, .after = IS) %>% 
  drop_na(zonas) %>% 
  select(everything(),-Apellidos)




 # mutate(datos, V = VLOOKUP(IS,localidades,IS,zonas))

#MAPA

# carga del plano base
#leaflet()  %>%  addTiles() 

names(datos)

leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = datos,
             label = datos$zonas,
             )
