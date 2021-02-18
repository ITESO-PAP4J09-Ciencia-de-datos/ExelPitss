library(tidyverse)
library(readxl)
library(tidyquant)
library(stringr)
library(cellranger)
library(sf)
library(ggplot2)
library(leaflet)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(ggplot2)
library(shiny)

localidades <- read_xlsx("localidades.xlsx",col_names=c("is","zonas"),skip=1) %>%
  mutate(Apellidos = word(is,1,2)) %>%
  relocate(Apellidos, .after = is) %>% 
  mutate(Apellidos=ifelse(Apellidos =="DE LA",word(is,1,3),Apellidos),
         Apellidos=ifelse(Apellidos == "LIZARAN MACEDA",word(is,1,3),Apellidos))

# verificación de 2 apellidos iguales
n_occur <- data.frame(table(localidades$Apellidos))
reps <- localidades[localidades$Apellidos %in% n_occur$Var1[n_occur$Freq > 1],]

datos <- read_xlsx("matriz_cursos.xlsx","Detalle de matriz",range = cell_cols("A:I")) %>%
  mutate(IS = gsub("[0-9]+", "", IS), IS = gsub("[[:punct:]]", "", IS)) %>%
  mutate(Apellidos = word(IS,-2,-1)) %>% 
  relocate(Apellidos, .after = IS) %>% 
  select(everything(),-c("IS & Modelo","V"))
  
datos <- datos %>% 
  mutate(Apellidos=ifelse(datos$Apellidos =="DE LA",word(IS,1,3),datos$Apellidos),
         Apellidos=ifelse(datos$Apellidos == "LIZARAN MACEDA",paste(word(IS,-2,-1),word(IS,1)),datos$Apellidos))

datos <- left_join(datos,localidades, by="Apellidos")

datos <- datos %>% 
  drop_na(zonas) %>% 
  relocate(zonas, .after = IS) %>% 
  select(everything(),-c("is","Apellidos"))

