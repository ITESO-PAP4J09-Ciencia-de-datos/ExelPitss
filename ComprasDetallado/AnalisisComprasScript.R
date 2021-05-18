

# Libraries ---------------------------------------------------------------
# importación y lectura de datos
library(readxl)
library(rgdal)
library(DT)

# visualización de datos
library(ggplot2)
library(data.table)

#tbd
library(Hmisc)
library(skimr)
library(dplyr)

#mutate across
#Compra va a inventario
# Data --------------------------------------------------------------------
#Load Raw Data
data <- read.csv('ComprasDetallado.csv', header = TRUE)
colnames(data)<-c('Proveedor','Categoria_Proveedor','Empresa','Sucursal','Fecha','Tipo_documento','Folio',
                  'Recepcion','Almacen','Referencia_orden_compra','Referencia_recepcion_compra','Clave_producto',
                  'Serial','Categoria','Notas','Nombre_producto','Cantidad_ordenada','Cantidad_recibida',
                  'Unidad_de_medida','Precio_unitario_por_moneda','Subtotal_por_moneda','Moneda','Tipo_Cambio',
                  'Precio_unitario_MXN','Subtotal_MXN','Total_MXN')


data %>% distinct(Categoria_Proveedor)

#Grop by and perform skim
Cantidad <- data$Cantidad_ordenada
data <- data %>% mutate(across(c()))


data$Total_MXN <- as.numeric(gsub('[$,]','',data$Total_MXN))
data$Precio_unitario_por_moneda <- as.numeric(gsub('[$,]','',data$Precio_unitario_por_moneda))
data$Precio_unitario_MXN <- as.numeric(gsub('[$,]','',data$Precio_unitario_MXN))
data$Subtotal_por_moneda <- as.numeric(gsub('[$,]','',data$Subtotal_por_moneda))
data$Subtotal_MXN <- as.numeric(gsub('[$,]','',data$Subtotal_MXN))

data$Referencia_orden_compra <- NULL
data$Referencia_recepcion_compra <- NULL
data$Notas <- NULL
data$Serial <- NULL
#Describe raw data
summary(data$Total_MXN)
summary(data$Precio_unitario_MXN)
# Subsection --------------------------------------------------------------













































