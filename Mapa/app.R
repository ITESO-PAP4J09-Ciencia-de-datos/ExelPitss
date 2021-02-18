
# LIBRER?AS ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(tidyquant)
library(stringr)
library(cellranger)
library(shiny)
library(sf)
library(ggplot2)
library(leaflet)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(ggplot2)


# DATOS -------------------------------------------------------------------

localidades <- read_xlsx("localidades.xlsx",col_names=c("is","zonas"),skip=1) %>%
    mutate(Apellidos = word(is,1,2)) %>%
    relocate(Apellidos, .after = is) %>% 
    mutate(Apellidos=ifelse(Apellidos =="DE LA",word(is,1,3),Apellidos),
           Apellidos=ifelse(Apellidos == "LIZARAN MACEDA",word(is,1,3),Apellidos))

# verificaci?n de 2 apellidos iguales
n_occur <- data.frame(table(localidades$Apellidos))
reps <- localidades[localidades$Apellidos %in% n_occur$Var1[n_occur$Freq > 1],]

datos <- read_xlsx("matriz_cursos.xlsx","Detalle de matriz",range = cell_cols("A:I")) %>%
    mutate(IS = gsub("[0-9]+", "", IS), IS = gsub("[[:punct:]]", "", IS)) %>%
    mutate(Apellidos = word(IS,-2,-1)) %>% 
    relocate(Apellidos, .after = IS) %>% 
    dplyr::select(everything(),-c("IS & Modelo","V"))

datos <- datos %>% 
    mutate(Apellidos=ifelse(datos$Apellidos =="DE LA",word(IS,1,3),datos$Apellidos),
           Apellidos=ifelse(datos$Apellidos == "LIZARAN MACEDA",paste(word(IS,-2,-1),word(IS,1)),datos$Apellidos))

datos <- left_join(datos,localidades, by="Apellidos")

datos <- datos %>% 
    drop_na(zonas) %>% 
    relocate(zonas, .after = IS) %>% 
    dplyr::select(everything(),-c("is","Apellidos"))

names(datos) <- c("IS","Zonas","Modelo","Marca","Virtual","Requerido","Mes","Certificaci?n")

marcas <- datos %>% 
    distinct(Marca) %>% 
    pull()

modelos <- datos %>% 
    distinct(Modelo) %>% 
    pull()

zonas <- datos %>% 
    distinct(Zonas) %>% 
    pull()

is <- datos %>% 
    distinct(IS) %>% 
    pull()

# UI CODE -----------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
    #Assign Dasbhoard title 
    titlePanel("Mapa cobertura ExelPitss"),
    sidebarPanel(
        selectInput(inputId  = "marcas",
                    label    = h4("Marca"),
                    choices  = marcas,
                    selected = "XEROX"
        ),
        uiOutput(outputId = "select_models")
        
    ),
    mainPanel( 
        #this will create a space for us to display our map
        leafletOutput(outputId = "Mexico"), 
        #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
        absolutePanel(top = 60, left = 20, 
                      checkboxInput("markers", "Depth", FALSE),
                      checkboxInput("heat", "Heatmap", FALSE)
        )
    ))


# SERVER ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    datos_fil <- reactive({
        datos %>% 
            filter(Marca == input$marcas)
    })
    
    modelos <- reactive({
        datos_fil() %>% 
            distinct(Modelo) %>% 
            pull()
    })
    
    output$select_models <- renderUI({
        selectInput(inputId  = "modelos",
                    label = h4("Modelo"),
                    choices = modelos(),
                    multiple = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)