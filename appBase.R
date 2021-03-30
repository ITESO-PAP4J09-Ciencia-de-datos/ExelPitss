
# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(patchwork)

# funciones ---------------------------------------------------------------
source("Limpieza.R", 
       local = knitr::knit_global(),
       encoding = "utf-8")
# DATA --------------------------------------------------------------------
df <- tiempos_os_tidy_tbl

# listados(manufacturer)

estados <- df %>% 
    distinct(Ruta) %>% 
    pull()
modelo_I <- df %>% 
    distinct(Modelo) %>% 
    pull()
tecnico <- df %>% 
    distinct(`Técnico de visita`, Ruta) %>% 
    pull()

#names(traccion) <- c("Delantera","4x4","Trasera")

# UI ----------------------------------------------------------------------
# 
ui <- fluidPage(
    theme = shinytheme("darkly"),
    tabsetPanel(
      tabPanel(
          title = "Visualizacion de Datos",
              verticalLayout(
                  wellPanel(
                      selectInput(
                          inputId  = "Estado",
                          label    = "Escoge el Estado",
                          choices  = estados,
                          selected = FALSE,
                          width    = "150px" 
                        ),
                      
                      uiOutput(outputId = "tabla"),
                      
                      #selectInput(
                          #inputId   = "Tecnico",
                          #label     = "Selecciona al tecnico:",
                          #choices   = tecnico,
                          #selected  = tecnico,
                      #),
                  submitButton()
                  ), #wellPanel
                  
                  plotOutput(outputId = "grafica")
                  
              )
          
      )
  )
)


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  
    df_filtrada <- reactive({
      df %>%
          filter(Ruta == input$Estado)
    })
    
    
    output$tabla <- renderTable({
  })
    
    
    output$tabla <- renderUI({
        pickerInput(
            inputId  = "Tecnico",
            label    = "Escoge los tecnicos a nombrar",
            choices  = df_filtrada() %>% 
              distinct(`Técnico de visita`) %>% 
              pull(`Técnico de visita`),
            selected = FALSE,
            options  = list(
                `actions-box` = TRUE,
                size = 5,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE,
            width = "auto"
        )
    })
    
    output$grafica <- renderPlot({
      
    })
}
    shinyApp(ui, server)
