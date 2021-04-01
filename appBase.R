
# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(patchwork)

# funciones ---------------------------------------------------------------
source("limpieza_reporte_Tiempo_respuesta.R", 
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
md <- c("Drift","Naïve", "Seasonal Naïve", "Mean")
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
                      
                      uiOutput(outputId = "tecnico"),
                      uiOutput(outputId = "modelo"),
                      
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
    
    output$tecnico <- renderUI({
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
    
    modelos <- reactive({
      Train_tsb %>% 
        filter(Ruta == input$Estado)
      
      if (input$modelos == "Drift"){
        modelos() <- Train_tsb %>%
          model(Drift = RW(`Tiempo_de_respuesta`~drift())
          ) %>% 
          forecast(h = "1 month")
      }
      
      if (input$modelos == "Naïve"){
        modelos() <- Train_tsb %>%
          model(NAIVE(`Tiempo_de_respuesta`)
          ) %>% 
          forecast(h = "1 month")
      }
      
      if (input$modelos == "Seasonal Naïve"){
        modelos() <- Train_tsb %>%
          model(SNAIVE(`Tiempo_de_respuesta`)
          ) %>% 
          forecast(h = "1 month")
      }
      
      if (input$modelos == "Mean"){
        modelos() <- Train_tsb %>%
          model(MEAN(`Tiempo_de_respuesta`)
          ) %>% 
          forecast(h = "1 month")
      }
      
    })
    
  
    
    output$modelo <- renderUI({
      pickerInput(
        inputId  = "modelos",
        label    = "Escoge el modelo",
        choices  = md,
        selected = FALSE,
        options  = list(
          `actions-box` = TRUE,
          size = 5,
          `selected-text-format` = "count > 3"
        )
      )
    })
    
    output$grafica <- renderPlot({
      autoplot(modelos())+
        ggtitle("Modelo")
    })
}
    shinyApp(ui, server)
