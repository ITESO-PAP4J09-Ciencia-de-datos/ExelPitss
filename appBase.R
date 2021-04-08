
# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(patchwork)

# funciones ---------------------------------------------------------------
source("Modelos.R", 
       local = knitr::knit_global(),
       encoding = "utf-8")

# DATA --------------------------------------------------------------------
datos <- tiempos_os_tidy_tbl

# listados(manufacturer)

estados <- datos %>% 
    distinct(Ruta) %>% 
    pull()
modelo_I <- datos %>% 
    distinct(Modelo) %>% 
    pull()
tecnico <- datos %>% 
    distinct(`Técnico de visita`, Ruta) %>% 
    pull()


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
                      
                      # uiOutput(outputId = "tecnico"),
                      # uiOutput(outputId = "modelo"),
                      
                      #selectInput(
                          #inputId   = "Tecnico",
                          #label     = "Selecciona al tecnico:",
                          #choices   = tecnico,
                          #selected  = tecnico,
                      #),
                  # submitButton()
                   ), #wellPanel

                  plotOutput(outputId = "grafica")
                  
              )
          
      )
  )
)


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  
    # output$tecnico <- renderUI({
    #   pickerInput(
    #     inputId  = "Tecnico",
    #     label    = "Escoge los tecnicos a nombrar",
    #     choices  = df_filtrada() %>% 
    #       distinct(`Técnico de visita`) %>% 
    #       pull(`Técnico de visita`),
    #     selected = FALSE,
    #     options  = list(
    #       `actions-box` = TRUE,
    #       size = 5,
    #       `selected-text-format` = "count > 3"
    #     ),
    #     multiple = TRUE,
    #     width = "auto"
    #   )
    # })
    
    # modelos <- reactive({
    #   Train_tsb %>% 
    #     filter(Ruta == input$Estado)
    #   
    #   if (input$modelos == "Drift"){
    #     modelos() <- Train_tsb %>%
    #       model(Drift = RW(`Tiempo_de_respuesta`~drift())
    #       ) %>% 
    #       forecast(h = "1 month")
    #   }
    #   
    #   if (input$modelos == "Naïve"){
    #     modelos() <- Train_tsb %>%
    #       model(NAIVE(`Tiempo_de_respuesta`)
    #       ) %>% 
    #       forecast(h = "1 month")
    #   }
    #   
    #   if (input$modelos == "Seasonal Naïve"){
    #     modelos() <- Train_tsb %>%
    #       model(SNAIVE(`Tiempo_de_respuesta`)
    #       ) %>% 
    #       forecast(h = "1 month")
    #   }
    #   
    #   if (input$modelos == "Mean"){
    #     modelos() <- Train_tsb %>%
    #       model(MEAN(`Tiempo_de_respuesta`)
    #       ) %>% 
    #       forecast(h = "1 month")
    #   }
    #   
    # })
  
    # output$modelo <- renderUI({
    #   pickerInput(
    #     inputId  = "modelos",
    #     label    = "Escoge el modelo",
    #     choices  = md,
    #     selected = FALSE,
    #     options  = list(
    #       `actions-box` = TRUE,
    #       size = 5,
    #       `selected-text-format` = "count > 3"
    #     )
    #   )
    # })
    
    Modelos_fit <- reactive({
      Train_tsb %>%
        filter(Ruta == input$Estado, Fecha_recepion <= yearmonth("2020-04-01")) %>% 
        model(
          "ARIMA_BC" = ARIMA(box_cox(Tiempo_de_respuesta,lambda1))
        ) %>% 
        forecast(h = "5 month")
    })
    
    
    output$grafica <- renderPlot({
      Modelos_fit() %>% 
        autoplot(Train_tsb, level = NULL) +
        ggtitle("Entrenamiento") +
        xlab("Años") + ylab("horas") +
        guides(colour=guide_legend(title="Forecast")) +
        geom_vline(xintercept = as.Date("2020-05-01", color = "Red",
                                        linetype = "dashed"))+
        geom_vline( xintercept = as.Date("2020-9-01", color = "Red",
                                         linetype = "dashed"))+
        annotate("label", x = c(as.Date("2019-03-01"),as.Date("2020-07-01"),as.Date("2020-11-01")),
                 y = 3.5, label = c("Train set", "Test set","Validation set"),
                 color = c("black","blue","green"))
    })
}
    shinyApp(ui, server)
