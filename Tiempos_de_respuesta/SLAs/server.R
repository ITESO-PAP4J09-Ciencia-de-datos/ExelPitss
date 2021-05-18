# LIBRERIAS ---------------------------------------------------------------

# ciencia de datos
library(tidyverse)

# importación de datos
library(readxl)

# elementos de shiny 
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

# pronósticos
library(tsibble)
library(feasts)
library(fable)

# visualización de datos
library(plotly)


# SERVER ------------------------------------------------------------------


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

# fit ---------------------------------------------------------------------
    
    modelos_fit <- reactive({
        train %>% 
            map(
                . %>% dplyr::filter(Ruta == input$ruta2) %>% 
                    model(
                        Media                     = MEAN(Tiempos),
                        Ingenuo                   = NAIVE(Tiempos),
                        `Ingenuo Estacional`      = SNAIVE(Tiempos),
                        Drift                     = RW(Tiempos ~ drift()),
                        `Suavizacion Exponencial` = ETS(Tiempos),
                        Arima                     = ARIMA(Tiempos)
                    )
            )
    })
    
# forecast ----------------------------------------------------------------
    
    modelos_fc <- reactive({
        modelos_fit() %>% 
            map(
                . %>% forecast(h = "1 year")
            )
    })
    
# tab - visualizacion -----------------------------------------------------
    
    output$grafica1 <- renderPlotly({
        tsbls[[input$frecuencia1]] %>% 
            dplyr::filter(Ruta == as.name(input$ruta1)) %>%
            autoplot(Tiempos) + #aes(color = SLA) +
            facet_wrap(~ SLA, scales = "free_y") +
            ggtitle(paste("Tiempos de",input$ruta1, tolower(input$frecuencia1))) +
            geom_vline(xintercept = as.Date("2020-03-10"), linetype = "longdash", size = 1) +
            theme_minimal() +
            theme(
                legend.position = "none",
                plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
            ) +
            scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831"))
    })
    
# tab - modelado ----------------------------------------------------------
    
    
    output$grafica2 <- renderPlot({
        modelos_fc()[[input$frecuencia2]] %>% 
            dplyr::filter(.model %in% input$modelo2) %>% 
            autoplot(tsbls[[input$frecuencia2]], level = NULL, size = 1) +
            facet_wrap(~ SLA, scales = "free_y") +
            ggtitle(paste("Pronosticos de",input$ruta2, tolower(input$frecuencia2))) +
            theme_minimal() +
            theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
                  legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
            labs(x = input$frecuencia2) +
            scale_colour_manual("Modelos", values = colores)
    })
    
    output$reporte1 <- renderTable({
        modelos_fc()[[input$frecuencia2]] %>% 
            accuracy(tsbls[[input$frecuencia2]]) %>%
            dplyr::group_by(SLA) %>% 
            slice_min(RMSE)
    })
    
    output$reporte2 <- renderPrint({
        modelos_fit()[[input$frecuencia2]] %>% 
            dplyr::filter(SLA == input$sla2) %>% 
            select(input$modelresidual2) %>% 
            report()
    })
    
    output$reporte3 <- renderPlot({
        modelos_fit()[[input$frecuencia2]] %>% 
            dplyr::filter(SLA == input$sla2) %>% 
            select(input$modelresidual2) %>% 
            gg_tsresiduals() +
            ggtitle(paste("Residuales modelo:", 
                          input$modelresidual2, "del", 
                          input$sla2, "de", input$ruta2, 
                          tolower(input$frecuencia2)))
    })
    
# tab - pronosticos -------------------------------------------------------
    
    future_fit <- reactive({
        modelos_fit() %>% 
            map2(tsbls, ~ .x %>% refit(.y)
            )
    })
    
    future_fc <- reactive({
        future_fit() %>% 
            map(
                . %>% forecast(h = input$forecast3)
            )
    })
    
    output$grafica3 <- renderPlot({
        future_fc()[[input$frecuencia3]] %>% 
            dplyr::filter(SLA    %in% input$tiempos3,
                          .model %in% input$modelo3) %>% 
            autoplot(tsbls[[input$frecuencia3]], level = NULL) + 
            facet_wrap(~ SLA, scales = "free_y") +
            ggtitle(paste("Pronosticos SLA's de",input$ruta3,"a ",input$forecast3,"dias")) +
            theme_minimal() +
            theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
                  legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
            labs(x = input$frecuencia3) +
            scale_colour_manual("Modelos", values = colores)
    })
    
    

})
