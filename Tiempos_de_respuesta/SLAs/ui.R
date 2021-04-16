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

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    title = "Modelos SLA's",
    shinyWidgets::setBackgroundImage(src = "fondo.png"),
    shinyjs::useShinyjs(),
    #Assign Dasbhoard title
    titlePanel(div("Modelos SLA's",style = "color:white; font-size: 70px; font-style:proxima nova; font-weight:bold; font-style:italic",align = "center",
                   img(height = 105, width = 400, src = "logo_blanco.png"))),
    navbarPage("Menu",
               
# tab: visualizacion ------------------------------------------------------
               tabPanel("Visualizacion de Datos",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("ruta1",
                                            label = h4("Ruta", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                            choices = rutas, 
                                            multiple = FALSE, 
                                            selected = "JALISCO"),
                                radioButtons("frecuencia1",
                                             label = h4("Frecuencia", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                             choices = frecuencias,
                                             selected = "Mensual")#,
                                # actionButton("go1","Visualizar los datos",
                                #              icon = icon(name="eye"),
                                #              style = "color: #fff; background-color: #F89438; border-color: #E27831")
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(
                                        "Grafica",
                                        tags$head(
                                            tags$style(type = "text/css", "a{color: #fff}")
                                        ),
                                        style = "color: #fff",
                                        plotlyOutput(outputId = "grafica1") %>% withSpinner(color="#F89438") )         
                                )
                            )
                        )
               ),
               
# tab: modelado -----------------------------------------------------------
               tabPanel("Modelado",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("ruta2",
                                            label = h4("Ruta", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                            choices = rutas, 
                                            multiple = FALSE, 
                                            selected = "JALISCO"),
                                radioButtons("frecuencia2",
                                             label = h4("Frecuencia", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                             choices = frecuencias,
                                             selected = "Mensual"),
                                selectInput("modelo2",
                                            label = h4("Modelo", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                            choices = names(modelos), 
                                            multiple = TRUE, 
                                            selected = names(modelos))#,
                                # actionButton("go2","Modelar",
                                #              icon = icon(name="tools"),
                                #              style = "color: #fff; background-color: #F89438; border-color: #E27831"),
                                # actionButton("error2","Obtener exactitud",
                                #              icon = icon(name="drafting-compass"),
                                #              style = "color: #fff; background-color: #F89438; border-color: #E27831")
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(
                                        "Grafica",
                                        tags$head(
                                            tags$style(type = "text/css", "a{color: #fff}")
                                        ),
                                        style = "color: #fff",
                                        plotOutput("grafica2") %>% withSpinner(color="#F89438")), 
                                    tabPanel(
                                        "Analisis",
                                        tags$head(
                                            tags$style(type = "text/css", "a{color: #fff}")
                                        ),
                                        style = "color: #fff",
                                        splitLayout(
                                            radioGroupButtons("sla2",
                                                              label      = h4("SLA", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                                              choices    = tiempos,
                                                              selected   = "TR",
                                                              direction  = "vertical",
                                                              individual = TRUE,
                                                              checkIcon  = list(
                                                                  yes      = tags$i(class = "fa fa-circle",
                                                                                    style    = "color: #F89438"),  
                                                                  no       = tags$i(class = "fa fa-circle-o",
                                                                                    style    = "color: #F89438"))
                                            ),
                                            radioGroupButtons("modelresidual2",
                                                              label      = h4("Modelo", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                                              choices    = names(modelos),
                                                              selected   = "Suavizacion Exponencial",
                                                              direction  = "vertical",
                                                              individual = TRUE,
                                                              checkIcon  = list(
                                                                  yes      = tags$i(class = "fa fa-square",
                                                                                    style    = "color: #F89438"),  
                                                                  no       = tags$i(class = "fa fa-square-o",
                                                                                    style    = "color: #F89438"))
                                            )
                                        ),
                                        verticalLayout(
                                            h4("Accuracy", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                            tableOutput(outputId = "reporte1") %>% withSpinner(color="#F89438"),
                                            h4("Reporte", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                            verbatimTextOutput(outputId = "reporte2") %>% withSpinner(color="#F89438"),
                                            h4("Residuales", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                            plotOutput(outputId = "reporte3") %>% withSpinner(color="#F89438")
                                        )
                                    )
                                )
                            )
                        )
                        
               ),
               
# tab: pronosticos --------------------------------------------------------
               tabPanel("Pronosticos",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("ruta3",
                                            label = h4("Ruta", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                            choices = rutas, 
                                            multiple = FALSE, 
                                            selected = "JALISCO"),
                                radioButtons("frecuencia3",
                                             label = h4("Frecuencia", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                             choices = frecuencias,
                                             selected = "Mensual"),
                                selectInput("modelo3",
                                            label = h4("Modelo", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                            choices = names(modelos), 
                                            multiple = TRUE, 
                                            selected = "Ingenuo Estacional"),
                                awesomeCheckboxGroup(
                                    inputId = "tiempos3",
                                    label = h4("SLA", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                    choices = tiempos,
                                    selected = tiempos,
                                    inline = TRUE, 
                                    status = "primary"
                                ),
                                sliderInput("forecast3",
                                            label = h4("Rango de pronostico según la periodicidad escogida", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                            min = 1, max = 1000, value = 5)
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel(
                                        "Grafica",
                                        tags$head(
                                            tags$style(type = "text/css", "a{color: #fff}")
                                        ),
                                        style = "color: #fff",
                                        plotOutput("grafica3") %>% withSpinner(color="#F89438"))
                                )
                            )
                        )
               )
               
    )
))
