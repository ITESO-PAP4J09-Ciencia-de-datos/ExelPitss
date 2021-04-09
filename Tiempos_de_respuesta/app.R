
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

# pronósticos
library(tsibble)
library(feasts)
library(fable)

# DATOS -------------------------------------------------------------------

# COLORES
az_os <- rgb(0,0,51/255)
az_cl <- rgb(0,154/255,203/255)
negro <- rgb(51/255,51/255,51/255)
n_os <- rgb(226/255,120/255,49/255)
n_cl <- rgb(248/255,148/255,56/255)

datos <- read_xlsx("tiempos_respuesta.xlsx")

#glimpse(datos)

datos <- datos %>% 
    mutate(across(contains("Tiempo"), as.numeric)) %>% 
    rename_all(funs(make.names(.))) %>% 
    rename(Orden = N...de.orden,
           Serie = N...de.serie,
           Equipo = N...de.equipo,
           Num.Cliente = N..de.cliente,
           Distribuidor = Grupo.empresarial,
           Nivel = Nivel.de.servicio,
           Recepcion = `Fecha.recepción`,
           Ejecutivo = Usuario.captura,
           IS.asignado = `Técnico.asignado`,
           IS.visita = `Técnico.de.visita`,
           Visitas = N...de.visitas,
           Cierre = Fecha.cierre,
           TST = Tiempo.transcurrido,
           TMO = Tiempo.efectivo.en.sitio,
           TR = Tiempo.de.respuesta,
           TLR = Limite.de.tiempo.de.respuesta,
           TLR.restante = `Tiempo.límite.restante...24`,
           TSP = Tiempo.efectivo,
           TLS = `Limite.tiempo.de.solución.total`,
           TLS.restante = `Tiempo.límite.restante...27`) %>% 
    mutate(Recepcion = as.Date(Recepcion),
           Cierre    = as.Date(Cierre))

datos_tsbl <- datos %>% filter(IS.visita != "Consultoria Dual",
                               Estatus   == "RESUELTA",
                               Categoría == "CORRECTIVO") %>% 
    group_by(Recepcion, Ruta) %>% 
    summarise(across(.cols = c(TR, TSP, TMO, TST), mean),
              .groups = "drop") %>% 
    pivot_longer(
        cols      = starts_with("T"),
        names_to  = "SLA", 
        values_to = "Tiempos"
    ) %>% 
    as_tsibble(
        # index : la variable temporal
        index = Recepcion,
        # key : la(s) variable(s) que identifican a cada serie de tiempo
        key   = c(Ruta, SLA) 
    ) %>% 
    fill_gaps(.full = TRUE, Tiempos = mean(Tiempos))

datos_week_tsbl <- datos_tsbl %>% 
    group_by_key() %>% 
    index_by(Semana = yearweek(Recepcion)) %>% 
    summarise(Tiempos = mean(Tiempos), .groups = "drop")

datos_month_tsbl <- datos_tsbl %>% 
    group_by_key() %>% 
    index_by(Mes = yearmonth(Recepcion)) %>% 
    summarise(Tiempos = mean(Tiempos), .groups = "drop")

rutas <- datos %>%  
    distinct(Ruta) %>% 
    pull()

frecuencias <- c("Diariamente","Semanalmente","Mensualmente")
modelos <- c("MEAN","NAÏVE","SEASONAL NAÏVE","DRIFT","ETS", "ARIMA")

# UI CODE ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    title = "Modelos SLA's",
    setBackgroundImage(src = "fondo.png"),
    useShinyjs(),
    #Assign Dasbhoard title
    titlePanel(div("Modelos SLA's",style = "color:white; font-size: 70px; font-style:proxima nova; font-weight:bold; font-style:italic",align = "center",
                   img(height = 105, width = 400, src = "logo_blanco.png"))),
    navbarPage("Menú",
        tabPanel("Visualización de Datos",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("ruta1",
                                     label = h4("Ruta", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                     choices = rutas, 
                                     multiple = FALSE, 
                                     selected = "JALISCO"),
                         radioButtons("frecuencia1",
                                      label = h4("Frecuencia", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                      choices = frecuencias),
                         actionButton("go1","Visualizar los datos",
                                      icon = icon(name="eye"),
                                      style = "color: #fff; background-color: #F89438; border-color: #E27831")
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel(
                                 "Gráfica",
                                 tags$head(
                                     tags$style(type = "text/css", "a{color: #fff}")
                                 ),
                                 style = "color: #fff",
                                 plotOutput(outputId = "grafica1"))         
                         )
                     )
                 )
                 ),
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
                                      choices = frecuencias),
                         selectInput("modelo2",
                                     label = h4("Modelo", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                     choices = modelos, 
                                     multiple = TRUE, 
                                     selected = modelos),
                         actionButton("go2","Modelar",
                                      icon = icon(name="tools"),
                                      style = "color: #fff; background-color: #F89438; border-color: #E27831"),
                         actionButton("error2","Obtener exactitud",
                                      icon = icon(name="drafting-compass"),
                                      style = "color: #fff; background-color: #F89438; border-color: #E27831")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel(
                                 "Gráfica",
                                 tags$head(
                                     tags$style(type = "text/css", "a{color: #fff}")
                                 ),
                                 style = "color: #fff",
                                 plotOutput("grafica2"),
                                 dataTableOutput("tabla2")), 
                             tabPanel(
                                 "Reportes",
                                 tags$head(
                                     tags$style(type = "text/css", "a{color: #fff}")
                                 ),
                                 style = "color: #fff",
                                 dataTableOutput(outputId = "reporte"))         
                             
                             
                         )
                     )
                     )
        ),
        tabPanel("Pronósticos",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("ruta3",
                                     label = h4("Ruta", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                     choices = rutas, 
                                     multiple = FALSE, 
                                     selected = "JALISCO"),
                         radioButtons("frecuencia3",
                                      label = h4("Frecuencia", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                      choices = frecuencias),
                         selectInput("modelo3",
                                     label = h4("Modelo", style="color:#E27831; font-style:urw din italic; font-size:20px"), 
                                     choices = modelos, 
                                     multiple = FALSE, 
                                     selected = "ETS"),
                         sliderInput("forecast3",
                                     label = h4("Rango de pronóstico según la periodicidad escogida", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                     min = 1, max = 1000, value = 5),
                         actionButton("go3","Visualizar pronósticos",
                                      icon = icon(name="chart-line"),
                                      style = "color: #fff; background-color: #F89438; border-color: #E27831")
                         
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel(
                                 "Gráfica",
                                 tags$head(
                                     tags$style(type = "text/css", "a{color: #fff}")
                                 ),
                                 style = "color: #fff",
                                 plotOutput("grafica3"))
                         )
                     )
                 )
        )
    
)
)


# SERVER ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

    # observer para mostrar la visualización de datos
    shiny::observeEvent(input$go1,{
            
            plot_d <- datos_tsbl %>%
                filter(Ruta == as.name(input$ruta1)) %>%
                autoplot(Tiempos) + #aes(color = SLA) +
                facet_wrap(~ SLA, scales = "free_y") +
                ggtitle(paste("Tiempos de",input$ruta1, tolower(input$frecuencia1))) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
                      ) + 
                scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831"))
            
            plot_w <- datos_week_tsbl %>%
                filter(Ruta == as.name(input$ruta1)) %>%
                autoplot(Tiempos) + #aes(color = SLA) +
                facet_wrap(~ SLA, scales = "free_y") +
                ggtitle(paste("Tiempos de",input$ruta1, tolower(input$frecuencia1))) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
                    ) + 
                scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831"))
            
            plot_m <- datos_month_tsbl %>%
                filter(Ruta == as.name(input$ruta1)) %>%
                autoplot(Tiempos) +# aes(color = SLA) +
                facet_wrap(~ SLA, scales = "free_y") +
                ggtitle(paste("Tiempos de",input$ruta1, tolower(input$frecuencia1))) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
                ) + 
                scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831"))
            if (input$frecuencia1 == "Diariamente"){
                output$grafica1 <- renderPlot({
                    plot_d
                })
            } else if (input$frecuencia1 == "Semanalmente"){
                output$grafica1 <- renderPlot({
                    plot_w
                })
            } else {
                output$grafica1 <- renderPlot({
                    plot_m
                })
            }
    })
    
    # observer para mostrar la visualización de datos
    #shiny::observeEvent(input$go2,{
        
        # modelado diario
        tiempos_dtrain <- datos_tsbl %>% 
            filter_index("2018-01-11" ~ "2020-03-10")
        tiempos_dtrain_fit <- tiempos_dtrain %>% 
            filter(Ruta == input$ruta2) %>% 
            model(Mean = MEAN(Tiempos),
                  `Naïve` = NAIVE(Tiempos),
                  `Seasonal naïve` = SNAIVE(Tiempos),
                  Drift = RW(Tiempos ~ drift()),
                  ETS = ETS(Tiempos),
                  ARIMA = ARIMA(Tiempos))
        tiempos_dtrain_fc <- tiempos_dtrain_fit %>% 
            forecast(h = "1 year")
        plot_dtrain_fc <- tiempos_dtrain_fc %>%
            filter(.model %in% input$modelo2) %>%
            autoplot(datos_tsbl, level = NULL) +
            facet_wrap(~ SLA, scales = "free_y") +
            theme_minimal() +
            labs(x = "Días",title = "Pronósticos SLA's") +
            guides(colour = guide_legend(title = "Pronóstico"))
        
        # modelado semanal
        tiempos_wtrain <- datos_week_tsbl %>% 
            filter_index("2018-01-11" ~ "2020-03-10")
        tiempos_wtrain_fit <- tiempos_wtrain %>% 
            filter(Ruta == input$ruta2) %>% 
            model(Mean = MEAN(Tiempos),
                  `Naïve` = NAIVE(Tiempos),
                  `Seasonal naïve` = SNAIVE(Tiempos),
                  Drift = RW(Tiempos ~ drift()),
                  ETS = ETS(Tiempos),
                  ARIMA = ARIMA(Tiempos))
        tiempos_wtrain_fc <- tiempos_wtrain_fit %>% 
            forecast(h = "1 year")
        plot_wtrain_fc <- tiempos_wtrain_fc %>% 
            filter(.model %in% input$modelo2) %>% 
            autoplot(datos_week_tsbl, level = NULL) +
            facet_wrap(~ SLA, scales = "free_y") +
            theme_minimal() +
            labs(x = "Semanas",title = "Pronósticos SLA's") +
            guides(colour = guide_legend(title = "Pronóstico"))
        
        # modelado mensual
        tiempos_mtrain <- datos_month_tsbl %>% 
            filter_index("2018-01-11" ~ "2020-03-10")
        tiempos_mtrain_fit <- tiempos_mtrain %>% 
            filter(Ruta == input$ruta2) %>% 
            model(Mean = MEAN(Tiempos),
                  `Naïve` = NAIVE(Tiempos),
                  `Seasonal naïve` = SNAIVE(Tiempos),
                  Drift = RW(Tiempos ~ drift()),
                  ETS = ETS(Tiempos),
                  ARIMA = ARIMA(Tiempos))
        tiempos_mtrain_fc <- tiempos_mtrain_fit %>% 
            forecast(h = "1 year")
        plot_mtrain_fc <- tiempos_mtrain_fc %>% 
            filter(.model %in% input$modelo2) %>% 
            autoplot(datos_month_tsbl, level = NULL) +
            facet_wrap(~ SLA, scales = "free_y") +
            theme_minimal() +
            labs(x = "Meses",title = "Pronósticos SLA's") +
            guides(colour = guide_legend(title = "Pronóstico"))
        
        # if para mostrar la correspondiente
        if (input$frecuencia2 == "Diariamente"){
            # output$reporte <- renderDataTable({
            #     tiempos_dtrain_fit %>%  
            #         report()
            # })
            output$grafica2 <- renderPlot({
                plot_wtrain_fc
            })
            
        } else if (input$frecuencia2 == "Semanalmente"){
            # output$reporte <- renderDataTable({
            #     tiempos_wtrain_fit %>%
            #         report()
            # })
            output$grafica2 <- renderPlot({
                plot_wtrain_fc
            })
        } else {
            # output$reporte <- renderDataTable({
            #     tiempos_mtrain_fit %>% 
            #         report()
            # })
            output$grafica2 <- renderPlot({
                plot_mtrain_fc
            })
        }
        
    #})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
