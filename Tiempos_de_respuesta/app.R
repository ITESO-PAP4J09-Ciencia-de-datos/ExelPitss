
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

# DATOS -------------------------------------------------------------------

# COLORES
az_os <- rgb(0,0,51/255) # "#000033"
az_cl <- rgb(0,154/255,203/255) # "#009ACB"
negro <- rgb(51/255,51/255,51/255) # "#333333"
n_os <- rgb(226/255,120/255,49/255) # "#E27831"
n_cl <- rgb(248/255,148/255,56/255) # "#F89438"

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

# datos diarios
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
    fill_gaps(.full = TRUE, Tiempos = mean(Tiempos)) %>% 
    filter_index("2018-02-12" ~ "2021-03-10")

# datos semanales
datos_week_tsbl <- datos_tsbl %>% 
    group_by_key() %>% 
    index_by(Semana = yearweek(Recepcion)) %>% 
    summarise(Tiempos = mean(Tiempos), .groups = "drop")

# datos mensuales
datos_month_tsbl <- datos_tsbl %>% 
    group_by_key() %>% 
    index_by(Mes = yearmonth(Recepcion)) %>% 
    summarise(Tiempos = mean(Tiempos), .groups = "drop")

rutas <- datos %>%  
    distinct(Ruta) %>% 
    pull()

# frecuencias <- c("Diariamente","Semanalmente","Mensualmente")

modelos <- list(
  Media                     = MEAN(Tiempos),
  Ingenuo                   = NAIVE(Tiempos),
  `Ingenuo Estacional`      = SNAIVE(Tiempos),
  Drift                     = RW(Tiempos ~ drift()),
  `Suavización Exponencial` = ETS(Tiempos),
  Arima                     = ARIMA(Tiempos)

)

# crear una lista con todas las tsibbles
tsbls <- list(
  Diaria  = datos_tsbl,
  Semanal = datos_week_tsbl,
  Mensual = datos_month_tsbl
)

frecuencias <- names(tsbls)

train <- tsbls %>% 
  map(. %>% filter_index("2018-03-01" ~ "2020-03-10"))

# # training diarios
# tiempos_dtrain <- datos_tsbl %>% 
#     filter_index("2018-03-01" ~ "2020-03-10")
# 
# # training semanales
# tiempos_wtrain <- datos_week_tsbl %>% 
#     filter_index("2018-03-01" ~ "2020-03-10")
# 
# # training mensuales
# tiempos_mtrain <- datos_month_tsbl %>% 
#     filter_index("2018-03-01" ~ "2020-03-10")
# 
# trainings <- list(
#   Diaria  = tiempos_dtrain,
#   Semanal = tiempos_wtrain,
#   Mensual = tiempos_mtrain
# )

tiempos <- datos_tsbl %>% 
  distinct(SLA) %>% 
  pull()

colores <- c("#E27831","#009ACB","#EEB422","#00008B","#8B4500","#EE4000")

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

# tab: visualización ------------------------------------------------------
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
                                      choices = frecuencias,
                                      selected = "Mensual")#,
                         # actionButton("go1","Visualizar los datos",
                         #              icon = icon(name="eye"),
                         #              style = "color: #fff; background-color: #F89438; border-color: #E27831")
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel(
                                 "Gráfica",
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
                                 "Gráfica",
                                 tags$head(
                                     tags$style(type = "text/css", "a{color: #fff}")
                                 ),
                                 style = "color: #fff",
                                 plotOutput("grafica2") %>% withSpinner(color="#F89438")), 
                             tabPanel(
                                 "Análisis",
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
                                                selected   = "Suavización Exponencial",
                                                direction  = "vertical",
                                                individual = TRUE,
                                                checkIcon  = list(
                                                  yes      = tags$i(class = "fa fa-square",
                                                  style    = "color: #F89438"),  
                                                  no       = tags$i(class = "fa fa-square-o",
                                                  style    = "color: #F89438"))
                                                  )
                                 ),
                                 tableOutput(outputId = "reporte1") %>% withSpinner(color="#F89438"),
                                 verbatimTextOutput(outputId = "reporte2") %>% withSpinner(color="#F89438"),
                                 plotOutput(outputId = "reporte3") %>% withSpinner(color="#F89438")#,
                                 # verbatimTextOutput(outputId = "modelosprint") %>% withSpinner(color="#F89438"),
                                 # verbatimTextOutput(outputId = "fcprint") %>% withSpinner(color="#F89438")
                             )
                         )
                     )
                 )
        ),

# tab: pronósticos --------------------------------------------------------
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
                                     label = h4("Rango de pronóstico según la periodicidad escogida", style="color:#E27831; font-style:urw din italic; font-size:20px"),
                                     min = 1, max = 1000, value = 5)
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel(
                                 "Gráfica",
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
)



# SERVER ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

# fit ---------------------------------------------------------------------
  
  modelos_fit <- reactive({
    train %>% 
      map(
        . %>% filter(Ruta == input$ruta2) %>% 
          model(
            Media                     = MEAN(Tiempos),
            Ingenuo                   = NAIVE(Tiempos),
            `Ingenuo Estacional`      = SNAIVE(Tiempos),
            Drift                     = RW(Tiempos ~ drift()),
            `Suavización Exponencial` = ETS(Tiempos),
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

# tab - visualización -----------------------------------------------------

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

  # output$modelosprint <- renderPrint({
  #   modelos_fit()[[input$frecuencia2]]
  # })
  # 
  # output$fcprint <- renderPrint({
  #   modelos_fc()[[input$frecuencia2]]
  # })
  
  output$grafica2 <- renderPlot({
    modelos_fc()[[input$frecuencia2]] %>% 
      filter(.model %in% input$modelo2) %>% 
      autoplot(tsbls[[input$frecuencia2]], level = NULL) +
      facet_wrap(~ SLA, scales = "free_y") +
      ggtitle(paste("Pronósticos de",input$ruta2, tolower(input$frecuencia2))) +
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
      filter(SLA == input$sla2) %>% 
      select(input$modelresidual2) %>% 
      report()
  })
  
  output$reporte3 <- renderPlot({
    modelos_fit()[[input$frecuencia2]] %>% 
      filter(SLA == input$sla2) %>% 
      select(input$modelresidual2) %>% 
      gg_tsresiduals() +
      ggtitle(paste("Residuales modelo:", 
                    input$modelresidual2, "del", 
                    input$sla2, "de", input$ruta2, 
                    tolower(input$frecuencia2)))
  })

# tab - pronósticos -------------------------------------------------------

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
      filter(SLA    %in% input$tiempos3,
             .model %in% input$modelo3) %>% 
      autoplot(tsbls[[input$frecuencia3]], level = NULL) + 
      facet_wrap(~ SLA, scales = "free_y") +
      ggtitle(paste("Pronósticos SLA's de",input$ruta3,"a ",input$forecast3,"días")) +
      theme_minimal() +
      theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
            legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
      labs(x = input$frecuencia3) +
      scale_colour_manual("Modelos", values = colores)
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# plot_d <- reactive({
#   datos_tsbl %>%
#     filter(Ruta == as.name(input$ruta1)) %>%
#     autoplot(Tiempos) + #aes(color = SLA) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Tiempos de",input$ruta1, tolower(input$frecuencia1))) +
#     geom_vline(xintercept = as.Date("2020-03-10"), linetype = "longdash", size = 1) +
#     theme_minimal() +
#     theme(
#         legend.position = "none",
#         plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
#           ) + 
#     scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831"))
# })
#   
# plot_w <- reactive({
#   datos_week_tsbl %>%
#     filter(Ruta == as.name(input$ruta1)) %>%
#     autoplot(Tiempos) + #aes(color = SLA) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Tiempos de",input$ruta1, tolower(input$frecuencia1))) +
#     geom_vline(xintercept = as.Date("2020-03-10"), linetype = "longdash", size = 1) +
#     theme_minimal() +
#     theme(
#         legend.position = "none",
#         plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
#         ) + 
#     scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831"))
# })
#   
# plot_m <- reactive({
#   datos_month_tsbl %>%
#     filter(Ruta == as.name(input$ruta1)) %>%
#     autoplot(Tiempos) +# aes(color = SLA) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     geom_vline(xintercept = as.Date("2020-03-10"), linetype = "longdash", size = 1) +
#     ggtitle(paste("Tiempos de",input$ruta1, tolower(input$frecuencia1))) +
#     theme_minimal() +
#     theme(
#         legend.position = "none",
#         plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
#     ) + 
#     scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831")) 
# })

# output$grafica1 <- renderPlotly({
#   if (input$frecuencia1 == "Diariamente"){
#     plot_d()
#   } else if (input$frecuencia1 == "Semanalmente"){
#     plot_w()
#   } else {
#     plot_m()
#   }
# })

# diarios_fit <- reactive({
#   tiempos_dtrain %>% 
#     filter(Ruta == input$ruta2) %>%
#     model(
#       Media                     = MEAN(Tiempos),
#       Ingenuo                   = NAIVE(Tiempos),
#       `Ingenuo Estacional`      = SNAIVE(Tiempos),
#       Drift                     = RW(Tiempos ~ drift()),
#       `Suavización Exponencial` = ETS(Tiempos),
#       Arima                     = ARIMA(Tiempos)
#     )
# }) 

# diarios_fc <- reactive({
#   diarios_fit() %>% 
#     dplyr::select(input$modelo2)  %>% 
#     forecast(h = "1 year")
# })

# plot_dtrain_fc <- reactive({
#   diarios_fc() %>%
#     autoplot(datos_tsbl, level = NULL) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Pronósticos de",input$ruta1, tolower(input$frecuencia1))) +
#     theme_minimal() +
#     theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
#           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
#     labs(x = "Días") +
#     scale_colour_manual("Modelos", values = colores)
# })

# modelado semanal
# semanal_fit <- reactive({
#   tiempos_wtrain %>% 
#     filter(Ruta == input$ruta2) %>%
#     model(
#       Media = MEAN(Tiempos),
#       Ingenuo = NAIVE(Tiempos),
#       `Ingenuo Estacional` = SNAIVE(Tiempos),
#       Drift = RW(Tiempos ~ drift()),
#       `Suavización Exponencial` = ETS(Tiempos),
#       Arima = ARIMA(Tiempos)
#     )
# }) 
# 
# semanal_fc <- reactive({
#   semanal_fit() %>% 
#     dplyr::select(input$modelo2)  %>%
#     forecast(h = "1 year")
# })
# 
# plot_wtrain_fc <- reactive({
#   semanal_fc() %>% 
#     autoplot(datos_week_tsbl, level = NULL) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Pronósticos de",input$ruta1, tolower(input$frecuencia1))) +
#     theme_minimal() +
#     theme(plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
#           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
#     labs(x = "Semanas") +
#     scale_colour_manual("Modelos", values = colores) 
# }) 
# 
# # modelado mensual
# mensual_fit <- reactive({
#   tiempos_mtrain %>% 
#     filter(Ruta == input$ruta2) %>%
#     model(
#       Media = MEAN(Tiempos),
#       Ingenuo = NAIVE(Tiempos),
#       `Ingenuo Estacional` = SNAIVE(Tiempos),
#       Drift = RW(Tiempos ~ drift()),
#       `Suavización Exponencial` = ETS(Tiempos),
#       Arima = ARIMA(Tiempos)
#     )
# })
#   
# mensual_fc <- reactive({
#   mensual_fit() %>%
#     dplyr::select(input$modelo2)  %>%
#     forecast(h = "1 year")
# })
#   
# 
# plot_mtrain_fc <- reactive({
#   mensual_fc() %>% 
#     autoplot(datos_month_tsbl, level = NULL) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Pronósticos SLA's de",input$ruta2, tolower(input$frecuencia2))) +
#     labs(x = "Meses") +
#     theme_minimal() +
#     theme(plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
#           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1))+
#     scale_colour_manual("Modelos", values = colores)
# })

# output$grafica2 <- renderPlot({
#     if (input$frecuencia2 == "Diariamente"){
#         plot_wtrain_fc()
#     } else if (input$frecuencia2 == "Semanalmente"){
#         plot_wtrain_fc()
#     } else {
#         plot_mtrain_fc()
#     }
# })

# acc_daily <- reactive({
#   diarios_fc() %>% 
#     accuracy(datos_tsbl) %>%
#     dplyr::group_by(SLA) %>% 
#     slice_min(RMSE)
# })
# 
# acc_semanal <- reactive({
#   semanal_fc() %>% 
#     accuracy(datos_week_tsbl) %>%
#     dplyr::group_by(SLA) %>% 
#     slice_min(RMSE)
# })
# 
# acc_mensual <- reactive({
#   mensual_fc() %>% 
#     accuracy(datos_month_tsbl) %>%
#     dplyr::group_by(SLA) %>% 
#     slice_min(RMSE)
# })
# 
#   output$reporte1 <- renderTable({
#     if (input$frecuencia2 == "Diariamente"){
#       acc_daily()
#     } else if (input$frecuencia2 == "Semanalmente"){
#       acc_semanal()
#     } else {
#       acc_mensual()
#     } 
#   })

# diarios_residuals <- reactive({
#   diarios_fit() %>% 
#     filter(SLA == input$sla2) %>% 
#     select(input$modelresidual2) %>% 
#     gg_tsresiduals() + 
#     ggtitle(paste("Residuales modelo:",input$modelresidual2,"del",input$sla2,"de",input$ruta2, tolower(input$frecuencia2)))
# })
# 
# semanal_residuals <- reactive({
#   semanal_fit() %>% 
#     filter(SLA == input$sla2) %>% 
#     select(input$modelresidual2) %>% 
#     gg_tsresiduals() +
#     ggtitle(paste("Residuales modelo:",input$modelresidual2,"del",input$sla2,"de",input$ruta2, tolower(input$frecuencia2)))
# })
# 
# mensual_residuals <- reactive({
#   mensual_fit() %>% 
#     filter(SLA == input$sla2) %>% 
#     select(input$modelresidual2) %>% 
#     gg_tsresiduals() +
#     ggtitle(paste("Residuales modelo:",input$modelresidual2,"del",input$sla2,"de",input$ruta2, tolower(input$frecuencia2)))
# })
# 
# output$reporte2 <- renderPlot({
#   if (input$frecuencia2 == "Diariamente"){
#     diarios_residuals()
#   } else if (input$frecuencia2 == "Semanalmente"){
#     semanal_residuals()
#   } else {
#     mensual_residuals()
#   }
# })
# 
# # modelado diario
# diarios_fut_fit <- reactive({
#   datos_tsbl %>%  
#     filter(Ruta == input$ruta3) %>%
#     model(
#       Media = MEAN(Tiempos),
#       Ingenuo = NAIVE(Tiempos),
#       `Ingenuo Estacional` = SNAIVE(Tiempos),
#       Drift = RW(Tiempos ~ drift()),
#       `Suavización Exponencial` = ETS(Tiempos),
#       Arima = ARIMA(Tiempos)
#     )
# }) 

# diarios_fut_fc <- reactive({
#   diarios_fut_fit() %>% 
#     dplyr::select(input$modelo3)  %>% 
#     filter(SLA %in% input$tiempos3) %>% 
#     forecast(h = input$forecast3)
# })
# 
# plotd_fut_fc <- reactive({
#   diarios_fut_fc() %>%
#     autoplot(datos_tsbl, level = NULL) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Pronósticos SLA's de",input$ruta3,"a ",input$forecast3,"días")) +
#     theme_minimal() +
#     theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
#           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
#     labs(x = "Días") +
#     scale_colour_manual("Modelos", values = colores)
# })
# 
# # modelado semanal
# semanal_fut_fit <- reactive({
#   datos_week_tsbl %>%  
#     filter(Ruta == input$ruta3) %>%
#     model(
#       Media = MEAN(Tiempos),
#       Ingenuo = NAIVE(Tiempos),
#       `Ingenuo Estacional` = SNAIVE(Tiempos),
#       Drift = RW(Tiempos ~ drift()),
#       `Suavización Exponencial` = ETS(Tiempos),
#       Arima = ARIMA(Tiempos)
#     )
# }) 
# 
# semanal_fut_fc <- reactive({
#   semanal_fut_fit() %>% 
#     dplyr::select(input$modelo3)  %>% 
#     filter(SLA %in% input$tiempos3) %>% 
#     forecast(h = input$forecast3)
# })
# 
# plotw_fut_fc <- reactive({
#   semanal_fut_fc() %>%
#     autoplot(datos_week_tsbl, level = NULL) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Pronósticos SLA's de",input$ruta3,"a ",input$forecast3,"semanas")) +
#     theme_minimal() +
#     theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
#           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
#     labs(x = "Semanas") +
#     scale_colour_manual("Modelos", values = colores)
# })
# 
# # modelado mensual
# mensual_fut_fit <- reactive({
#   datos_month_tsbl %>%  
#     filter(Ruta == input$ruta3) %>%
#     model(
#       Media = MEAN(Tiempos),
#       Ingenuo = NAIVE(Tiempos),
#       `Ingenuo Estacional` = SNAIVE(Tiempos),
#       Drift = RW(Tiempos ~ drift()),
#       `Suavización Exponencial` = ETS(Tiempos),
#       Arima = ARIMA(Tiempos)
#     )
# }) 
# 
# mensual_fut_fc <- reactive({
#   mensual_fut_fit() %>% 
#     dplyr::select(input$modelo3) %>% 
#     filter(SLA %in% input$tiempos3) %>% 
#     forecast(h = input$forecast3)
# })
# 
# plotm_fut_fc <- reactive({
#   mensual_fut_fc() %>%
#     autoplot(datos_month_tsbl, level = NULL) +
#     facet_wrap(~ SLA, scales = "free_y") +
#     ggtitle(paste("Pronósticos SLA's de",input$ruta3,"a ",input$forecast3,"meses")) +
#     theme_minimal() +
#     theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
#           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
#     labs(x = "Meses") +
#     scale_colour_manual("Modelos", values = colores)
# })
# 
#   output$grafica3 <- renderPlot({
#     if (input$frecuencia3 == "Diariamente"){
#       plotd_fut_fc()
#     } else if (input$frecuencia3 == "Semanalmente"){
#       plotw_fut_fc()
#     } else {
#       plotm_fut_fc()
#     }
#   })