
# LIBRERIAS ---------------------------------------------------------------

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
library(bslib)
library(shinydashboard)
library(rgdal)
library(shinythemes)
library(shinyWidgets)

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

names(datos) <- c("IS","Zonas","Modelo","Marca","Virtual","Requerido","Fecha","Certificacion")

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

fechas <- datos %>%
    drop_na(Fecha) %>% 
    distinct(Fecha) %>% 
    pull()

fechas <- format(as.Date(fechas), "%Y-%m")
fechascompletas <- seq(min(fechas), max(fechas), by="months")

# DATOS MAPA --------------------------------------------------------------

tmp <- tempdir()
url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)
mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")

pal <- colorQuantile("YlGn", NULL, n = 5)
state_popup <- paste0("<strong>Estado: </strong>", 
                      mexico$name, 
                      "<br><strong>PIB per c?pita, miles de pesos, 2008: </strong>", 
                      mexico$gdp08)
# UI CODE -----------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("superhero"),
    #Assign Dasbhoard title 
    titlePanel(h1("Cobertura ExelPitss")),
    sidebarPanel(
        selectInput(inputId  = "marcas",
                    label    = h4("Marca"),
                    choices  = marcas,
                    selected = "XEROX"
        ),
        uiOutput(outputId = "select_models"),
        uiOutput(outputId = "select_zonas"),
        uiOutput(outputId = "select_is"),
        uiOutput(outputId = "select_fechas"),
        shiny::actionButton( inputId = "clearOptions"
                             , icon = icon( name = "eraser")
                             , label = "Reestablecer todo"
                             , style = "color: #fff; background-color: #D75453; border-color: #C73232"
        )
        
    ),
    mainPanel( 
        shinydashboard::box(
            width = 12
            , title = "Mapa"
            # separate the box by a column
            , column(
                width = 10
                , shiny::actionButton( inputId = "clearHighlight"
                                       , icon = icon( name = "eraser")
                                       , label = "Reestablecer mapa"
                                       , style = "color: #fff; background-color: #D75453; border-color: #C73232"
                ),
            )
            # separate the box by a column
            , column(
                width = 9
                , leaflet::leafletOutput( outputId = "myMap"
                                          , height = 500,
                                          width = 600
                )
            )
        ) # end of the box
     # end of fluid page
        
    )
)


# SERVER ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    initialInputs <- isolate(reactiveValuesToList(input))
    
    observe({
        # OPTIONAL - save initial values of dynamic inputs
        inputValues <- reactiveValuesToList(input)
        initialInputs <<- utils::modifyList(inputValues, initialInputs)
    })
    
    observeEvent(input$clearOptions, {
        for (id in names(initialInputs)) {
            value <- initialInputs[[id]]
            # For empty checkboxGroupInputs
            if (is.null(value)) value <- ""
            session$sendInputMessage(id, list(value = value))
        }
    })
    
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
    
    datos_fil2 <- reactive({
        datos %>% 
            filter(Marca == input$marcas) %>% 
            filter(Modelo %in% input$modelos)
    })
    
    zonas <- reactive({
        datos_fil2() %>% 
        distinct(Zonas) %>% 
        pull()
    })
    
    output$select_zonas <- renderUI({
        selectInput(inputId  = "zonas",
                    label = h4("Zona"),
                    choices = zonas(),
                    multiple = TRUE)
    })
    
    datos_fil3 <- reactive({
        datos %>% 
            filter(Marca == input$marcas) %>% 
            filter(Modelo %in% input$modelos) %>% 
            filter(Zonas %in% input$zonas)
    })
    
    is <- reactive({
        datos_fil3() %>% 
        distinct(IS) %>% 
        pull()
    })
    
    output$select_is <- renderUI({
        selectInput(inputId  = "is",
                    label = h4("Ingeniero de Servicio"),
                    choices = is(),
                    multiple = TRUE)
    })
        
    datos_fil4 <- reactive({
        datos %>% 
            filter(Marca == input$marcas) %>% 
            filter(Modelo %in% input$modelos) %>% 
            filter(Zonas %in% input$zonas) %>% 
            filter(Fechas %in% input$fechas)
    })
        
    fechas <- reactive({
        datos_fil4() %>% 
        distinct(Fechas) %>% 
        pull()
    })    
        
    output$select_fechas <- renderUI({
        airMonthpickerInput(
            inputId    = "fechas",
            label      = "Selecciona la fecha:",
            multiple   = TRUE,
            disabledDates = fechasbien,
            view       = "months",
            language   = "es",
            clearButton = TRUE,
            autoClose  = TRUE
        )
    })    
        
    foundational.map <- function(){
        leaflet() %>%
            #addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
            #setView( lng = -87.567215
            #         , lat = 41.822582
            #         , zoom = 11 ) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons( data = mexico
                         , fillOpacity = 0
                         , opacity = 0.2
                         , color = "#000000"
                         , weight = 2
                         , layerId = mexico$state
                         , group = "click.list")
    }
    
    # reactiveVal for the map object, and corresponding output object.
    myMap_reval <- reactiveVal(foundational.map())
    output$myMap <- renderLeaflet({
        myMap_reval()
    }) 
    
    # To hold the selected map region id.
    click.list <- shiny::reactiveValues( ids = vector() )
    
    shiny::observeEvent( input$myMap_shape_click, ignoreNULL = T,ignoreInit = T, {
        
        # If already selected, first remove previous selection
        if(length(click.list)>0)
        {
            remove_id = click.list$ids
            lines.of.interest <- mexico[ which( mexico$state %in% remove_id) , ]
            leaflet::leafletProxy( mapId = "myMap" ) %>%
                addPolylines( data = lines.of.interest
                              , layerId = lines.of.interest@data$id
                              , color = "#000000"
                              , weight = 2
                              , opacity = 0.2)
        }
        
        # add current selection
        click <- input$myMap_shape_click
        click.list$ids <- click$id  # we only store the last click now!
        lines.of.interest <- mexico[ which( mexico$state %in% click.list$ids ) , ]
        print(click)
        if( is.null( click$id ) ){
            req( click$id )
        } else if( !click$id %in% lines.of.interest@data$id ){
            leaflet::leafletProxy( mapId = "myMap" ) %>%
                addPolylines( data = lines.of.interest
                              , layerId = lines.of.interest@data$id
                              , color = "#6cb5bc"
                              , weight = 5
                              , opacity = 1
                ) 
        }
        
    }) # end of shiny::observeEvent({})
    
    # oberver for the clearHighlight button.
    shiny::observeEvent( input$clearOptions, {
        click.list$ids <- NULL
        myMap_reval(foundational.map()) # reset options.
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)