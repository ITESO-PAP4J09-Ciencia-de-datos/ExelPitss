
# LIBRERIAS ---------------------------------------------------------------

library(utils)

# ciencia de datos
library(tidyverse)

# importación y lectura de datos
library(readxl)
library(rgdal)

# visualización del mapa
library(leaflet)
library(raster)
library(spData)
library(tmap)

# elementos de shiny 
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)

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
#fechascompletas <- seq(min(fechas), max(fechas), by="months")

# DATOS MEXICO ------------------------------------------------------------

# estados y municipios

# poner pin en cada municipio

# cuando seleccionen el pin, despliegue nombres de IS, o un resumen

# cuando seleccionen todo el estado

# que el mapa se vaya filtrando conforme las opciones que se van seleccionando



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
    useShinyjs(),
    #Assign Dasbhoard title 
    titlePanel(h1("Cobertura ExelPitss")),
    sidebarPanel(
                div(
            id = "form",
            uiOutput(outputId = "select_marcas"),
            uiOutput(outputId = "select_models"),
            uiOutput(outputId = "select_zonas"),
            uiOutput(outputId = "select_is"),
            uiOutput(outputId = "select_fechas")
        ),
        actionButton("reset_input",
                     label = "Reestablecer todo",
                     icon = icon( name = "eraser"),
                     style = "color: #fff; background-color: #D75453; border-color: #C73232"
                    )
        
    ),
    mainPanel(
        tabsetPanel(
            tabPanel(
                "MAPA",
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
                        )
                    )
                    # separate the box by a column
                    , column(
                        width = 9
                        , leaflet::leafletOutput( outputId = "myMap"
                                                  , height = 500,
                                                  width = 600
                        )
                    )
                )
            ),
            tabPanel(
                "TABLA",
                tableOutput(outputId = "tabla1")
                #código para devolver tabla
            )
            
        )
         # end of the box
     # end of fluid page
        
    )
)


# SERVER ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$reset_input, {
        reset("form")
    })
    
    # sin seleccionar nada
    datos_fil <- reactive({
        datos
    })
    
    # si no escogen ni modelo, ni zona, ni is
    datos_fil1 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas)
    })
    
    # si no escogen ni zona, ni is
    datos_fil2 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas) %>% 
            filter(Modelo %in% input$modelos)
    })
    
    # si no escogen IS
    datos_fil3 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas) %>% 
            filter(Modelo %in% input$modelos) %>% 
            filter(Zonas %in% input$zonas)
    })
      
    # si no escogen ni modelo, ni zonas
    datos_fil4 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas) %>%
            filter(IS %in% input$is)
    })
    
    # si no escogen ni modelo, ni is
    datos_fil5 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas) %>%
            filter(Zonas %in% input$zonas)
    })
    
    # si no escogen ni marca, ni is
    datos_fil6 <- reactive({
        datos %>% 
            filter(Modelo %in% input$modelos) %>%
            filter(Zonas %in% input$zonas)
    })
    
    # si no escogen ni marca, ni zonas
    datos_fil7 <- reactive({
        datos %>% 
            filter(Modelo %in% input$modelos) %>%
            filter(IS %in% input$is)
    })
    
    # si no escogen ni marca, ni zona, ni is
    datos_fil8 <- reactive({
        datos %>% 
            filter(Modelo %in% input$modelos)
    })
    
    # si no escogen ni marca, ni modelo, ni is
    datos_fil9 <- reactive({
        datos %>% 
            filter(Zonas %in% input$zonas)
    })
    
    # si no escogen ni marca, ni modelo, ni zonas
    datos_fil.10 <- reactive({
        datos %>% 
            filter(IS %in% input$is)
    })
    
    # si escogen todo
    datos_fil.11 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas) %>% 
            filter(Modelo %in% input$modelos) %>% 
            filter(Zonas %in% input$zonas) %>% 
            filter(IS %in% input$is)
    })
    
    # si no escogen marca
    datos_fil.12 <- reactive({
        datos %>% 
            filter(Modelo %in% input$modelos) %>% 
            filter(Zonas %in% input$zonas) %>% 
            filter(IS %in% input$is)
    })
    
    # si no escogen modelo
    datos_fil.13 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas) %>% 
            filter(Zonas %in% input$zonas) %>% 
            filter(IS %in% input$is)
    })
    
    # si no escogen zona
    datos_fil.14 <- reactive({
        datos %>% 
            filter(Marca %in% input$marcas) %>% 
            filter(Modelo %in% input$modelos) %>% 
            filter(IS %in% input$is)
    })
    
    # si no escogen ni marca, ni modelo
    datos_fil.15 <- reactive({
        datos %>% 
            filter(Zonas %in% input$zonas) %>% 
            filter(IS %in% input$is)
    })
      
    # OPCIONES MARCAS
    marcas <- reactive({
        datos_fil() %>% 
            distinct(Marca) %>% 
            pull()
    })
    
    output$select_marcas <- renderUI({
        selectInput(inputId  = "marcas",
                    label    = h4("Marca"),
                    choices  = marcas(),
                    #selected = "XEROX",
                    multiple = TRUE)
    })
    
    # OPCIONES MODELOS
    modelos <- reactive({
        datos_fil1() %>% 
            distinct(Modelo) %>% 
            pull() 
    })
    
    
    output$select_models <- renderUI({
        selectInput(inputId  = "modelos",
                    label = h4("Modelo"),
                    choices = modelos(),
                    multiple = TRUE)
    })
    
    # OPCIONES ZONAS
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
    
    # OPCIONES IS
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
        
    # OUTPUT MAPA MEXICO
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
            #addMarkers(lng = Long, lat = Lat, options = popupOptions(closeButton = FALSE))
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
    
    # OUTPUT TABLA
    output$tabla1 <- renderTable(width="400px",{
        if (is.null(input$marcas) & is.null(input$modelos) & is.null(input$zonas) & is.null(input$is)){
            datos_fil()
        }
        else if (is.null(input$modelos) & is.null(input$zonas) & is.null(input$is)){
            return(datos_fil1())
        }
        else if (is.null(input$zonas) & is.null(input$is)){
            return(datos_fil2())
        }
        else if(is.null(input$is)){
            return(datos_fil3())
        }
        else if (is.null(input$modelos) & is.null(input$zonas)){
            return(datos_fil4())
        }
        else if (is.null(input$modelos) & is.null(input$is)){
            return(datos_fil5())
        }
        else if (is.null(input$marcas) & is.null(input$is)){
            return(datos_fil6())
        }
        else if (is.null(input$marcas) & is.null(input$zonas)){
            return(datos_fil7())
        }
        else if(is.null(input$marcas) & is.null(input$zonas) & is.null(input$is)){
            return(datos_fil8())
        }
        else if (is.null(input$marcas) & is.null(input$modelos) & is.null(input$is)){
            return(datos_fil9())
        }
        else if (is.null(input$marcas) & is.null(input$modelos) & is.null(input$zonas)){
            return(datos_fil.10())
        }
        else if (is.null(input$marcas)){
            return(datos_fil.12())
        }
        else if (is.null(input$modelos)){
            return(datos_fil.13())
        }
        else if (is.null(input$zonas)){
            return(datos_fil.14())
        }
        else if (is.null(input$marcas) & is.null(input$modelos)) {
            return(datos_fil.15())
        }
        else {
            return(datos_fil.11())
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)