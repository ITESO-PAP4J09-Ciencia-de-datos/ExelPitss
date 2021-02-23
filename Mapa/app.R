
# LIBRERIAS ---------------------------------------------------------------

library(utils)

# ciencia de datos
library(tidyverse)

# importación y lectura de datos
library(readxl)
library(rgdal)

# visualización del mapa
library(leaflet)
library(ggmap) # -> para obtener lon y lat de los municipios
library(raster)
library(spData)
library(tmap)
library(RJSONIO)

# elementos de shiny 
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)

# DATOS -------------------------------------------------------------------

coordenadas <- read_xlsx("municipios_coord.xlsx","municipios",col_names=c("zonas","estado","latitud","longitud"),skip=1)

localidades <- read_xlsx("localidades.xlsx",col_names=c("is","zonas"),skip=1) %>%
    mutate(Apellidos = word(is,1,2)) %>%
    relocate(Apellidos, .after = is) %>% 
    mutate(Apellidos=ifelse(Apellidos =="DE LA",word(is,1,3),Apellidos),
           Apellidos=ifelse(Apellidos == "LIZARAN MACEDA",word(is,1,3),Apellidos))

localidades <- left_join(localidades,coordenadas, by = "zonas") %>% 
  dplyr::select(everything(), -c("estado"))

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

names(datos) <- c("IS","Zonas","Modelo","Marca","Virtual","Requerido","Fecha","Certificacion","Latitud","Longitud")

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

#fechas <- format(as.Date(fechas), "%Y-%m")
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
                        , leaflet::leafletOutput( outputId = "myMap",
                                                  height = 500,
                                                  width = 600
                        )
                    )
                    , column(
                      width = 3,
                      tableOutput(outputId = "tabla_is")
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
    
    datos_filtrados <- reactive({
      datos %>% 
      filter(
          Marca %in% input$marcas,
          Modelo %in% input$modelos,
          Zonas %in% input$zonas,
          IS %in% input$is,
        )
    })
  
    # OPCIONES MARCAS
    marcas <- reactive({
        datos %>% 
            distinct(Marca) %>% 
            pull()
    })
    
    output$select_marcas <- renderUI({
        pickerInput(inputId  = "marcas",
                    label    = h4("Marca"),
                    choices  = marcas(),
                    options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                    multiple = TRUE,
                    selected = marcas())
    })
    
    # OPCIONES MODELOS
    modelos <- reactive({
        datos %>%
          filter(Marca %in% input$marcas) %>% 
          distinct(Modelo) %>% 
          pull() 
    })
    
    
    output$select_models <- renderUI({
        pickerInput(inputId  = "modelos",
                    label = h4("Modelo"),
                    choices = modelos(),
                    options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                    multiple = TRUE,
                    selected = modelos())
    })
    
    # OPCIONES ZONAS
    zonas <- reactive({
      datos %>%
        filter(Marca %in% input$marcas, Modelo %in% input$modelos) %>%
        distinct(Zonas) %>% 
        pull() 
    })
    
    output$select_zonas <- renderUI({
        pickerInput(inputId  = "zonas",
                    label = h4("Zona"),
                    choices = zonas(),
                    options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                    multiple = TRUE,
                    selected = zonas()
                    )
    })
    
    # OPCIONES IS
    is <- reactive({
        datos %>% 
          filter(Marca %in% input$marcas, Modelo %in% input$modelos, Zonas %in% input$zonas) %>%
          distinct(IS) %>% 
          pull()
    })
    
    output$select_is <- renderUI({
        pickerInput(inputId  = "is",
                    label = h4("Ingeniero de Servicio"),
                    choices = is(),
                    options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                    multiple = TRUE,
                    selected = is()
                    )
    })
        
    # OUTPUT MAPA MEXICO
    
    #iconos normales
    icons <- awesomeIcons(
        icon = 'bolt',
        iconColor = 'green',
        markerColor = "black",
        library = 'fa'
    )
    #iconos seleccionados
    icons2 <- makeAwesomeIcon(
        icon = 'flag',
        markerColor = 'black',
        iconColor = 'green',
        library = 'fa'
    )
    
    #popup
    pop <- reactive({
      datos %>% 
        filter(Zonas == input$myMap_marker$id) %>% 
        select(IS)
    })
      
    
    
    foundational.map <- function(){
        leaflet() %>%
            #addTiles() %>%
            #addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag)) %>% 
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
                         , group = "click.list") %>% 
            addAwesomeMarkers(
              lng = localidades$longitud,
              lat = localidades$latitud,
              layerId = localidades$zonas,
              options = popupOptions(closeButton = FALSE),
              label = localidades$zonas,
              #popup = paste("Ingenieros: ", pop()),
              icon = icons)
            # addPopups(lng = localidades$longitud, lat = localidades$latitud, localidades,
            #           options = popupOptions(closeButton = FALSE))
    }
    
    data <- reactiveVal()
    
    # reactiveVal for the map object, and corresponding output object.
    myMap_reval <- reactiveVal(foundational.map())
    output$myMap <- renderLeaflet({
        myMap_reval()
    }) 
    
    output$tabla_is <- renderTable({
      if (is.null(input$clickedMarker))
        return()
      else {
         localidades %>% 
              filter(zonas == input$clickedMarker) %>% 
              select(is)
      }
         })
    # observeEvent(input$marcas, {
    #   leafletProxy("myMap") %>%
    #     clearGroup("myMarkers") %>%
    #     addMarkers(data = datos[datos$Marca %in% input$marcas, ],
    #                lng = localidades$longitud,
    #                lat = localidades$latitud)
    # })
    
    
    # cuando le des clic al marker aparezca una tabla con los nombres de los IS y que cambie el marker seleccionado
    # observeEvent(input$myMap_marker_click,{
    #   
    #   data$clickedMarker = input$myMap_marker_click
    #   leafletProxy('myMap') %>%
    #     addAwesomeMarkers(#popup=as.character(row_selected$mag),
    #                       layerId = as.character(data$clickedMarker$id),
    #                       lng = data()$long,
    #                       lat = data()$lat,
    #                       icon = icons2)
    #   
    #   # Reset previously selected marker
    #   if(!is.null(data()))
    #   {
    #     proxy %>%
    #       addMarkers(#popup = as.character(data()$mag), 
    #                  layerId = as.character(data()$id),
    #                  lng = data()$long, 
    #                  lat = data()$lat)
    #   }
    #   # set new value to reactiveVal 
    #   data(data$clickedMarker)
    # 
    # 
    #   #myMap_reval$showPopup(localidades$is[localidades$zonas == event])
    #   
    #   
    #   
    # })
    
   
    # To hold the selected map region id.
    click.list <- shiny::reactiveValues( ids = vector() )
    
    shiny::observeEvent(input$myMap_shape_click, ignoreNULL = T,ignoreInit = T, {
        
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
        datos_filtrados()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)