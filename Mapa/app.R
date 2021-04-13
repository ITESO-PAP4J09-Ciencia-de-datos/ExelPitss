
# LIBRERIAS ---------------------------------------------------------------

library(utils)

# ciencia de datos
library(tidyverse)

# importación y lectura de datos
library(readxl)
library(rgdal)
library(DT)

#visualización de texto
library(htmltools)

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
library(shinycssloaders)



# DATOS -------------------------------------------------------------------

# COLORES
az_os <- rgb(0,0,51/255)
az_cl <- rgb(0,154/255,203/255)
negro <- rgb(51/255,51/255,51/255)
n_os <- rgb(226/255,120/255,49/255)
n_cl <- rgb(248/255,148/255,56/255)

coordenadas <- read_xlsx("municipios_coord.xlsx","municipios",col_names=c("Zonas","estado","latitud","longitud"),skip=1)

localidades <- read_xlsx("localidades.xlsx",col_names=c("is","Zonas"),skip=1) %>%
    mutate(Apellidos = word(is,1,2)) %>%
    relocate(Apellidos, .after = is) %>% 
    mutate(Apellidos=ifelse(Apellidos =="DE LA",word(is,1,3),Apellidos),
           Apellidos=ifelse(Apellidos == "LIZARAN MACEDA",word(is,1,3),Apellidos))

localidades <- left_join(localidades,coordenadas, by = "Zonas") %>% 
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
    drop_na(Zonas) %>% 
    relocate(Zonas, .after = IS) %>% 
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
    title = "Cobertura Exel Pitss",
    setBackgroundImage(src="fondo.png"),
    useShinyjs(),
    #Assign Dasbhoard title
    titlePanel(div("COBERTURA",style = "color:white; font-size: 70px; font-style:proxima nova; font-weight:bold; font-style:italic",align = "center",
                   img(height = 105, width = 400, src = "exel pitss logo final_RGB-04.png"))),
    sidebarPanel(#alpha('white', 0.1),
                #tags$head(tags$style(".well {background-color: #fff;  opacity: 0.8;}")),
      width = 3,
                div(
            id = "form",
            style = "text-align:center; color: #FFFFFF24; fill:transparent", #rgb(0,0,51/255);",
            uiOutput(outputId = "select_marcas"),
            uiOutput(outputId = "select_models"),
            uiOutput(outputId = "select_zonas"),
            uiOutput(outputId = "select_is"),
            uiOutput(outputId = "select_fechas")
        ),
        actionButton(
                     #div(
                       "reset_input",
                       label = "Reestablecer todo",
                       icon = icon( name = "eraser"),
                       style = "color: #fff; background-color: #F89438; border-color: #E27831"
                    )
        
    ),
    mainPanel(
        tabsetPanel(
            tabPanel(
                "MAPA",style = "color: #fff",
                shinydashboard::box(
                    width = 12,
                    title = "MAPA",
                    
                    # separate the box by a column
                    column(width = 10, 
                        shiny::actionButton( inputId = "clearMap"
                                               , icon = icon( name = "eraser")
                                               , label = "Reestablecer mapa"
                                               , style = "color: #fff; background-color: #F89438; border-color: #E27831"
                        ),
                        leaflet::leafletOutput( outputId = "myMap",
                                                height = 400,
                                                width = 670
                        ) %>% withSpinner(color="#F89438")
                    ), 
                    column(width = 2,
                      div(style="text-align:center;
                          width:250px;
                          height:50px;
                          padding-top:50px;
                          position:relative;
                          color:white;
                          font-size: 20px; 
                          font-style: bold",
                      textOutput(outputId = "text")
                      ),
                      #style = "color: #fff",
                      div(style="text-align:center;
                          width:250px;
                          height:50px;
                          padding-top:50px;
                          position:relative;
                          color:white;
                          font-size: 15px",
                          tableOutput(outputId = "tabla_is")
                      )
                      
                    )
                    # , column(
                    #   width = 1,
                    #  
                    # )
                )
            ),
            tabPanel(
                "TABLA DE CURSOS",
                tags$head(
                  tags$style(type = "text/css", "a{color: #fff}")
                  ),
                style = "color: #fff",
                dataTableOutput(outputId = "tabla1")  %>% withSpinner(color="#F89438")# width = "120%")
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
    
    datos_filtrados <- reactive({
      datos %>%
        mutate(across(where(is.character),as_factor)) %>% 
      filter(
          Marca %in% input$marcas,
          Modelo %in% input$modelos,
          Zonas %in% input$zonas,
          IS %in% input$is
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
                    label    = h4("Marca", style="color:#70476F; font-style:urw din italic; font-size:20px"),
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
                    label = h4("Modelo", style="color:#70476F; font-style:urw din italic;font-size:20px"),
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
                    label = h4("Zona", style="color:#70476F; font-style:urw din italic; font-size:20px"),
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
                    label = h4("Ingeniero de Servicio", style="color:#70476F; font-style:urw din italic; font-size:20px"),
                    choices = is(),
                    options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                    multiple = TRUE,
                    selected = is()
                    )
    })
        
    #iconos normales
    icons <- awesomeIcons(
        icon = 'id-badge',
        iconColor = "#009ACB",
        markerColor = "black",
        library = 'fa'
    )
    
    #iconos seleccionados
    icons2 <- awesomeIcons(
        icon = 'id-badge',
        markerColor = "orange",
        iconColor = "#fff",
        library = 'fa',
        spin = TRUE
    )
    
    # función de nuestro mapa
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
        setView(lat = 22.1565,lng = -100.986,zoom=4.5)
    }
    
    # OUTPUT MAPA MEXICO.
    myMap_reval <- reactiveVal(foundational.map())
    output$myMap <- renderLeaflet({
        myMap_reval()
    }) 
    
    # To hold the selected map region id.
    click.list <- shiny::reactiveValues( ids = vector() )
    
    # observer para seleccionar el estado
    shiny::observeEvent(input$myMap_shape_click, ignoreNULL = T,ignoreInit = T, {
        
        # If already selected, first remove previous selection
        if(length(click.list)>0)
        {
            remove_id = click.list$ids
            lines.of.interest <- mexico[ which( mexico$state %in% remove_id) , ]
            markers.of.interest <- coordenadas[ which( coordenadas$estado %in% remove_id) , ]
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
        #markers.of.interest <- coordenadas[ which( coordenadas$estado %in% remove_id) , ]
        #print(click)
        if( is.null( click$id ) ){
            req( click$id )
        } else if( !click$id %in% lines.of.interest@data$id ){
            leaflet::leafletProxy( mapId = "myMap" ) %>%
                addPolylines( data = lines.of.interest
                              , layerId = lines.of.interest@data$id
                              , color = "#0571B0"
                              , weight = 5
                              , opacity = 1
                 )
        }
        
    }) # end of shiny::observeEvent({})
    
    # observer para ir filtrando los markers del mapa según el input
    observeEvent({
      input$marcas
      input$modelos
      input$zonas
      input$is},{
          #dplyr::select(Zonas)
      leafletProxy("myMap") %>%
          addAwesomeMarkers(
            lng = datos_filtrados()$Longitud,
            lat = datos_filtrados()$Latitud,
            layerId = datos$Zonas,
            icon = icons
          )
    })
    
    # observer para borrar los inputs
    observeEvent(input$reset_input, {
      reset("form")
    })
    
    #observer para desplegar tabla de IS en cada marker
    shiny::observe({
        click <- input$myMap_marker_click
        if (is.null(click)){
          output$text <- renderText({"Selecciona alguna zona"})
        } else {
        estado <- datos %>% filter(Latitud==click$lat,Longitud==click$lng) %>% distinct(Zonas)
        texto1 <- paste("Ingenieros en ",estado,":")
        tabla <- datos_filtrados() %>% filter(Latitud==click$lat,Longitud==click$lng) %>% distinct(IS)
        output$text <- renderText({texto1})
        output$tabla_is <- renderTable(tabla,spacing='xs',align='l',colnames=FALSE)
        }
    })
    
    # observer para cambiar el color del marker seleccionado
    shiny::observeEvent(input$myMap_marker_click, {
      click <- input$myMap_marker_click
      proxy <- leafletProxy("myMap")
      if(click$id == "Selected"){
        proxy %>% removeMarker(layerId = "Selected")
      }else{
        proxy %>% addAwesomeMarkers(lat = click$lat, lng = click$lng,icon = icons2, layerId = "Selected")
      }
    })
    
    # observer para el boton de borrar lo seleccionado del mapa
    shiny::observeEvent(input$clearMap, {
        click.list$ids <- NULL
        myMap_reval(foundational.map())
        
        proxy <- leafletProxy('myMap')
        if (input$clearMap){ 
          proxy %>% removeMarker(layerId = "Selected") %>% 
            addAwesomeMarkers(
              lng = datos_filtrados()$Longitud,
              lat = datos_filtrados()$Latitud,
              layerId = datos$Zonas,
              #options = popupOptions(closeButton = FALSE),
              label = ,
              icon = icons
            )
          output$text <- renderText({"Selecciona alguna zona"})
          tabla <- tibble(vacio=c(""))
          output$tabla_is <- renderTable(tabla,spacing='xs',align='l',colnames=FALSE)
        }
    }) 
    
    # tabla_final <- datos_filtrados() %>%
    #   dplyr::select(everything(), -c("Latitud","Longitud"))
   
    
    
    # OUTPUT TABLA
    output$tabla1 <- DT::renderDataTable(
      
      DT::datatable({datos_filtrados() %>% dplyr::select(everything(),-c("Latitud","Longitud"))},
                    rownames = FALSE,
                    options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000033', 'color': '#fff'});",
                        "}")
                    ),
                    filter=list(
                      position = "top"
                    )
      )
      )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)