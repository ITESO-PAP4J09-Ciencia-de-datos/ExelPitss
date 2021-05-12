
# LIBRERIAS ---------------------------------------------------------------

library(utils)

# ciencia de datos
library(tidyverse)

# importación de datos
library(readxl)
library(rgdal)
library(DT)

#visualización de texto
library(htmltools)

# elementos de shiny 
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinymanager)
library(shinyalert)

# visualización del mapa
library(leaflet)
library(ggmap) # -> para obtener lon y lat de los municipios
library(raster)
library(spData)
library(tmap)
library(RJSONIO)

# pronósticos
library(tsibble)
library(feasts)
library(fable)

# visualización de datos
library(plotly)

# COLORES -----------------------------------------------------------------

az_os <- rgb(0,0,51/255) # "#000033"
az_cl <- rgb(0,154/255,203/255) # "#009ACB"
negro <- rgb(51/255,51/255,51/255) # "#333333"
n_os <- rgb(226/255,120/255,49/255) # "#E27831"
n_cl <- rgb(248/255,148/255,56/255) # "#F89438"

# DATOS SLA'S-------------------------------------------------------------------

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

# frecuencias <- c("Diariamente","Semanalmente","Mensualmente")

modelos <- list(
  #Media                     = MEAN(Tiempos),
  #Ingenuo                   = NAIVE(Tiempos),
  SNAIVE              = SNAIVE(Tiempos),
  #Drift                     = RW(Tiempos ~ drift()),
  ETS                 = ETS(Tiempos),
  #Arima                     = ARIMA(Tiempos),
  `STLF ARIMA`        = decomposition_model(
    STL(Tiempos ~ trend(window = 13) +season(window = "periodic"),robust = TRUE), ARIMA(season_adjust)),
  `STLF SNAIVE`       = decomposition_model(
    STL(Tiempos ~ trend(window = 13) +season(window = "periodic"),robust = TRUE), SNAIVE(season_adjust)),
  `STLF ETS & SNAIVE` = decomposition_model(
    STL(Tiempos ~ trend(window = 13) +season(window = "periodic"),robust = TRUE), ETS(season_adjust), SNAIVE(season_year))
)

colores <- c("#E27831","#009ACB","#EEB422","#00008B","#8B4500","#EE4000")

tiempos <- c("TR","TST","TSP","TMO")

users <- read_csv("users.csv")

opciones_user <- users %>% 
  filter(user != "admin") %>% 
  select(user)

logos <- tibble(
  users = users %>% dplyr::distinct(user) %>% pull(),
  logos = paste(users,".png")
) %>% 
  mutate(logos = gsub(" ", "", logos))


# DATOS COBERTURA ---------------------------------------------------------

coordenadas <- read_xlsx("municipios_coord.xlsx","municipios", col_names=c("Zonas","estado","latitud","longitud"),skip=1)

localidades <- read_xlsx("localidades.xlsx", col_names=c("is","Zonas"), skip=1) %>%
  dplyr::mutate(Apellidos = word(is,1,2)) %>%
  relocate(Apellidos, .after = is) %>%
  dplyr::mutate(Apellidos = ifelse(Apellidos == "DE LA", word(is,1,3), Apellidos),
         Apellidos = ifelse(Apellidos == "LIZARAN MACEDA", word(is,1,3), Apellidos))

localidades <- left_join(localidades,coordenadas, by = "Zonas") %>%
  dplyr::select(everything(), -c("estado"))

# verificaci?n de 2 apellidos iguales
n_occur <- data.frame(table(localidades$Apellidos))
reps <- localidades[localidades$Apellidos %in% n_occur$Var1[n_occur$Freq > 1],]

datos_c <- read_xlsx("matriz_cursos.xlsx","Detalle de matriz",range = cell_cols("A:I")) %>%
  mutate(IS = gsub("[0-9]+", "", IS), IS = gsub("[[:punct:]]", "", IS)) %>%
  mutate(Apellidos = word(IS,-2,-1)) %>%
  relocate(Apellidos, .after = IS) %>%
  dplyr::select(everything(),-c("IS & Modelo","V"))

datos_c <- datos_c %>%
  mutate(Apellidos=ifelse(datos_c$Apellidos =="DE LA",word(IS,1,3),datos_c$Apellidos),
         Apellidos=ifelse(datos_c$Apellidos == "LIZARAN MACEDA",paste(word(IS,-2,-1),word(IS,1)),datos_c$Apellidos))

datos_c <- left_join(datos_c,localidades, by="Apellidos")

datos_c <- datos_c %>%
  drop_na(Zonas) %>%
  relocate(Zonas, .after = IS) %>%
  dplyr::select(everything(),-c("is","Apellidos"))

names(datos_c) <- c("IS","Zonas","Modelo","Marca","Virtual","Requerido","Fecha","Certificacion","Latitud","Longitud")

marcas <- datos_c %>%
  distinct(Marca) %>%
  pull()

modelos <- datos_c %>%
  distinct(Modelo) %>%
  pull()

zonas <- datos_c %>%
  distinct(Zonas) %>%
  pull()

is <- datos_c %>%
  distinct(IS) %>%
  pull()

fechas <- datos_c %>%
  drop_na(Fecha) %>%
  distinct(Fecha) %>%
  pull()


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
# UI ----------------------------------------------------------------------

ui <- fluidPage(
  title = "Exel Pitss APP's",
  tags$head(tags$style(HTML(".nav.nav-pills.nav-stacked > .active > a, .nav.nav-pills.nav-stacked > .active > a:hover {
    background-color: #009ACB;
  }
.well {
    min-height: 20px;
    max-width: 300px;
    padding: 19px;
    margin-bottom: 20px;
    background-color: #F89438;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
    box-shadow: inset 0 1px 1px rgba(0,0,0,.05);

      }
                            "))),
shinyWidgets::setBackgroundImage(src = "fondo.png"),
shinyjs::useShinyjs(),
# shinyauthr login ui module here
shinyauthr::loginUI("login"),
absolutePanel(
  div(class = "push-right", shinyauthr::logoutUI(id = "logout")),
  uiOutput("tab1_ui")
)
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # login & logout -------------------------------------------------------------------
  
  credentials <- callModule(shinyauthr::login, "login", 
                            data = users,
                            user_col = user,
                            pwd_col = password,
                            sodium_hashed = FALSE,
                            log_out = reactive(logout_init()))
  
  
  # logout status managed by shinyauthr module and stored here
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  # user --------------------------------------------------------------------
  
  user <- reactive({
    credentials()$info
  })
  
  logo <- reactive({
    logos %>% filter(users == user()$user) %>% select(logos)
  })
  
  output$logo_user_header <- renderUI({
    div(align = "right",img(style = "max-width: 100%; height: auto !important;", src = logo()))
  })
  
  output$user_info <- renderUI({
    tags$img(style = "max-width: 100%; height: auto !important;",src = logo())
  })
  
  # change password ---------------------------------------------------------
  
  observeEvent(input$ask, {
    insertUI(
      selector = "#placeholder",
      ui = tags$div(id = "module-pwd",
                    tags$head(tags$style("#bien{color: white;
                             font-size: 12px;
                         }"
                    )
                    ),
                    tags$head(tags$style("#error{color: red;
                             font-size: 12px;
                         }"
                    )
                    ),
                    passwordInput("nueva_pwd1",
                              label = h3("New password", style="color:white; font-style:urw din italic;")),
                    passwordInput("nueva_pwd2",
                              label = h3("Confirm new password", style="color:white;")),
                    textOutput(outputId = "bien"),
                    textOutput(outputId = "error"),
                    actionButton("cancel","Cancelar",icon = icon("window-close"), style = "color: #fff; background-color: #F89438; padding:10px; font-size:100%"),
                    actionButton("submit","Actualizar contraseña", icon = icon("save"), style = "color: #fff; background-color: #009ACB; padding:10px; font-size:100%")
                    
      )
    )
    removeUI(selector = "#module-new-user")
    removeUI(selector = "#module-del-user")
  })
  
  observe({
    # input$nueva_pwd1
    # input$nueva_pwd2
    input1 <- input$nueva_pwd1
    input2 <- input$nueva_pwd2
    if (is.null(input2)){
      output$error <- renderText({""})
      output$bien <- renderText({""})
    } else if(is.null(input1)){
      output$error <- renderText({""})
      output$bien <- renderText({""})
    } else if (is.null(input2) & is.null(input1)){
      output$error <- renderText({""})
      output$bien <- renderText({""})
    } else if (input2 == input1) {
      output$bien <- renderText({"Las contraseñas coinciden"})
      output$error <- renderText({""})
    } else {
      output$bien <- renderText({""})
      output$error <- renderText({"Las contraseñas no coinciden"})
    }
  })
  
  observeEvent(input$submit,{
    
    if (input$nueva_pwd1 == input$nueva_pwd2){
      users <- #reactive({
        users %>% 
        mutate(password = replace(password, user == user()$user, input$nueva_pwd2))
      write_csv(users, "users.csv")
      removeUI(selector = "#module-pwd")
      shinyalert(title = "¡Contraseña actualizada!",
                 text = "Por favor refresca la página para volver a ingresar con tu nueva contraseña.",
                 type = "success",
                 confirmButtonCol = "#F89438",
                 animation = "pop",
                 closeOnEsc = TRUE
                 )
    } else {
      shinyalert(title = "Contraseñas incorrectas",
                 text = "Por favor vuelve a intentarlo.",
                 type = "error",
                 confirmButtonCol = "#F89438",
                 animation = "pop",
                 closeOnEsc = TRUE
      )
    }
  })
  
  observeEvent({
    input$cancel
  }, {
    removeUI(selector = "#module-pwd")
  })
  
  # add user ----------------------------------------------------------------
  observe({
    if (is.null(user()$user)){
      return()
    } else if (user()$user == "admin"){
      output$add_new_user <- renderUI({
        actionButton("add_u", "Agregar nuevo user", icon = icon("plus-circle"), style = "color: #fff; background-color: #F89438; padding:20px; font-size:100%",align = "center")
      })
    } else {
      output$add_new_user <- renderUI({
        #actionButton("add_u", "Agregar nuevo user", icon = icon("plus-circle"), style = "color: #fff; background-color: #F89438; padding:20px; font-size:100%",align = "center")
      })
    }
  })

  observeEvent(input$add_u,{
    insertUI(
      selector = "#placeholder2",
      ui = tags$div(id = "module-new-user",
                    textInput("new_user", label = h3("Nuevo user", style="color:white; font-style:urw din italic;")),
                    passwordInput("new_user_pwd", label = h3("Contraseña", style="color:white; font-style:urw din italic;")),
                    textInput("new_user_client", label = h3("Nombre del cliente o NA", style="color:white; font-style:urw din italic;")),
                    materialSwitch("expire_new_user", 
                                   label = h3("¿Expiración?", style="color:white; font-style:urw din italic;"),
                                   value = FALSE,
                                   status = "danger"),
                    uiOutput(outputId = "expire_date"),
                    radioGroupButtons("modo_admin",
                                      label      = h4("Modo Administrador", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                      choices    = c("Sí", "No"),
                                      selected   = "No",
                                      direction  = "horizontal",
                                      individual = TRUE,
                                      checkIcon  = list(
                                        yes      = tags$i(class = "fa fa-square",
                                                          style    = "color: #F89438"),
                                        no       = tags$i(class = "fa fa-square-o",
                                                          style    = "color: #F89438"))
                    ),
                    actionButton("cancel2","Cancelar",icon = icon("window-close"), style = "color: #fff; background-color: #F89438; padding:10px; font-size:100%"),
                    actionButton("submit2","Agregar user", icon = icon("save"), style = "color: #fff; background-color: #009ACB; padding:10px; font-size:100%")
      )
    )
    removeUI(selector = "#module-pwd")
    removeUI(selector = "#module-del-user")
  })
  
  revisar_admin <- reactive({
    if (input$modo_admin == "Sí"){
      return(TRUE)
    } else {
      return (FALSE)
    }
  })
  
  observe({
    input_expire <- input$expire_new_user
    if (is.null(input_expire)){
      return()
    } else if (input_expire == TRUE){
      output$expire_date <- renderUI({
        dateInput("expire_user_date", 
                  label = h4("Fecha de Expiración", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"), 
                  value = Sys.Date(),
                  min   = Sys.Date()
                  )
      })
    } else {
      output$expire_date <- renderUI({
        # dateInput("expire_user_date", 
        #           label = h4("Modo Administrador", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"), 
        #           value = Sys.Date(),
        #           min   = Sys.Date()
        # )
      })
    }
  })
  
  revisar_expire <- reactive({
    if (input$expire_new_user == FALSE){
      return(NA)
    } else {
      return(input$expire_user_date)
    }
  })
  
  observeEvent({
    input$submit2
    },{
      users <- #reactive({
        users %>% 
        rbind(c(input$new_user, input$new_user_pwd, input$new_user_client,"01/01/2021",revisar_expire(), revisar_admin()))
      write_csv(users, "users.csv")
      removeUI(selector = "#module-new-user")
      shinyalert(title = "¡Usuario agregado!",
                 text = "Por favor refresca la página para poder ingresar con el nuevo user.",
                 type = "success",
                 confirmButtonCol = "#F89438",
                 animation = "pop",
                 closeOnEsc = TRUE
      )
    })
  
  observeEvent({
    input$cancel2
  }, {
    removeUI(selector = "#module-new-user")
  })
  
  # delete user -------------------------------------------------------------
  observe({
    if (is.null(user()$user)){
      return()
    } else if (user()$user == "admin"){
      output$delete_user <- renderUI({
        actionButton("del_u", "Eliminar un user", icon = icon("trash"), style = "color: #fff; background-color: #E27831; padding:20px; font-size:100%",align = "center")
      })
    } else {
      output$delete_user <- renderUI({
        #actionButton("add_u", "Agregar nuevo user", icon = icon("plus-circle"), style = "color: #fff; background-color: #F89438; padding:20px; font-size:100%",align = "center")
      })
    }
  })
  
  
  observeEvent(input$del_u,{
    insertUI(
      selector = "#placeholder3",
      ui = tags$div(id = "module-del-user",
                    radioGroupButtons("user_eliminar",
                                      label      = h4("Usuario a eliminar", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                      choices    = opciones_user$user,
                                      #selected   = last(users$user),
                                      direction  = "horizontal",
                                      individual = TRUE,
                                      checkIcon  = list(
                                        yes      = tags$i(class = "fa fa-circle",
                                                          style    = "color: #F89438"),
                                        no       = tags$i(class = "fa fa-circle-o",
                                                          style    = "color: #F89438"))
                    ),
                    actionButton("cancel3","Cancelar",icon = icon("window-close"), style = "color: #fff; background-color: #F89438; padding:10px; font-size:100%"),
                    actionButton("submit3","Eliminar user", icon = icon("eraser"), style = "color: #fff; background-color: #009ACB; padding:10px; font-size:100%")
      )
    )
    removeUI(selector = "#module-pwd")
    removeUI(selector = "#module-new-user")
  })
  
  observeEvent({
    input$submit3
  },{
    users <- #reactive({
      users %>% 
      filter(user != input$user_eliminar)
      #rbind(c(input$new_user, input$new_user_pwd, input$new_user_client,"01/01/2021",revisar_expire(), revisar_admin()))
    write_csv(users, "users.csv")
    removeUI(selector = "#module-del-user")
    shinyalert(title = "¡Usuario eliminado!",
               text = "Por favor refresca la página para completar la eliminación.",
               type = "success",
               confirmButtonCol = "#F89438",
               animation = "pop",
               closeOnEsc = TRUE
    )
  })
  
  observeEvent({
    input$cancel3
  }, {
    removeUI(selector = "#module-del-user")
  })
  
  # renderUI ----------------------------------------------------------------
  output$tab1_ui <- renderUI({
    
    req(credentials()$user_auth)

# principal ---------------------------------------------------------------
    fluidPage(
      # title = "Exel Pitss APP's",
      # tags$head(tags$style(HTML(".nav.nav-pills.nav-stacked > .active > a, .nav.nav-pills.nav-stacked > .active > a:hover {
      #     background-color: #009ACB;
      #   }
      # .well {
      #     min-height: 20px;
      #     max-width: 300px;
      #     padding: 19px;
      #     margin-bottom: 20px;
      #     background-color: #F89438;
      #     border-radius: 4px;
      #     -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
      #     box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
      # 
      #       }
      #                             "))),
      useShinyalert(),
      tags$head(tags$style("#title{
                         color: white;
                        text-align: center;
                     } ")),
      tags$div(class = "header",
               titlePanel(
                 fluidRow(
                   column(10,img(height = 50, width = 190.47, src = "admin.png")), 
                   column(2, "Usuario:",style = "color:white; font-size: 20px; font-style:proxima nova; font-weight:bold; font-style:italic;",align = "center",uiOutput(outputId = "logo_user_header"))
                          )
                )
              ),
      navlistPanel(id = "TODO",well = TRUE,
                   tabPanel("Inicio",icon = icon("home"), style = "color:#000033", #house-user
                            titlePanel(div("Aplicaciones",style = "color:white; font-size: 80px; font-style:proxima nova; font-weight:bold; font-style:italic;",align = "center",
                                           tags$img(style = "max-width: 100%; height: auto !important;",
                                                    src = "admin.png"))),
                            fluidRow(align = "center",
                                     column(6,
                                            div(style="display: inline-block;",img(id="gif_slas",src="slas.gif", height = 330,style="cursor:pointer;"))),
                                     column(6,
                                            div(style="display: inline-block;",img(id="gif_cursos",src="cursos.gif", height = 330,style="cursor:pointer;"))
                                     )
                            ),
                            shinyauthr::logoutUI("logout"),
                            shinyjs::useShinyjs()
                   ),

# tabPanel: SLA's ---------------------------------------------------------
                   tabPanel("SLA's",icon = icon("clock-o"),style = "color:#000033", #stopwatch
                            shinyWidgets::setBackgroundImage(src = "fondo.png"),
                            tags$head(tags$style("#title{
                                                 color: white;
                                                text-align: center;
                                             } ")),
                            tags$div(class = "header",
                              titlePanel(
                                fluidRow(
                                  column(5,"Modelos SLA's",style = "color:white; font-size: 60px; font-style:proxima nova; font-weight:bold; font-style:italic;",align = "center"), 
                                  column(7, tags$img(style = "max-width: 100%; height: auto !important;",src = "admin.png"))
                                )
                              )),
                            navbarPage("Menú",
                                       
# tab: visualización ------------------------------------------------------
                                       tabPanel("Visualización de Datos",icon = icon("eye"),
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    uiOutput(outputId = "select_rutas1"),
                                                    # selectInput("ruta1",
                                                    #             label = h4("Ruta", style="color:white; font-style:urw din italic; font-size:20px"),
                                                    #             choices = rutas,
                                                    #             multiple = FALSE,
                                                    #             selected = "JALISCO"),
                                                    uiOutput(outputId = "select_frecuencias1"),
                                                    # radioGroupButtons("frecuencia1",
                                                    #              label = h4("Frecuencia", style="color:white; font-style:urw din italic; font-size:20px"),
                                                    #              choices = frecuencias,
                                                    #              selected = "Mensual",
                                                    #              status = "primary",
                                                    #              direction = "vertical")#,
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
                                       
# tab: descomposicion -----------------------------------------------------
                                       
                                       tabPanel("Descomposición", icon = icon("gear"),
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    uiOutput(outputId = "select_rutasD"),
                                                    # selectInput("rutaD",
                                                    #             label = h4("Ruta", style="color:white; font-style:urw din italic; font-size:20px"),
                                                    #             choices = rutas,
                                                    #             multiple = FALSE,
                                                    #             selected = "JALISCO"),
                                                    uiOutput(outputId = "select_frecuenciasD"),
                                                    # radioGroupButtons("frecuenciaD",
                                                    #              label = h4("Frecuencia", style="color:white; font-style:urw din italic; font-size:20px"),
                                                    #              choices = frecuencias,
                                                    #              selected = "Mensual",
                                                    #              status = "primary",
                                                    #              direction = "vertical"),
                                                    uiOutput(outputId = "select_tiemposD")
                                                    # radioGroupButtons("slaD",
                                                    #                   label      = h4("SLA", style="color:white; font-style:urw din italic; font-size:20px"),
                                                    #                   choices    = tiempos,
                                                    #                   selected   = "TR",
                                                    #                   direction  = "vertical",
                                                    #                   individual = FALSE,
                                                    #                   checkIcon  = list(
                                                    #                     yes      = tags$i(class = "fa fa-circle",
                                                    #                                       style    = "color: #009ACB"),
                                                    #                     no       = tags$i(class = "fa fa-circle-o",
                                                    #                                       style    = "color: #009ACB")))
                                                  ),
                                                  mainPanel(
                                                    tabsetPanel(
                                                      tabPanel(
                                                        "Gráfica",
                                                        tags$head(
                                                          tags$style(type = "text/css", "a{color: #fff}")
                                                        ),
                                                        style = "color: #fff",
                                                        plotOutput(outputId = "graficaD") %>% withSpinner(color="#F89438") )
                                                    )
                                                  )
                                                )
                                       ),
                                       
# tab: modelado -----------------------------------------------------------
                                       tabPanel("Modelado",icon = icon("wrench"),#tools
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    uiOutput(outputId = "select_rutas2"),
                                                    uiOutput(outputId = "select_frecuencias2"),
                                                    selectInput("modelo2",
                                                                label = h4("Modelo", style="color:white; font-style:urw din italic; font-size:20px"),
                                                                choices = c("ARIMA","ETS","STLF ARIMA", "STLF SNAIVE", "STLF ETS", "COMBINADO"),
                                                                multiple = TRUE,
                                                                selected = c("ARIMA","ETS","STLF ARIMA", "STLF SNAIVE", "STLF ETS", "COMBINADO"))
                                                  ),
                                                  mainPanel(
                                                    tabsetPanel(
                                                      tabPanel(
                                                        "Gráfica",
                                                        tags$head(
                                                          tags$style(type = "text/css", "a{color: #fff}")
                                                        ),
                                                        style = "color: #fff",
                                                        plotlyOutput("grafica2") %>% withSpinner(color="#F89438")),
                                                      tabPanel(
                                                        "Análisis",
                                                        tags$head(
                                                          tags$style(type = "text/css", "a{color: #fff}")
                                                        ),
                                                        style = "color: #fff",
                                                        splitLayout(
                                                          uiOutput(outputId = "select_sla2"),
                                                          radioGroupButtons("modelresidual2",
                                                                            label      = h4("Modelo", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                                                                            choices    = c("ARIMA","ETS","STLF ARIMA", "STLF SNAIVE", "STLF ETS", "COMBINADO"),
                                                                            selected   = "ETS",
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
                                       
# tab: pronósticos --------------------------------------------------------
                                       tabPanel("Pronósticos",icon = icon("chart-line"),
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    uiOutput(outputId = "select_rutas3"),
                                                    uiOutput(outputId = "select_frecuencias3"),
                                                    selectInput("modelo3",
                                                                label = h4("Modelo", style="color:white; font-style:urw din italic; font-size:20px"),
                                                                choices = c("ARIMA","ETS","STLF ARIMA", "STLF SNAIVE", "STLF ETS", "COMBINADO"),
                                                                multiple = TRUE,
                                                                selected = "ETS"),
                                                    uiOutput(outputId = "select_sla3"),
                                                    sliderInput("forecast3",
                                                                label = h4("Rango de pronóstico según la periodicidad escogida", style="color:white; font-style:urw din italic; font-size:20px"),
                                                                min = 1, max = 1000, value = 10)
                                                  ),
                                                  mainPanel(
                                                    tabsetPanel(
                                                      tabPanel(
                                                        "Gráfica",
                                                        tags$head(
                                                          tags$style(type = "text/css", "a{color: #fff}")
                                                        ),
                                                        style = "color: #fff",
                                                        plotlyOutput("grafica3") %>% withSpinner(color="#F89438"))
                                                    )
                                                  )
                                                )
                                       )
                                       
                            )
                   ),#tabPanel SLA's
                   
# MAPA COBERTURA ----------------------------------------------------------
                   tabPanel("Cursos", icon = icon("users"), style = "color:#000033",
                            tags$head(tags$style("#title{
                                                 color: white;
                                                text-align: center;
                                             } ",
                                             HTML(".nav.nav-pills.nav-stacked > .active > a, .nav.nav-pills.nav-stacked > .active > a:hover {
                                                background-color: #009ACB;
                                              }
                                            .well {
                                                min-height: 20px;
                                                max-width: 300px;
                                                padding: 19px;
                                                margin-bottom: 20px;
                                                background-color: #F89438;
                                                border-radius: 4px;
                                                -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
                                                box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
                                            
                                                  }
                                                                        "))),
                            tags$div(class = "header",
                                     titlePanel(
                                       fluidRow(
                                         column(5,"Cursos",style = "color:white; font-size: 60px; font-style:proxima nova; font-weight:bold; font-style:italic;",align = "center"), 
                                         column(7, tags$img(style = "max-width: 100%; height: auto !important;",src = "admin.png"))
                                       )
                                     )),
                            #dataTableOutput(outputId = "tablaprueba")
                            sidebarPanel(
                              #well = TRUE,
                              #alpha('white', 0.1),
                              #tags$head(tags$style(".well {background-color: #fff;  opacity: 0.8;}")),
                              #width = 3,
                              div(
                                id = "form",
                                style = "text-align:center; color: white; fill:transparent", #rgb(0,0,51/255);",
                                uiOutput(outputId = "select_marcas"),
                                uiOutput(outputId = "select_models"),
                                uiOutput(outputId = "select_zonas"),
                                uiOutput(outputId = "select_is"),
                                #uiOutput(outputId = "select_fechas")
                              ),
                              actionButton(
                                "reset_input",
                                label = "Reestablecer opciones",
                                icon = icon( name = "redo"),
                                style = "color: #fff; background-color: #000033; "
                              )

                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  "MAPA",icon=icon("map"),style = "color: #fff",
                                  shinydashboard::box(
                                    width = 12,
                                    title = "MAPA",

                                    # separate the box by a column
                                    column(width = 10,
                                           shiny::actionButton( inputId = "clearMap"
                                                                , icon = icon(name = "redo")
                                                                , label = "Reestablecer mapa"
                                                                , style = "color: #fff; background-color: #F89438; border-color: #E27831"
                                           ),
                                           leaflet::leafletOutput( outputId = "myMap",
                                                                   #height = 400,
                                                                   #width = 900
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
                          tableOutput(outputId = "tabla_is")# %>% withSpinner(color="#F89438")
                          )

                                    )
                          # , column(
                          #   width = 1,
                          #
                          # )
                                  )
                                ),

# TABLA COBERTURA ---------------------------------------------------------
                          tabPanel(
                            "TABLA DE CURSOS",icon=icon("table"),
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
                   ),
                   
# Menú Más ----------------------------------------------------------------
                   navbarMenu("Más",
                              tabPanel("Usuario",
                                       tags$head(tags$style("#title{
                                                 color: white;
                                                text-align: center;
                                             } ")),
                                       tags$div(class = "header",
                                                titlePanel(
                                                  fluidRow(
                                                    column(7,"Información de usuario",style = "color:white; font-size: 60px; font-style:proxima nova; font-weight:bold; font-style:italic;",align = "center"), 
                                                    column(5, uiOutput(outputId = "user_info"))
                                                  )
                                                )
                                                #,style = "color:white; font-size: 60px; font-style:proxima nova; font-weight:bold; font-style:italic;",align = "center"), 
                                                    #column(7, tags$img(style = "max-width: 100%; height: auto !important;",src = "admin.png"))
                                                ),
                                       fluidRow(#align = "center",
                                                      #uiOutput(outputId = "user_info"),
                                                      column(4, actionButton("ask","Cambiar Contraseña",icon = icon("edit"), style = "color: #fff; background-color: #F89438; padding:20px; font-size:100%"),align = "center"),
                                                      column(4, uiOutput(outputId = "add_new_user")),
                                                      column(4, uiOutput(outputId = "delete_user")),
                                                      tags$div(id = "placeholder"),
                                                      tags$div(id = "placeholder2"),
                                                      tags$div(id = "placeholder3")
                                       )
                              )
                   ), #menu mas
                   widths=c(2,10)
      ) #navlistPanel
    ) #fluidPage
  })
  
  # cambiar páneles ---------------------------------------------------------
  shinyjs::onclick("gif_slas",  updateTabsetPanel(session, inputId="TODO", selected="SLA's"))
  shinyjs::onclick("gif_cursos",  updateTabsetPanel(session, inputId="TODO", selected="Cursos"))                                                
  
  # datos filtrados sla's---------------------------------------------------------
  
  datos_filtrados1 <- reactive({
    if (user()$user == "admin"){
      d <- datos
    }else {
      d <- datos %>% 
        filter(
          str_detect(Cliente, users %>% filter(user == user()$user) %>% pull(cliente))
        )
    }
    return(d)
  })
  
  # datos diarios
  datos_tsbl <- reactive({
    datos_filtrados1() %>% dplyr::filter(IS.visita != "Consultoria Dual",
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
  })
  
  datos_day_tsbl <- reactive({
    datos_tsbl() %>% 
      group_by_key() %>% 
      index_by(Fecha = Recepcion) %>% 
      summarise(Tiempos = mean(Tiempos), .groups = "drop")
  })
  
  # datos semanales
  datos_week_tsbl <- reactive({ 
    datos_tsbl() %>% 
      group_by_key() %>% 
      index_by(Fecha = yearweek(Recepcion)) %>% 
      summarise(Tiempos = mean(Tiempos), .groups = "drop")
  })
  
  # datos mensuales
  datos_month_tsbl <- reactive({
    datos_tsbl() %>% 
      group_by_key() %>% 
      index_by(Fecha = yearmonth(Recepcion)) %>% 
      summarise(Tiempos = mean(Tiempos), .groups = "drop")
  })
  
  rutas <- reactive({
    datos_filtrados1() %>%  
      distinct(Ruta) %>% 
      pull()
  })
  
  # crear una lista con todas las tsibbles
  tsbls <- reactive({
    list(
      Diaria  = datos_day_tsbl(),
      Semanal = datos_week_tsbl(),
      Mensual = datos_month_tsbl()
    )
  })
  
  frecuencias <- reactive({names(tsbls())})
  
  train <- reactive({
    tsbls() %>% 
      map(. %>% filter_index("2018-03-01" ~ "2020-03-10"))
  }) 
  
  # tiempos <- c("TR","TST","TSP","TMO")
  #             reactive({
  #   datos_tsbl() %>% 
  #     distinct(SLA) %>% 
  #     pull()
  # })
  
  # inputs sla's------------------------------------------------------------------
  output$select_rutas1 <- renderUI({
    selectInput("ruta1",
                label = h4("Ruta", style="color:white; font-style:urw din italic; font-size:20px"),
                choices = rutas(),
                multiple = FALSE,
                selected = "JALISCO")
  })
  
  output$select_frecuencias1 <- renderUI({ 
    radioGroupButtons("frecuencia1",
                      label = h4("Frecuencia", style="color:white; font-style:urw din italic; font-size:20px"),
                      choices = frecuencias(),
                      selected = "Mensual",
                      status = "primary",
                      direction = "vertical")
  })
  
  output$select_rutasD <- renderUI({
    selectInput("rutaD",
                label = h4("Ruta", style="color:white; font-style:urw din italic; font-size:20px"),
                choices = rutas(),
                multiple = FALSE,
                selected = "JALISCO")
  })
  
  output$select_frecuenciasD <- renderUI ({
    radioGroupButtons("frecuenciaD",
                      label = h4("Frecuencia", style="color:white; font-style:urw din italic; font-size:20px"),
                      choices = frecuencias(),
                      selected = "Mensual",
                      status = "primary",
                      direction = "vertical")
  })
  
  output$select_tiemposD <- renderUI({
    radioGroupButtons("slaD",
                      label      = h4("SLA", style="color:white; font-style:urw din italic; font-size:20px"),
                      choices    = tiempos,
                      selected   = "TR",
                      direction  = "vertical",
                      individual = FALSE,
                      checkIcon  = list(
                        yes      = tags$i(class = "fa fa-circle",
                                          style    = "color: #009ACB"),  
                        no       = tags$i(class = "fa fa-circle-o",
                                          style    = "color: #009ACB")))
  })
  
  output$select_rutas2 <- renderUI({
    selectInput("ruta2",
                label = h4("Ruta", style="color:white; font-style:urw din italic; font-size:20px"),
                choices = rutas(),
                multiple = FALSE,
                selected = "JALISCO")
  })
  
  output$select_frecuencias2 <- renderUI({
    radioGroupButtons("frecuencia2",
                      label = h4("Frecuencia", style="color:white; font-style:urw din italic; font-size:20px"),
                      choices = frecuencias(),
                      selected = "Mensual",
                      status = "primary",
                      direction = "vertical")
  })
  
  output$select_sla2 <- renderUI({
    radioGroupButtons("sla2",
                      label      = h4("SLA", style="color:white; font-style:urw din italic; font-size:20px; font-weight: bold"),
                      choices    = tiempos,
                      selected   = "TSP",
                      direction  = "vertical",
                      individual = TRUE,
                      checkIcon  = list(
                        yes      = tags$i(class = "fa fa-circle",
                                          style    = "color: #F89438"),
                        no       = tags$i(class = "fa fa-circle-o",
                                          style    = "color: #F89438"))
    )
  })
  
  output$select_rutas3 <- renderUI({
    selectInput("ruta3",
                label = h4("Ruta", style="color:white; font-style:urw din italic; font-size:20px"),
                choices = rutas(),
                multiple = FALSE,
                selected = "JALISCO")
  })
  
  output$select_frecuencias3 <- renderUI({
    radioGroupButtons("frecuencia3",
                      label = h4("Frecuencia", style="color:white; font-style:urw din italic; font-size:20px"),
                      choices = frecuencias(),
                      selected = "Mensual",
                      status = "primary",
                      direction = "vertical")
  })
  
  output$select_sla3 <- renderUI({
    awesomeCheckboxGroup(
      inputId = "tiempos3",
      label = h4("SLA", style="color:white; font-style:urw din italic; font-size:20px"),
      choices = tiempos,
      selected = tiempos,
      inline = TRUE,
      status = "primary"
    )
  })
  
  # fit ---------------------------------------------------------------------
  
  modelos_fit <- reactive({
    train() %>% 
      map(
        . %>% dplyr::filter(Ruta == input$ruta2) %>% 
          model(
            #Media                     = MEAN(Tiempos),
            #Ingenuo                   = NAIVE(Tiempos),
            #SNAIVE              = SNAIVE(Tiempos),
            #Drift                     = RW(Tiempos ~ drift()),
            ETS            = ETS(Tiempos),
            ARIMA          = ARIMA(Tiempos),
            `STLF ARIMA`   = decomposition_model(
              STL(Tiempos ~ trend(window = 13) +season(window = "periodic"),robust = TRUE), ARIMA(season_adjust)),
            `STLF SNAIVE`  = decomposition_model(
              STL(Tiempos ~ trend(window = 13) +season(window = "periodic"),robust = TRUE), SNAIVE(season_adjust)),
            `STLF ETS`     = decomposition_model(
              STL(Tiempos ~ trend(window = 13) +season(window = "periodic"),robust = TRUE), ETS(season_adjust))
          ) %>% 
          mutate(COMBINADO = (ETS + ARIMA + `STLF ARIMA` + `STLF SNAIVE` + `STLF ETS`) / 5)
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
    tsbls()[[input$frecuencia1]] %>% 
      dplyr::filter(Ruta == input$ruta1) %>%
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
  
  
  # tab - descomposición  ---------------------------------------------------
  
  output$graficaD <- renderPlot({
    tsbls()[[input$frecuenciaD]] %>%  
      dplyr::filter(Ruta == input$rutaD) %>%
      filter(SLA %in% input$slaD) %>% 
      model(
        STL(Tiempos ~ trend(window = 7) +
              season(window = "periodic"),
            robust = TRUE)) %>% 
      components() %>% 
      autoplot(colour = "#03B2ED") +
      ggtitle(paste("Descomposición del",input$slaD,"de:",input$rutaD, tolower(input$frecuencia1))) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1)
      ) +
      scale_colour_manual(values = c("#F89438","#009ACB","#03B2ED","#E27831"))
  })
  
  
  # tab - modelado ----------------------------------------------------------
  
  # output$grafica2 <- renderPlotly({
  #   modelos_fc()[[input$frecuencia2]] %>% 
  #     dplyr::filter(.model %in% input$modelo2) %>% 
  #     autoplot(tsbls()[[input$frecuencia2]], level = NULL, size = 1) +
  #     facet_wrap(~ SLA, scales = "free_y") +
  #     ggtitle(paste("Datos de prueba y entrenamiento de",input$ruta2, tolower(input$frecuencia2))) +
  #     theme_minimal() +
  #     theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
  #           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
  #     labs(x = input$frecuencia2) +
  #     scale_colour_manual("Modelos", values = colores)
  # })
  
  output$grafica2 <- renderPlotly({
    tsbls()[[input$frecuencia2]] %>% 
      filter(Ruta == input$ruta2) %>% 
      ggplot(aes(x = Fecha, y = Tiempos)) +
      geom_line(size = 1) +
      geom_line(
        data = modelos_fc()[[input$frecuencia2]] %>% dplyr::filter(.model %in% input$modelo2),
        aes(y = .mean, color = .model),
        size = 1
      ) +
      ggtitle(paste("Datos de prueba y entrenamiento de",input$ruta2, tolower(input$frecuencia2))) +
      theme_minimal() +
      theme(plot.title   = element_text(family = '', colour="#000033", size=14, hjust = 0.5, vjust = 1),
                legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
      labs(x = input$frecuencia2) +
      scale_colour_manual("Modelos", values = colores) +
      facet_wrap(~ SLA, scales = "free_y")
  })
  
  output$reporte1 <- renderTable({
    modelos_fc()[[input$frecuencia2]] %>% 
      accuracy(tsbls()[[input$frecuencia2]]) %>%
      dplyr::group_by(SLA) %>% 
      slice_min(RMSE)
  })
  
  output$reporte2 <- renderPrint({
    modelos_fit()[[input$frecuencia2]] %>% 
      dplyr::filter(SLA == input$sla2) %>% 
      dplyr::select(input$modelresidual2) %>% 
      report()
  })
  
  output$reporte3 <- renderPlot({
    modelos_fit()[[input$frecuencia2]] %>% 
      dplyr::filter(SLA == input$sla2) %>% 
      dplyr::select(input$modelresidual2) %>% 
      gg_tsresiduals() +
      ggtitle(paste("Residuales modelo:", 
                    input$modelresidual2, "del", 
                    input$sla2, "de", input$ruta2, 
                    tolower(input$frecuencia2)))
  })
  
  # tab - pronósticos -------------------------------------------------------
  
  future_fit <- reactive({
    modelos_fit() %>% 
      map2(tsbls(), ~ .x %>% refit(.y)
      ) #%>% 
    # mutate(
    #   combinado = ()
    # )
  })
  
  future_fc <- reactive({
    future_fit() %>% 
      map(
        . %>% forecast(h = input$forecast3)
      )
  })
  
  # output$grafica3 <- renderPlot({
  #   future_fc()[[input$frecuencia3]] %>% 
  #     dplyr::filter(SLA    %in% input$tiempos3,
  #                   .model %in% input$modelo3) %>% 
  #     autoplot(tsbls()[[input$frecuencia3]], level = NULL) + 
  #     facet_wrap(~ SLA, scales = "free_y") +
  #     ggtitle(paste("Pronósticos SLA's de",input$ruta3)) + #"a ",input$forecast3,"días")) +
  #     theme_minimal() +
  #     theme(plot.title   = element_text(family = '', colour="#000033", size=17, hjust = 0.5, vjust = 1),
  #           legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
  #     labs(x = input$frecuencia3) +
  #     scale_colour_manual("Modelos", values = colores)
  # })
  
  output$grafica3 <- renderPlotly({
    tsbls()[[input$frecuencia3]] %>% 
      filter(Ruta == input$ruta3) %>% 
      ggplot(aes(x = Fecha, y = Tiempos)) +
      geom_line(size = 1) +
      geom_line(
        data = future_fc()[[input$frecuencia3]] %>% dplyr::filter(.model %in% input$modelo3, SLA %in% input$tiempos3),
        aes(y = .mean, color = .model),
        size = 1
      ) +
      ggtitle(paste("Pronósticos SLA's de",input$ruta3)) +
      theme_minimal() +
      theme(plot.title   = element_text(family = '', colour="#000033", size=14, hjust = 0.5, vjust = 1),
            legend.title = element_text(family = '', colour="#000033", size=12, hjust = 0.5, vjust = 1)) +
      labs(x = input$frecuencia3) +
      scale_colour_manual("Modelos", values = colores) +
      facet_wrap(~ SLA, scales = "free_y")
  })
  
  # datos filtrados mapa ----------------------------------------------------
  datos_filtrados <- reactive({
    datos_c %>%
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
    datos_c %>% 
      distinct(Marca) %>% 
      pull()
  })
  
  # OPCIONES MODELOS
  modelos <- reactive({
    datos_c %>%
      filter(Marca %in% input$marcas) %>% 
      distinct(Modelo) %>% 
      pull() 
  })
  
  # OPCIONES ZONAS
  zonas <- reactive({
    datos_c %>%
      filter(Marca %in% input$marcas, Modelo %in% input$modelos) %>%
      distinct(Zonas) %>% 
      pull() 
  })
  
  # OPCIONES IS
  is <- reactive({
    datos_c %>% 
      filter(Marca %in% input$marcas, Modelo %in% input$modelos, Zonas %in% input$zonas) %>%
      distinct(IS) %>% 
      pull()
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
  
  # inputs cobertura --------------------------------------------------------
  output$select_marcas <- renderUI({
    pickerInput(inputId  = "marcas",
                label    = h4("Marca", style="color:white; font-style:urw din italic; font-size:20px"),
                choices  = marcas(),
                options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                multiple = TRUE,
                selected = marcas())
  })

  output$select_models <- renderUI({
    pickerInput(inputId  = "modelos",
                label = h4("Modelo", style="color:white; font-style:urw din italic;font-size:20px"),
                choices = modelos(),
                options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                multiple = TRUE,
                selected = modelos())
  })

  output$select_zonas <- renderUI({
    pickerInput(inputId  = "zonas",
                label = h4("Zona", style="color:white; font-style:urw din italic; font-size:20px"),
                choices = zonas(),
                options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                multiple = TRUE,
                selected = zonas()
    )
  })

  output$select_is <- renderUI({
    pickerInput(inputId  = "is",
                label = h4("Ingeniero de Servicio", style="color:white; font-style:urw din italic; font-size:20px"),
                choices = is(),
                options  = list(`actions-box`=TRUE,`live-search`=TRUE),
                multiple = TRUE,
                selected = is()
    )
  })

  # tab - mapa cobertura ------------------------------------------------------------
  #función de nuestro mapa
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
          layerId = datos_c$Zonas,
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
      estado <- datos_c %>% filter(Latitud==click$lat,Longitud==click$lng) %>% distinct(Zonas)
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
      reset("form")
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
  ))

}

shinyApp(ui = ui, server = server)