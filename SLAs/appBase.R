

# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(patchwork)

# funciones ---------------------------------------------------------------
'tiempos_os_tbl <- read_xlsx("SLAs/Reporte_Tiempo_respuesta.xlsx")

tiempos_os_tbl %>% glimpse()


tiempos_os_tidy_tbl <- tiempos_os_tbl %>%
  # Cambiar el nombre de las cols. de fecha a fecha-hora
  rename(
    Fecha_hora_recepcion = `Fecha recepción`,
    Fecha_hora_cierre    = `Fecha cierre`
  ) %>% 
  # Crear dos columnas para la pura fecha
  mutate(
    Fecha_recepion = as.Date(Fecha_hora_recepcion),
    Fecha_cierre   = as.Date(Fecha_hora_cierre)
  ) %>% 
  filter(Estatus == "RESUELTA",
         Categoría == "CORRECTIVO",
         !is.na(`Tiempo transcurrido`),
         !is.na(`Tiempo efectivo`)) %>% 
  # Cambiando algunos nombres por facilidad
  rename(
    OS           = `N.° de orden`,
    serie        = `N.° de serie`,
    num_equipo   = `N.° de equipo`,
    num_cliente  = `N.°de cliente`,
    cant_visitas = `N.° de visitas`
  ) %>%
  # Agregar variable con solo el mes-año
  mutate(mes = tsibble::yearmonth(Fecha_recepion)) %>% 
  # Acomodar la fecha al inicio de la tabla
  relocate(mes, starts_with("Fecha")) %>% 
  # Separar las columnas de tiempo transcurrido y tiempo efectivo en horas y min.
  separate(`Tiempo transcurrido`, into = c("t_trans_horas", "t_trans_minutos"),
           sep = ":") %>% 
  separate(`Tiempo efectivo`, into = c("t_efect_horas", "t_efect_minutos")) %>% 
  # Crear variables de tipo duración
  mutate(
    t_transcurrido = duration(hours = as.integer(t_trans_horas), 
                              minutes = as.integer(t_trans_minutos)),
    t_efectivo     = duration(hours = as.integer(t_efect_horas), 
                              minutes = as.integer(t_efect_minutos))
  ) %>% 
  # Quitar cols. que contengan en el nombre "horas" o "minutos"
  select(-contains("horas"), -contains("minutos")) %>% 
  # Pasar a filas las columnas de tiempos
  pivot_longer(
    cols      = contains("Tiempo"),
    names_to  = "Tiempos",
    values_to = "Valor"
  ) %>% 
  # Separar la col. Valor en horas y minutos
  separate(Valor, into = c("Horas","Minutos"), sep = ":") %>% 
  # Crear columna con hora en formato decimal
  mutate(
    hora_decimal = as.numeric(Horas) + as.numeric(Minutos)/60
  ) %>% 
  # quitar cols. innecesarias
  select(-c(Horas, Minutos)) %>% 
  pivot_wider(
    names_from  = Tiempos,
    values_from = hora_decimal
  ) %>% 
  mutate(
    Cociente_tiempo = as.numeric(`Tiempo de respuesta`)/
      as.numeric(`Limite de tiempo de respuesta`)
  ) %>% 
  pivot_longer(
    cols      = contains("Tiempo"),
    names_to  = "Tiempos",
    values_to = "hora_decimal"
  ) '

# DATA --------------------------------------------------------------------
df <- tiempos_os_tidy_tbl

# listados(manufacturer)

estados <- df %>% 
    distinct(Ruta) %>% 
    pull()
modelo_I <- df %>% 
    distinct(Modelo) %>% 
    pull()
tecnico <- df %>% 
    distinct(`Técnico de visita`) %>% 
    pull()

#names(traccion) <- c("Delantera","4x4","Trasera")

# UI ----------------------------------------------------------------------

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
                          selected = estados,
                          width    = "150px" 
                        ),
                      
                      uiOutput(outputId = "select_models"),
                      
                      selectInput(
                          inputId   = "Tecnico",
                          label     = "Selecciona al tecnico:",
                          choices   = tecnico,
                          selected  = tecnico,
                      ),
                      selectInput(
                          inputId   = "Modelo",
                          label     = "Selecciona el modelo:",
                          choices   = modelo_I,
                      ),
                  submitButton()
                  ), #wellPanel
                  
                  tableOutput(outputId = "tabla1")
              ),
          
      ),
      tabPanel(
          title = "Tab 2"
      )
  )
)


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  
    df_filtrada <- reactive({
      df %>%
          filter(Ruta == input$estados)
    })
    
    modelos <- reactive({
        df_filtrada() %>% 
            distinct(Modelos) %>% 
            pull()
    })
    
    output$tabla1 <- renderTable({
  })
    
    # output$models <- renderPrint({
    #     modelos()
    # })
    
    output$select_models <- renderUI({
        pickerInput(
            inputId  = "Tecnico",
            label    = "Escoge los tecnicos a nombrar",
            choices  = tecnico(),
            selected = tecnico(),
            options  = list(
                `actions-box` = TRUE,
                size = 5,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE,
            width = "auto"
        )
    })
    }
shinyApp(ui, server)