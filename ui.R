library(shiny)
library(DT)
library(leaflet)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Seguimiento de Pedidos y Furgonetas"),
  
  tabsetPanel(
    # Primera pestaña: Peticiones Emergentes y Mapa
    tabPanel("Peticiones Emergentes y Mapa",
             mainPanel(
               DTOutput("tabla_peticiones"),  # Tabla de pedidos emergentes debajo
               style = "height: 800px; overflow-y: scroll;",
               br(),
               leafletOutput("mapa_furgonetas", height = 500),  # Mapa en la parte superior
             )
    ),
    
    # Segunda pestaña: Pedidos Aceptados
    tabPanel("Pedidos Aceptados",
             DTOutput("tabla_pedidos")
    )
  )
))
