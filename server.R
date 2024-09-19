library(shiny)
library(DT)
library(leaflet)
library(shinyjs)


generar_coordenadas_madrid <- function() {
  lat_min <- 40.34
  lat_max <- 40.48
  lon_min <- -3.85
  lon_max <- -3.58
  
  latitud <- runif(1, min = lat_min, max = lat_max)
  longitud <- runif(1, min = lon_min, max = lon_max)
  
  return(c(latitud, longitud))
}

# Datos simulados para peticiones emergentes (sin coordenadas en la tabla)
datos_peticiones <- data.frame(
  ID = 1:5,
  Nombre_cliente = c("Juan", "Ana", "Pedro", "Laura", "Carlos"),
  DireccionRecogida = c("Calle 1", "Calle 2", "Calle 3", "Calle 4", "Calle 5"),
  Fragil = c("Sí", "No", "No", "Sí", "No"),
  latitud = c(40.4168, 40.4238, 40.4000, 40.415, 40.410),  # Esto no se muestra
  longitud = c(-3.7038, -3.6822, -3.7070, -3.701, -3.705),  # Esto no se muestra
  stringsAsFactors = FALSE
)

# Datos simulados para furgonetas
datos_furgonetas <- data.frame(
  ID = 1:3,
  Nombre = c("Furgoneta 1", "Furgoneta 2", "Furgoneta 3"),
  Telefono = c("+34654890912", "647 90 56 38", "677 523 101"),
  Latitud = c(40.4168, 40.4238, 40.4000),
  Longitud = c(-3.7038, -3.6822, -3.7070),
  stringsAsFactors = FALSE
)

shinyServer(function(input, output, session) {
  peticiones_emergentes <- reactiveVal(datos_peticiones)  
  pedidos_aceptados <- reactiveVal(data.frame())
  furgonetas_tiemporeal <- reactiveVal(datos_furgonetas)
  
  mover_furgonetas <- function() {
    df <- furgonetas_tiemporeal()
    for (i in 1:nrow(df)) {
      coords <- generar_coordenadas_madrid()
      df$Latitud[i] <- coords[1]
      df$Longitud[i] <- coords[2]
    }
    furgonetas_tiemporeal(df)  # Actualizar el dataframe reactivo
  }
  
  # Actualizar el mapa
  actualizar_mapa <- function() {
    leafletProxy("mapa_furgonetas") %>%
      clearMarkers() %>%
      addMarkers(data = peticiones_emergentes(), lat = ~latitud, lng = ~longitud, 
                 popup = ~paste("Pedido ID: ", ID, "<br>",
                                "Cliente: ", Nombre_cliente, "<br>",
                                "Entrega en: ",DireccionRecogida, "<br>",
                                "Fragil: ", Fragil),
                 icon = icons(iconUrl = "icono_pedido.png", iconWidth = 25, iconHeight = 41)) %>%
      addMarkers(data = furgonetas_tiemporeal(), lat = ~Latitud, lng = ~Longitud, 
                 popup = ~paste("Furgoneta: ", Nombre, "<br>",
                                "Telefono: ",Telefono),
                 icon = icons(iconUrl = "icono_furgoneta.png", iconWidth = 25, iconHeight = 41))
  }
  
  # Tabla de peticiones emergentes (sin coordenadas)
  output$tabla_peticiones <- renderDT({
    datatable(peticiones_emergentes()[, -c(5,6)], selection = 'single', escape = FALSE, 
              options = list(scrollY = 200, paging = FALSE))  # Agregar scroll
  })
  
  # Tabla de pedidos aceptados
  output$tabla_pedidos <- renderDT({
    datatable(pedidos_aceptados()[, -c(5,6)], selection = 'single', escape = FALSE, 
              options = list(scrollY = 200, paging = FALSE))
  })
  
  # Mapa interactivo
  output$mapa_furgonetas <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = datos_peticiones, lat = ~latitud, lng = ~longitud, 
                 popup = ~paste("Pedido ID:", ID, "<br>",
                                "Cliente:", Nombre_cliente, "<br>",
                                "Fragil:", Fragil),
                 icon = icons(iconUrl = "icono_pedido.png", iconWidth = 25, iconHeight = 41)) %>%
      addMarkers(data = datos_furgonetas, lat = ~Latitud, lng = ~Longitud, 
                 popup = ~paste("Furgoneta:", Nombre),
                 icon = icons(iconUrl = "icono_furgoneta.png", iconWidth = 25, iconHeight = 41))
  })
  
  # Aceptar peticiones
  observeEvent(input$tabla_peticiones_rows_selected, {
    fila <- input$tabla_peticiones_rows_selected
    if (length(fila) > 0) {
      pedido <- peticiones_emergentes()[fila, ]
      pedidos_aceptados(rbind(pedidos_aceptados(), pedido))
      peticiones_emergentes(peticiones_emergentes()[-fila, ])
      actualizar_mapa()
    }
  })
  
  # Actualización cada 5 minutos
  observeEvent(reactiveTimer(30000)(),{
    mover_furgonetas()
    actualizar_mapa()
  })
})
