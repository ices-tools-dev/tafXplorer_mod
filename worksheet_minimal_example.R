library(shiny)
library(leaflet)

load("data/map_data.RData")

mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map1"))
  )
}

mod_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  output$map1 <- renderLeaflet(map)

  observeEvent(input$map1_shape_click, {
    if (input$map1_shape_click$group == "Eco_regions") {
      leafletProxy("map1", session) %>%
        showGroup(group = input$map1_shape_click$id)
    }
  })
  })
}

# main app
ui <- fluidPage(
  mod_map_ui("map")
)

server <- function(input, output, session) {
  mod_map_server("map")
}

app <- shinyApp(ui, server)

runApp(app)
