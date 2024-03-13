library(shiny)
library(shinyWidgets)
library(leaflet)

########## Load utilities ############

######### Load modules ##############
source("mod_map_selector.r")

# shiny user interface
ui <- fluidPage(
  navbarPage(
    title = "tafXplorer",
    tabPanel(
      "Map",
      mod_map_selector_ui("map_selector_1")
    )
  )
)
