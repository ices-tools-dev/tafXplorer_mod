library(shiny)
library(shinyWidgets)
library(leaflet)

########## Load utilities ############
# none yet

######### Load modules ##############
source("mod_map_selector.R")

# load app data
load("data/map_data.RData")

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
