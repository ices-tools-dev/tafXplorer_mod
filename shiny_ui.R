library(shiny)
library(shinyWidgets)
library(leaflet)

########## Load utilities ############
# none yet

# load app data
load("data/map_data.RData", envir = .GlobalEnv)

######### Load modules ##############
source("mod_map_selector.R")

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
