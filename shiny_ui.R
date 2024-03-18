library(shiny)
library(shinyWidgets)
library(leaflet)
library(datamods)
library(stringr)
library(reactable)
library(dplyr)

########## Load utilities ############
source("utilities_webservices.R")

# load app data
load("data/map_data.RData", envir = .GlobalEnv)

######### Load modules ##############
source("mod_map_selector.R")
source("mod_navigation.R")

# shiny user interface
ui <- fluidPage(
  navbarPage(
    title = "tafXplorer",
    windowTitle = "tafXplorer",
    id = "tabset",
    fluid = TRUE,
    tabPanel(
      "Assessment selection",
      mod_navigation_ui("navigation_1"),
      mod_map_selector_ui("map_selector_1")
    )
  )
)
