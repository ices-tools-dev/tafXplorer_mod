library(shiny)
library(shinyWidgets)
library(leaflet)
library(datamods)
library(stringr)
library(reactable)
library(dplyr)
library(data.tree)
library(shinyAce)
library(wesanderson)
library(shinyjs)
library(bslib)
library(RCurl)

########## Load utilities ############
source("utilities_webservices.R")

# load app data
load("data/map_data.RData", envir = .GlobalEnv)

######### Load modules ##############
source("mod_map_selector.R")
source("mod_file_tree.R")
source("mod_file_viz.R")

# shiny user interface
ui <- fluidPage(
  tags$head(
    useShinyjs(),
    # introjsUI(),
    tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
    tags$script(src = "detect_click2.js"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
  ),
  navbarPage(
    title = "tafXplorer",
    id = "tabset",
    selected = "Stock assessment selection",
    tabPanel(
      "Stock assessment selection",
      mod_map_selector_ui("map_selector_1")
    )
  )
)
