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
library(icesConnect)

########## Load utilities ############
source("utilities_webservices.R")

# load app data
load("data/map_data.RData", envir = .GlobalEnv)

######### Load modules ##############
source("mod_login.R")
source("mod_map_selector.R")
source("mod_file_tree.R")
source("mod_file_viz.R")

title_html <- tags$a(
    href = "https://ices-tools-dev.shinyapps.io/TAFXplorer/",
        tags$img(
        src = "negative_ices_logo.png",
        style = "margin-top: 0px; margin-bottom: 0px; padding-right:10px;",
        height = "50px"
      )
)

options(spinner.type = 5, 
        spinner.color = "#00B7A3",
        spinner.size = 0.7)





# shiny user interface
ui <- fluidPage(
  tags$head(
    useShinyjs(),
    # introjsUI(),
    tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
    tags$script(src = "detect_click2.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
  ),
  

  
  navbarPage(
    collapsible = TRUE,
    fluid = TRUE,
    title = title_html,
    id = "tabset",
    selected = "Stock assessment selection",
    tabPanel(
      "Stock assessment selection",
      tags$div(
        uiOutput("user_info_ui"),
        style = "position: absolute; top: 50px; right: 20px; z-index: 1000;"
      ),
      mod_map_selector_ui("map_selector_1")
    )
  )
)
