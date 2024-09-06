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
source("utilities_login.R")
source("utilities.R")

# load app data
load("data/map_data.RData", envir = .GlobalEnv)

######### Load modules ##############
source("mod_map_selector.R")
source("mod_file_tree.R")
source("mod_file_viz.R")

title_html <- tags$a(
    href = "https://ices-taf.shinyapps.io/tafxplorer/",
    tags$img(
      src = "https://www.ices.dk/SiteCollectionImages/ICES%20logos/NEGATIVE%20ICES-logo.png",
      style = "margin-top: -15px; padding-right:10px;padding-bottom:-10px",
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
    tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
    tags$script(src = "detect_click2.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
  ),

  # open app with the map selector tab only
  navbarPage(
    collapsible = TRUE,
    fluid = TRUE,
    title = title_html,
    id = "tabset",
    windowTitle = "TAFXplorer"
  )
)
