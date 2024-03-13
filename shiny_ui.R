library(data.table)
library(dplyr)
library(dygraphs)
library(DT)
library(fisheryO)
library(htmltools)
library(htmlwidgets)
library(ggplot2)
library(ggradar)
library(ggtext)
library(glue)
library(gsubfn)
library(icesFO)
library(icesSAG)
library(icesTAF)
library(icesVocab)
library(leaflet)
library(plotly)
library(reshape2)
library(rintrojs)
library(RColorBrewer)
library(RCurl)
library(rvest)
library(scales)
library(sf)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(tidyverse)
library(tm)
library(widgetframe)
library(icesASD)
# library(mixfishtools)
library(shiny)
library(shinyWidgets)
library(mapplots)
library(maps)
library(mapdata)
library(pals)
library(leaflet)
library(sf)
library(shinyjs)
library(reshape2)
library(shinydashboard)
library(data.tree)
library(shinyAce)
library(wesanderson)


########## Load utilities ############
source("utilities_load_ecoregion_shp.r")
source("utilities_ecoregion_mapping.r")
source("utilities_webservices.r")
source("utilities_JS_callbacks.r")
source("utilities_plotting.r")

######### Load modules ##############
source("mod_01_map_selector.r")

# shiny user interface
ui <- fluidPage(
  navbarPage(
    title = "tafXplorer",
    tabPanel(
      "Map",
      mod_map_selector_ui("map_selector_1")
    )
    # tabPanel(
    #   "Biological production",
    #   mod_03_biological_production_ui("biological_production")
    # ),
    # tabPanel(
    #   "VPA",
    #   mod_06_VPA_ui("vpa")
    # ),
    # tabPanel(
    #   "R examples",
    #   mod_examples_ui("examples")
    # )
  )
)

# make a module for the example page
# have a side menu and all the examples.
# then the selected example is shown in the pane to the right
