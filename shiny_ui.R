# library(shiny)
# library(shinyWidgets)
# library(leaflet)
# library(datamods)
# library(stringr)
# library(reactable)
# library(dplyr)
# library(data.tree)
# library(shinyAce)
# library(wesanderson)
# library(shinyjs)
# library(bslib)
# library(RCurl)
# library(icesConnect)

# ########## Load utilities ############
# source("fct_webservices.R")
# source("fct_login.R")
# source("fct_helpers.R")
# source("fct_file_tree.R")
# source("fct_file_viz.R")
# source("fct_resources.R")

# # load app data
# load("data/map_data.RData", envir = .GlobalEnv)

# ######### Load modules ##############
# source("mod_map_selector.R")
# source("mod_file_tree.R")
# source("mod_file_viz.R")
# source("mod_resources.R")

# title_html <- tags$a(
#   href = "https://ices-taf.shinyapps.io/tafxplorer/",
#   class = "navbar-brand-logo",
#   tags$img(
#     src = "negative_ices_logo.png",
#     alt = "ICES logo"
#   )
# )

# options(
#   spinner.type = 5,
#   spinner.color = "#00B7A3",
#   spinner.size = 0.7
# )


# # shiny user interface
# ui <- fluidPage(
#   tags$head(
#     useShinyjs(),
#     # tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
#     tags$script(src = "detect_click2.js"),
#     tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
#     tags$link(rel = "shortcut icon", href = "TAFXplorer_PNG.png"),
#     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
#   ),


#   # open app with the map selector tab only
#   # navbarPage(
#   #   collapsible = TRUE,
#   #   fluid = TRUE,
#   #   title = title_html,
#   #   id = "tabset",
#   #   windowTitle = "TAFXplorer",
#   #   # push right
#   #     bslib::nav_spacer(),

#   #     # Share button: use actionButton (NOT bookmarkButton)
#   #     bslib::nav_item(
#   #       actionButton("share_btn",
#   #         label = "Share",
#   #         icon = icon("link"),
#   #         class = "btn btn-default",
#   #         style = "margin-right: 8px;"
#   #       )
#   #     ),
#   #     tabPanel(
#   #       tagList("Resources"),
#   #       value = "resources",
#   #       mod_resources_ui("resources_1")
#   #     )
#   # )
#   navbarPage(
#     collapsible = TRUE,
#     fluid = TRUE,
#     title = title_html,
#     id = "tabset",
#     windowTitle = "TAFXplorer",
#     tabPanel(
#       title = "TAF assessment selection",
#       value = "TAF assessment selection",
#       mod_map_selector_ui("map_selector_1")
#     ),
#     bslib::nav_spacer(),
#     bslib::nav_item(
#       actionButton(
#   "share_btn",
#   label = "Share",
#   icon = icon("link"),
#   class = "fx-share-btn"
# )
#     ),
#     tabPanel(
#       title = "Resources",
#       value = "resources",
#       mod_resources_ui("resources_1")
#     )
#   )
# )
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
source("fct_webservices.R")
source("fct_login.R")
source("fct_helpers.R")
source("fct_file_tree.R")
source("fct_file_viz.R")
source("fct_resources.R")

# load app data
load("data/map_data.RData", envir = .GlobalEnv)

######### Load modules ##############
source("mod_map_selector.R")
source("mod_file_tree.R")
source("mod_file_viz.R")
source("mod_resources.R")

title_html <- tags$a(
  href = "https://ices-taf.shinyapps.io/tafxplorer/",
  class = "navbar-brand-logo",
  tags$img(
    src = "negative_ices_logo.png",
    alt = "ICES logo"
  )
)

options(
  spinner.type = 5,
  spinner.color = "#00B7A3",
  spinner.size = 0.7
)

ui <- fluidPage(
  tags$head(
    useShinyjs(),
    tags$script(src = "detect_click2.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
    tags$link(rel = "shortcut icon", href = "TAFXplorer_PNG.png"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  navbarPage(
    collapsible = TRUE,
    fluid = TRUE,
    title = title_html,
    id = "tabset",
    windowTitle = "TAFXplorer",

    # Main fixed left tab
    tabPanel(
      title = "TAF assessment selection",
      value = "map_selector",
      mod_map_selector_ui("map_selector_1")
    ),

    # Hidden anchor used only as insertion point for stock tabs
    tabPanel(
      title = "",
      value = "dynamic_anchor",
      div(style = "display:none;")
    ),

    # Push everything after this to the right
    bslib::nav_spacer(),

    # Fixed right item
    bslib::nav_item(
      actionButton(
        "share_btn",
        label = "Share",
        icon = icon("link"),
        class = "fx-share-btn"
      )
    ),

    # Fixed right tab
    tabPanel(
      title = "Resources",
      value = "resources",
      mod_resources_ui("resources_1")
    )
  )
)