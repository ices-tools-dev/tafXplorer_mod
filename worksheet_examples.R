tab_names <- LETTERS[1:3]

buttonSetInput <- function(nav_id, nav_value) {
  as.character(htmltools::tags$div(htmltools::tags$button(
    paste("Go to tab", nav_value),
    onClick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      nav_id,
      nav_value
    )
  )))
}

buttonWithAlert <- function(nav_id, nav_value) {
  as.character(htmltools::tags$div(htmltools::tags$button(
    paste("Alert", nav_value),
    onClick = sprintf("alert('Nav id is: %s, and nav value is: %s')", nav_id, nav_value)
  )))
}

rTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(ns("myTab"))
  )
}

rTabServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      alertButtonsHTML <- as.character(lapply(tab_names, buttonWithAlert, nav_id = ns("myNav")))
      setInputButtonsHTML <- as.character(lapply(tab_names, buttonSetInput, nav_id = ns("myNav")))
      
      tableWithButtons <- reactable::reactable(
        data.frame(
          Names = LETTERS[1:3],
          Alert = alertButtonsHTML,
          SetInput = setInputButtonsHTML
        ),
        columns = list(
          Alert = reactable::colDef(sortable = FALSE,html = TRUE),
          SetInput = reactable::colDef(sortable = FALSE, html = TRUE)
        )
      )
      
      output$myTab <- renderReactable({
        tableWithButtons
      })
      
      return(list(
        getButtonValue = shiny::reactive(input$myNav)
      ))
    }
  )
}


ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  title = "Reactable buttons navigation",
  shiny::titlePanel("App navigation with buttons in reactable widget"),
  shiny::div(
    class = "row",
    shiny::div(
      class = "col-4",
      helpText("Input from reactable button is:"),
      verbatimTextOutput("inputFromTableButton"),
      bslib::navs_tab_card(
        id = "myTabs",
        bslib::nav(title = "Tab X", value = "X", rTabUI("rtab") ),
        bslib::nav(title = "Tab A", value = "A", "Contents of tab A"),
        bslib::nav(title = "Tab B", value = "B", "Contents of tab B"),
        bslib::nav(title = "Tab C", value = "C", "Contents of tab C")
      )
    )
  )
)

server <- function(input, output, session) {
  
  rTabOut <- rTabServer("rtab")
  
  output$inputFromTableButton <- renderPrint({
    rTabOut$getButtonValue()
  })
  
  shiny::observeEvent(rTabOut$getButtonValue(), {
    shiny::updateTabsetPanel(session = session,
                             inputId = "myTabs",
                             selected = rTabOut$getButtonValue())
  })
}

shiny::shinyApp(ui, server)




library(shiny)
library(reactable)

ui <- fluidPage(
  titlePanel("row selection example"),
  reactableOutput("table"),
  verbatimTextOutput("selected")
)

server <- function(input, output, session) {
  selected <- reactive(getReactableState("table", "selected"))

  output$table <- renderReactable({
    reactable(iris, selection = "multiple", onClick = "select")
  })

  output$selected <- renderPrint({
    print(selected())
  })

  observe({
    print(iris[selected(), ])
  })
}

shinyApp(ui, server)