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




library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  navbarPage(title = "Test", id = "tabs",             
             tabPanel("Home",
                      sidebarPanel(
                        selectInput("testlist", "Select test:", list("A", "B", "C"), selected = "A"),
                        actionButton("append", "New tab")),
                      mainPanel()
             )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$append,{
    appendTab(inputId = "tabs",
              tabPanel(input$testlist,
                       sidebarPanel(
                         actionButton(paste0("remove_", input$testlist), "Delete")
                       )
              )
    )
  })
  
  observeEvent(lapply(grep(pattern = "^remove_", x = names(input), value = TRUE), function(x){input[[x]]}),{
    if(input$tabs != "Home"){
      removeTab(inputId = "tabs", target = input$tabs)
      updateSelectInput(session, "testlist", selected = input$testlist) # keep the selection when re-rendering sidebarPanel
    }
  })
}

shinyApp(ui, server)

ui <- fluidPage(
  tabsetPanel(id = "tabs",
              tabPanel(title = "Main",
                       value = "main",
                       
                       ## CONTENT PANEL ----- :
                       p("Add a new tab"),
                       actionButton("add", "Add", icon = icon("plus-circle"))
              ))
)

server <- function(input, output, session) {
  
  shinyInput <- function(name, id) paste(name, id, sep = "_")
  rv <- reactiveValues(counter = 0L)
  
  observeEvent(input$add, {
    rv$counter <- rv$counter + 1L
    ## GO TO THE NEWLY CREATED TAB:
    updateTabsetPanel(session, "tabs", shinyInput("new_tab", rv$counter))
  }, ignoreInit = TRUE)
  
  observeEvent(input$add, {
    appendTab(inputId = "tabs",
              tabPanel(title = paste("Tab", rv$counter),
                       value = shinyInput("new_tab", rv$counter),
                       
                       ## CONTENT PANEL ----- :
                       tags$h4(paste("This is tab:", rv$counter)),
                       p("Remove this tab"),
                       actionButton(shinyInput("remove_btn", rv$counter), "Remove", icon = icon("minus-circle"))
              )
    )
  })
  
  ## REACTIVITY TO ARRANGE TAB NAMES:
  current.tab <- eventReactive(input$tabs, {
    # don't accidentally remove main tab:
    if (!identical(input$tabs, "main")) {
      input$tabs
    } else {
      NULL
    }
  })
  
  ## OBSERVERS FOR THE REMOVE BTNS:
  observe({
    if (rv$counter > 0L) {
      lapply(seq(rv$counter), function(x) {
        observeEvent(input[[paste("remove_btn", x, sep = "_")]], {
          removeTab(inputId = "tabs", target = current.tab())
        })
      })
    }
  })
  
}

shinyApp(ui, server)





############################################################################

library(shiny)
library(shinyjs)
library(DT)
library(jsonlite)

## list of datasets
dataset_list <- c('iris', 'mtcars', 'USArrests', 'cars', 'airquality', 'CO2', 'faithful')

mod_data_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns('sel_dataset'), 'dataset', choices = dataset_list),
    DTOutput(ns("tab_dataset"))
  )
}

mod_data <- function(id, datasetname = "") {
  moduleServer(
    id,
    function(input, output, session) {

      ## dataset
      dataset <- reactive({
        req(input$sel_dataset)
        get(input$sel_dataset)
      })

      ## show dataset
      output$tab_dataset <- renderDT({
        req(dataset())
        datatable(dataset(), options = list(pageLength = 5))
      })


      ## return data
      rtn_data <- reactive({
        list(id = datasetname, name = input$sel_dataset, rows = nrow(dataset()), cols = ncol(dataset()))
      })

      return(reactive(rtn_data()))

    }
  )
}



server <- function(input, output, session) {

  rv <- reactiveValues(
    dataset_count = 0,                      # count of dataset tabs
    dataset_names = list(),                 # list of dataset tab names
    trigger_add_data_button = FALSE,        # trigger to add button to dataset tabPanel
    return_data = list()                    # list of dataset parameters
  )


  ## function to add a new dataset
  add_dataset <- function() {
    rv$dataset_count <- rv$dataset_count + 1
    dataset_name <- paste0("dataset_", rv$dataset_count)
    rv$dataset_names[[length(rv$dataset_names) + 1]] <- dataset_name
    rv$return_data[[dataset_name]] <<- mod_data(id = dataset_name, datasetname = dataset_name)
    appendTab(inputId = "tab_data", tabPanel(title = tab_title(dataset_name), value = dataset_name, mod_data_UI(dataset_name)))
  }


  ## tab title with close button
  tab_title <- function(name, type = "data") {
    tags$span(
      name,
      tags$span(icon("times"),
                style = "margin-left: 5px;",
                onclick = paste0("Shiny.setInputValue(\"", paste0("remove_", type, "_tab"), "\", \"", name, "\", {priority: \"event\"})"))
    )
  }


  ## tabs
  output$ui_tabs <- renderUI({
    isolate({
      rv$dataset_count <- rv$dataset_count + 1
      dataset_name <- paste0("dataset_", rv$dataset_count)
      rv$dataset_names[[length(rv$dataset_names) + 1]] <- dataset_name
      rv$return_data[[dataset_name]] <<- mod_data(id = dataset_name, datasetname = dataset_name)
      rv$trigger_add_data_button <- TRUE
    })
    tabsetPanel(id = "tab_data",
                tabPanel(title = tab_title(dataset_name), value = dataset_name, mod_data_UI(dataset_name)))
  })


  ## add a button to the tabPanel
  observeEvent(rv$trigger_add_data_button, {
    if (rv$trigger_add_data_button) {
      rv$trigger_add_data_button <- FALSE
      shinyjs::delay(100, session$sendCustomMessage(type = "addbutton", list(id = "tab_data", trigger = "add_data")))
      tryCatch(o_data$destroy(),
               error = function(e) NULL)
      o_data <<- observeEvent(input$add_data, {
        add_dataset()
      }, ignoreInit = TRUE)
    }
  }, once = FALSE)


  ## remove a dataset
  observeEvent(input$remove_data_tab, {
    removeTab(inputId = "tab_data", target = input$remove_data_tab)
    isolate({rv$dataset_names <- rv$dataset_names[!rv$dataset_names == input$remove_data_tab]})
  })



  ## reactive holding returned data
  returned_data <- reactive({
    use_data <- rv$return_data[unlist(rv$dataset_names)]
    l_data <- lapply(seq_along(use_data), function(i) {
      data <- use_data[[i]]
      if (length(data()) > 0) {
        data()
      } else {
        list()
      }
    })
    l_data
  })


  ## display returned data
  output$txt_returned_data <- renderPrint({
    req(returned_data())
    print(prettify(toJSON(returned_data(), auto_unbox = TRUE)))
  })

}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js", type="text/javascript")
  ),
  br(),
  fluidRow(
    column(8, uiOutput("ui_tabs")),
    column(4, verbatimTextOutput("txt_returned_data"))
  )
)

shinyApp(ui, server)


#############################################################################
# library(reactable)
# tab_names <- LETTERS[1:3]
# buttonSetInput <- function(nav_id, nav_value) {
# as.character(htmltools::tags$div(htmltools::tags$button(
# paste("Go to tab", nav_value),
# onClick = sprintf(
# "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
# nav_id,
# nav_value
# )
# )))
# }
# buttonWithAlert <- function(nav_id, nav_value) {
# as.character(htmltools::tags$div(htmltools::tags$button(
# paste("Alert", nav_value),
# onClick = sprintf("alert('Nav id is: %s, and nav value is: %s')", nav_id, nav_value)
# )))
# }
# rTabUI <- function(id) {
# ns <- NS(id)
# tagList(
# reactableOutput(ns("myTab"))
# )
# }
# rTabServer <- function(id) {
# moduleServer(
# id,
# function(input, output, session) {
# ns <- session$ns
# alertButtonsHTML <- as.character(lapply(tab_names, buttonWithAlert, nav_id = ns("myNav")))
# setInputButtonsHTML <- as.character(lapply(tab_names, buttonSetInput, nav_id = ns("myNav")))
# tableWithButtons <- reactable::reactable(
# data.frame(
# Names = LETTERS[1:3],
# Alert = alertButtonsHTML,
# SetInput = setInputButtonsHTML
# ),
# columns = list(
# Alert = reactable::colDef(sortable = FALSE,html = TRUE),
# SetInput = reactable::colDef(sortable = FALSE, html = TRUE)
# )
# )
# output$myTab <- renderReactable({
# tableWithButtons
# })
# return(list(
# getButtonValue = shiny::reactive(input$myNav)
# ))
# }
# )
# }
# ui <- bslib::page_fluid(
# theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
# title = "Reactable buttons navigation",
# shiny::titlePanel("App navigation with buttons in reactable widget"),
# shiny::div(
# class = "row",
# shiny::div(
# class = "col-4",
# helpText("Input from reactable button is:"),
# verbatimTextOutput("inputFromTableButton"),
# bslib::navs_tab_card(
# id = "myTabs",
# bslib::nav(title = "Tab X", value = "X", rTabUI("rtab") ),
# bslib::nav(title = "Tab A", value = "A", "Contents of tab A"),
# bslib::nav(title = "Tab B", value = "B", "Contents of tab B"),
# bslib::nav(title = "Tab C", value = "C", "Contents of tab C")
# )
# )
# )
# )
# server <- function(input, output, session) {
# rTabOut <- rTabServer("rtab")
# output$inputFromTableButton <- renderPrint({
# rTabOut$getButtonValue()
# })
# shiny::observeEvent(rTabOut$getButtonValue(), {
# shiny::updateTabsetPanel(session = session,
# inputId = "myTabs",
# selected = rTabOut$getButtonValue())
# })
# }
# shiny::shinyApp(ui, server)


library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  navbarPage(title = "Test", id = "tabs",             
             tabPanel("Home",
                      sidebarPanel(
                        selectInput("testlist", "Select test:", list("Aaaaaaa ", "Bbbbbbbb ", "Cccccc "), selected = "A"),
                        actionButton("append", "New tab")),
                      mainPanel()
             )
  )
)
## tab title with close button
tab_title <- function(name, type = "data") {
  tags$span(
    name,
    tags$span(icon("times"),
              style = "margin-left: 5px;",
              onclick = paste0("Shiny.setInputValue(\"", paste0("remove_", type, "_tab"), "\", \"", name, "\", {priority: \"event\"})"))
  )
}

server <- function(input, output, session) {
  
  observeEvent(input$append,{
    appendTab(inputId = "tabs",
              tabPanel(input$testlist,
                      title = tab_title(input$testlist, type = "data"),
                       sidebarPanel(
                        #  actionButton(paste0("remove_", input$testlist), "Delete")
                       )
              )
    )
  })
  
  observeEvent(lapply(grep(pattern = "^remove_", x = names(input), value = TRUE), function(x){input[[x]]}),{
    if(input$tabs != "Home"){
      removeTab(inputId = "tabs", target = input$tabs)
      updateSelectInput(session, "testlist", selected = input$testlist) # keep the selection when re-rendering sidebarPanel
    }
  })
}

shinyApp(ui, server)


library(shiny)

ui <- fluidPage(
    shinyjs::useShinyjs(),#this line NEEDS to be somewhere in the ui!
    navbarPage(
        title = HTML("<b><u>Title</u></b>"),
        id = 'banner'
    )
)

server <- function(input, output) {
    
    shinyjs::addClass(id = "banner", class = "navbar-right")#moves navbar right
    #this next line is the one APPENDING text to the navbar, thanks to "add = TRUE"
    shinyjs::html(id = "banner", html = "<p>companyName</p><p>company@place.com</p>", add = TRUE)

}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(bslib)
library(fontawesome)

ui <- fluidPage(
  theme = bs_theme(),  # Use the default Bootstrap theme
  
  # Login UI
  uiOutput("login_ui"),
  
  # Placeholder for the logged-in user information (icon and name)
  tags$div(
    uiOutput("user_info_ui"),
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;"
  ),
  
  # Main content (e.g., a navbar)
  navbarPage(
    title = "Navbar Example",
    id = "main_navbar",
    
    # Tab 1
    tabPanel("Home",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            actionButton("some_action", "Some Action")
          ),
          mainPanel(
            h3("This is the Home tab")
          )
        )
      )
    ),
    
    # Tab 2
    tabPanel("Another Tab",
      fluidPage(
        h3("This is another tab")
      )
    )
  ),
  
  # Toggle switch UI (appears after login)
  uiOutput("toggle_ui")
)
server <- function(input, output, session) {
  
  # Reactive value to track if the user is logged in
  logged_in <- reactiveVal(FALSE)
  
  # Reactive value to store the username
  user_name <- reactiveVal("")
  
  # Render the login UI
  output$login_ui <- renderUI({
    if (!logged_in()) {
      tagList(
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        actionButton("login_button", "Log in")
      )
    }
  })
  
  # Check login credentials
  observeEvent(input$login_button, {
    if (input$username == "user" && input$password == "password") {
      logged_in(TRUE)
      user_name(input$username)  # Store the username
    } else {
      showNotification("Invalid credentials", type = "error")
    }
  })
  
  # Render the user info UI if the user is logged in
  output$user_info_ui <- renderUI({
    if (logged_in()) {
      tags$div(
        icon("user-check", class = "fa-2x"),  # Logged-in icon
        tags$span(style = "margin-left: 10px;", paste("Logged in as:", user_name())),
        style = "display: flex; align-items: center;"
      )
    }
  })
  
  # Render the toggle UI if the user is logged in
  output$toggle_ui <- renderUI({
    if (logged_in()) {
      tags$div(
        style = "margin-top: 10px;",
        checkboxInput("toggle", "Toggle Switch", value = FALSE)
      )
    }
  })
}

shinyApp(ui, server)
