library(shiny)
library(bslib)
library(fontawesome)  # For Font Awesome icons

ui <- fluidPage(
  theme = bs_theme(),  # Use the default Bootstrap theme
  # Card with a close button
  card(
    card_header(
      title = "Card Header",
      tagList(
        span("Card Title"),
        actionButton(
          inputId = "close_card",
          label = NULL,
          icon = icon("times"),
          class = "btn-close-card",
          style = "
            position: absolute; 
            top: 10px; 
            right: 10px; 
            border: none; 
            background-color: transparent;
            font-size: 1em;      /* Adjust font size */
            width: 30px;           /* Adjust button width */
            height: 30px;          /* Adjust button height */
            padding: 0;            /* Remove padding */
          "
        )
      )
    ),
    card_body(
      "This is the body of the card."
    ),
    card_footer(
      "Footer content"
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$close_card, {
    removeUI(selector = ".card")
  })
}

shinyApp(ui, server)



library(shiny)
library(bslib)
library(fontawesome)

ui <- fluidPage(
  theme = bs_theme(),  # Use the default Bootstrap theme

  # Buttons to open the cards
  actionButton(inputId = "open_card1", label = "Open Card 1"),
  actionButton(inputId = "open_card2", label = "Open Card 2"),
  
  # Placeholder for dynamically inserting cards
  div(id = "card_container")
)

server <- function(input, output, session) {

  # Function to create a card UI
  create_card <- function(id, title, body, footer) {
    card(
      id = id,
      card_header(
        title = title,
        tagList(
          span(title),
          actionButton(
            inputId = paste0("close_", id),
            label = NULL,
            icon = icon("times"),
            class = "btn-close-card",
            style = "
              position: absolute; 
              top: 10px; 
              right: 10px; 
              border: none; 
              background-color: transparent;
              font-size: 1.5em;     
              width: 30px;           
              height: 30px;          
              padding: 0;            
            "
          )
        )
      ),
      card_body(body),
      card_footer(footer)
    )
  }
  
  # Observe event to open Card 1
  observeEvent(input$open_card1, {
    insertUI(
      selector = "#card_container",
      ui = create_card(
        id = "card1", 
        title = "Card 1 Header", 
        body = "This is the body of Card 1.", 
        footer = "Card 1 Footer content"
      )
    )
  })

  # Observe event to open Card 2
  observeEvent(input$open_card2, {
    insertUI(
      selector = "#card_container",
      ui = create_card(
        id = "card2", 
        title = "Card 2 Header", 
        body = "This is the body of Card 2.", 
        footer = "Card 2 Footer content"
      )
    )
  })
  
  # Observe event to close Card 1
  observeEvent(input$close_card1, {
    removeUI(selector = "#card1")
  })
  
  # Observe event to close Card 2
  observeEvent(input$close_card2, {
    removeUI(selector = "#card2")
  })
}

shinyApp(ui, server)




##### another example
library(shiny)
library(bslib)
library(fontawesome)

# Define a list of cards
card_info <- list(
  list(id = "card1", title = "Card 1", body = "This is the body of Card 1.", footer = "Card 1 Footer content"),
  list(id = "card2", title = "Card 2", body = "This is the body of Card 2.", footer = "Card 2 Footer content"),
  list(id = "card3", title = "Card 3", body = "This is the body of Card 3.", footer = "Card 3 Footer content")
)

ui <- fluidPage(
  theme = bs_theme(),  # Use the default Bootstrap theme

  # Dynamically generate the open buttons
  uiOutput("buttons_ui"),
  
  # Placeholder for dynamically inserting cards
  div(id = "card_container")
)

server <- function(input, output, session) {

  # Function to create a card UI
  create_card <- function(id, title, body, footer) {
    card(
      id = id,
      card_header(
        title = title,
        tagList(
          span(title),
          actionButton(
            inputId = paste0("close_", id),
            label = NULL,
            icon = icon("times"),
            class = "btn-close-card",
            style = "
              position: absolute; 
              top: 10px; 
              right: 10px; 
              border: none; 
              background-color: transparent;
              font-size: 1.5em;     
              width: 30px;           
              height: 30px;          
              padding: 0;            
            "
          )
        )
      ),
      card_body(body),
      card_footer(footer)
    )
  }
  
  # Dynamically generate the UI for open buttons
  output$buttons_ui <- renderUI({
    lapply(card_info, function(card) {
      actionButton(inputId = paste0("open_", card$id), label = paste("Open", card$title))
    })
  })
  
  # Observe events to open cards
  lapply(card_info, function(card) {
    observeEvent(input[[paste0("open_", card$id)]], {
      insertUI(
        selector = "#card_container",
        ui = create_card(
          id = card$id, 
          title = card$title, 
          body = card$body, 
          footer = card$footer
        )
      )
    })
  })

  # Observe events to close cards
  lapply(card_info, function(card) {
    observeEvent(input[[paste0("close_", card$id)]], {
      removeUI(selector = paste0("#", card$id))
    })
  })
}

shinyApp(ui, server)







# Card Module UI
card_module_ui <- function(id) {
  ns <- NS(id)
  div(id = ns("card_container"))
}

# Card Module Server
card_module_server <- function(id, card_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Function to create a card UI
    create_card <- function(id, title, body, footer) {
      card(
        id = ns(id),
        card_header(
          title = title,
          tagList(
            span(title),
            actionButton(
              inputId = ns(paste0("close_", id)),
              label = NULL,
              icon = icon("times"),
              class = "btn-close-card",
              style = "
                position: absolute; 
                top: 10px; 
                right: 10px; 
                border: none; 
                background-color: transparent;
                font-size: 1.5em;     
                width: 30px;           
                height: 30px;          
                padding: 0;            
              "
            )
          )
        ),
        card_body(body),
        card_footer(footer)
      )
    }
    
    observe({
      lapply(card_info(), function(card) {
        # Observe events to open cards
        observeEvent(input[[paste0("open_", card$id)]], {
          insertUI(
            selector = paste0("#", ns("card_container")),
            ui = create_card(
              id = card$id, 
              title = card$title, 
              body = card$body, 
              footer = card$footer
            )
          )
        })
        
        # Observe events to close cards
        observeEvent(input[[paste0("close_", card$id)]], {
          removeUI(selector = paste0("#", ns(card$id)))
        })
      })
    })
  })
}


library(shiny)
library(bslib)
library(fontawesome)

# Define a reactive list of cards (initially static, but could be made dynamic)
card_info_reactive <- reactiveVal(list(
  list(id = "card1", title = "Card 1", body = "This is the body of Card 1.", footer = "Card 1 Footer content"),
  list(id = "card2", title = "Card 2", body = "This is the body of Card 2.", footer = "Card 2 Footer content"),
  list(id = "card3", title = "Card 3", body = "This is the body of Card 3.", footer = "Card 3 Footer content")
))

ui <- fluidPage(
  theme = bs_theme(),  # Use the default Bootstrap theme
  
  # Dynamically generate the open buttons
  uiOutput("buttons_ui"),
  
  # Card module UI
  card_module_ui("card_module")
)

server <- function(input, output, session) {

  # Dynamically generate the UI for open buttons
  output$buttons_ui <- renderUI({
    lapply(card_info_reactive(), function(card) {
      actionButton(inputId = paste0("open_", card$id), label = paste("Open", card$title))
    })
  })

  # Call the card module server, passing the reactive card info
  card_module_server("card_module", card_info = card_info_reactive)
  
  # Example of updating the card info reactively (could be triggered by other events in the app)
  observeEvent(input$some_event, {
    card_info_reactive(updated_card_info_list)
  })
}

shinyApp(ui, server)




##############################################

library(shiny)
library(bslib)
library(fontawesome)

# Define the UI for the card module
card_module_ui <- function(id) {
  ns <- NS(id)
  div(id = ns("card_container"))
}

# Define the server logic for the card module
card_module_server <- function(id, card_info, card_ids) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Function to create a card UI
    create_card <- function(id, title, body, footer) {
      card(
        id = ns(id),
        card_header(
          title = title,
          tagList(
            span(title),
            actionButton(
              inputId = ns(paste0("close_", id)),
              label = NULL,
              icon = icon("times"),
              class = "btn-close-card",
              style = "
                position: absolute; 
                top: 10px; 
                right: 10px; 
                border: none; 
                background-color: transparent;
                font-size: 1.5em;     
                width: 30px;           
                height: 30px;          
                padding: 0;            
              "
            )
          )
        ),
        card_body(body),
        card_footer(footer)
      )
    }
    
    observe({
      lapply(card_ids(), function(card_id) {
        card <- card_info[[card_id]]
        
        # Observe events to open the card
        observeEvent(input[[paste0("open_", card_id)]], {
          insertUI(
            selector = paste0("#", ns("card_container")),
            ui = create_card(
              id = card_id, 
              title = card$title, 
              body = card$body, 
              footer = card$footer
            )
          )
        })
        
        # Observe events to close the card
        observeEvent(input[[paste0("close_", card_id)]], {
          removeUI(selector = paste0("#", ns(card_id)))
        })
      })
    })
  })
}

# Define the UI of the main app
ui <- fluidPage(
  theme = bs_theme(),  # Use the default Bootstrap theme
  
  # Buttons to trigger different sets of cards
  actionButton(inputId = "show_set1", label = "Show Set 1"),
  actionButton(inputId = "show_set2", label = "Show Set 2"),
  
  # Dynamically generate the open buttons
  uiOutput("buttons_ui"),
  
  # Card module UI
  card_module_ui("card_module")
)

# Define the server logic of the main app
server <- function(input, output, session) {

  # Define a list of card information
  card_info <- list(
    card1 = list(id = "card1", title = "Card 1", body = "This is the body of Card 1.", footer = "Card 1 Footer content"),
    card2 = list(id = "card2", title = "Card 2", body = "This is the body of Card 2.", footer = "Card 2 Footer content"),
    card3 = list(id = "card3", title = "Card 3", body = "This is the body of Card 3.", footer = "Card 3 Footer content"),
    card4 = list(id = "card4", title = "Card 4", body = "This is the body of Card 4.", footer = "Card 4 Footer content")
  )
  
  # Reactive list of card IDs
  card_ids <- reactiveVal(c("card1", "card2"))

  # Dynamically generate the UI for open buttons based on the reactive card IDs
  output$buttons_ui <- renderUI({
    lapply(card_ids(), function(card_id) {
      actionButton(inputId = paste0("open_", card_id), label = paste("Open", card_info[[card_id]]$title))
    })
  })
  
  # Call the card module server, passing the card information and reactive card IDs
  card_module_server("card_module", card_info = card_info, card_ids = card_ids)
  
  # Update the reactive card IDs when different button sets are clicked
  observeEvent(input$show_set1, {
    card_ids(c("card1", "card2"))
  })
  
  observeEvent(input$show_set2, {
    card_ids(c("card3", "card4"))
  })
}

shinyApp(ui, server)




library(shiny)

ui <- fluidPage(
  # Create a tabsetPanel with an ID to track the active tab
  navbarPage(
    id = "tabs",  # Assign an ID for tracking
    tabPanel("Tab 1", h2("This is Tab 1")),
    tabPanel("Tab 2", h2("This is Tab 2")),
    tabPanel("Tab 3", h2("This is Tab 3"))
  ),
  
  # Display the currently active tab
#   textOutput("active_tab")
)

server <- function(input, output, session) {
  # Output the active tab
  output$active_tab <- renderText({
    paste("Current active tab:", input$tabs)
  })
  
  # You can also use the active tab to trigger specific actions
  observeEvent(input$tabs, {
    # Do something based on the active tab
    print(paste("Switched to tab:", input$tabs))
  })
}

shinyApp(ui, server)





library(shiny)

demoUI <- function(id){
  
  tabsetPanel(
    id = "navigation",
    type = "hidden",
    
    tabPanelBody(
      value = "main_panel",
      actionButton(NS(id, "info_nav"), "Get info") # Pressing this button should take user to info_panel
    ),
    
    tabPanelBody(
      value = "info_panel",
      actionButton(NS(id, "main_nav"), "Back to main page")
    )
  )
  
}

demoServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$info_nav, {
      updateTabsetPanel(
        inputId = "navigation",
        selected = "info_panel"
      )
    })
    
    observeEvent(input$main_nav, {
      updateTabsetPanel(
        inputId = "navigation",
        selected = "main_panel"
      )
    })
    
  })
}


ui <- fluidPage(
  demoUI("test")
)

server <- function(input, output, session) {
  demoServer("test")
}

shinyApp(ui, server)




library(shiny)

ui <- fluidPage(
  
  tabsetPanel(
    id = "navigation",
    type = "hidden",
    
    tabPanelBody(
      value = "main_panel",
      actionButton("info_nav", "Get info")
    ),
    
    tabPanelBody(
      value = "info_panel",
      actionButton("main_nav", "Back to main page")
    )
  )
  
  
)

server <- function(input, output, session) {
  
  observeEvent(input$info_nav, {
    updateTabsetPanel(
      inputId = "navigation",
      selected = "info_panel"
    )
  })
  
  observeEvent(input$main_nav, {
    updateTabsetPanel(
      inputId = "navigation",
      selected = "main_panel"
    )
  })
  
  
}

shinyApp(ui, server)









library(shiny)
library(slickR)
install.packages("slickR")
carousel_ui <- function(id){
  ns <- NS(id)
  slickROutput(ns("slickr"), width="100%")
}

carousel_module <- function(input, output, session) {
  output$slickr <- renderSlickR({
    imgs <- list.files("~/Desktop/imgs", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
}

my_tab <- function(input,output,session,parent_session,tab_element,tab_name){

  ns = session$ns

  appendTab(inputId = "test_tabs",
            tabPanel(
              title = tab_name,
              value = tab_name,
              carousel_ui(ns("carousel")) # Operating in the parent session so explicitly supply the namespace
          ),
          session = parent_session
  )

  updateTabsetPanel(parent_session, "test_tabs", selected = tab_name) # Refer to test_tabs from the parent namespace

  # Need to update the carousel every time the user clicks on a tab
  # Else the carousel is only updated on the latest tab created

  observeEvent(tab_element(),{
    req(tab_element())

    if(tab_element() == tab_name){
      cat("Running\n")
      callModule(carousel_module,"carousel")# This module knows the namespace so no need to supply the namespace
    }
  })

}

ui <- fluidPage(  
      tabsetPanel(id = "test_tabs",
                  tabPanel(
                    title = "First tab",
                    value = "page1",
                    fluidRow(textInput('new_tab_name', 'New tab name'),
                             actionButton('add_tab_button','Add'))
                  )
      )
  )


server <- function(input, output, session) {

  tab_list <- NULL

  observeEvent(input$add_tab_button,{

                 tab_title <- input$new_tab_name
                 callModule(my_tab,tab_title,session,reactive(input$test_tabs),input$new_tab_name)

               })
}

shinyApp(ui, server)





# Module 1 UI and Server
module1UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("text"), "Module 1 Text Input"),
    textOutput(ns("output"))
  )
}

module1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$output <- renderText({
      paste("Module 1 says:", input$text)
    })
  })
}

# Module 2 UI and Server
module2UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("number"), "Module 2 Numeric Input", value = 0),
    textOutput(ns("output"))
  )
}

module2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$output <- renderText({
      paste("Module 2 received:", input$number)
    })
  })
}
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tabsetPanel(id = "tabs"),
  actionButton("add_tab", "Add Tab"),
  actionButton("remove_tab", "Remove Selected Tab")
)

server <- function(input, output, session) {
  tab_counter <- reactiveVal(0)
  tabs_list <- reactiveVal(list())  # Store a list of active tab IDs and modules
  
  # Function to generate UI for a tab with two modules
  generate_tab_ui <- function(tab_id) {
    fluidPage(
      h3(paste("Tab", tab_id)),
      module1UI(paste0("module1_", tab_id)),
      module2UI(paste0("module2_", tab_id))
    )
  }
  
  observeEvent(input$add_tab, {
    new_tab_number <- tab_counter() + 1
    new_tab_id <- paste0("tab", new_tab_number)
    
    # Add the new tab to the UI
    appendTab("tabs", tabPanel(paste("Tab", new_tab_number), value = new_tab_id, generate_tab_ui(new_tab_id)), select = TRUE)
    
    # Register the modules
    module1Server(paste0("module1_", new_tab_id))
    module2Server(paste0("module2_", new_tab_id))
    
    # Update the counter and list of tabs
    tab_counter(new_tab_number)
    tabs_list(c(tabs_list(), new_tab_id))
    
    # Update the URL when a new tab is added
    runjs(sprintf("history.pushState({}, '', '%s');", new_tab_id))
  })
  
  observeEvent(input$remove_tab, {
    current_tab <- input$tabs
    
    # Remove the tab from the UI
    removeTab("tabs", target = current_tab)
    
    # Update the list of remaining tabs
    remaining_tabs <- setdiff(tabs_list(), current_tab)
    tabs_list(remaining_tabs)
    
    if (length(remaining_tabs) > 0) {
      # Select the first remaining tab
      updateTabsetPanel(session, "tabs", selected = remaining_tabs[1])
      
      # Update the URL
      runjs(sprintf("history.pushState({}, '', '%s');", remaining_tabs[1]))
    }
  })
  
  observeEvent(input$tabs, {
    # Update the URL whenever a tab is manually selected
    runjs(sprintf("history.pushState({}, '', '%s');", input$tabs))
  })
}

shinyApp(ui, server)
