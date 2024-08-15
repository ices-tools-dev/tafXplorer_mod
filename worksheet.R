
library(icesTAF)
library(shiny)

# draft.data() #
install.deps()

taf.boot()
source.all()
source.taf("shiny.R"); runApp('shiny', launch.browser = TRUE)


taf.boot.path()
source.taf("data.R")






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
