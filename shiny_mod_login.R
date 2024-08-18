mod_login_ui <- function(id) {
  ns <- NS(id)
  
}


mod_login_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        values <- reactiveValues(authenticated = FALSE)
        
        obs1 <- observe({
            showModal(dataModal())
        })

        obs2 <- observe({
            req(input$ok)
            isolate({
                Username <- input$username
                Password <- input$password
            })
            Id_username <- which(my_username == Username)
            Id_password <- which(my_password == Password)
            if (length(Id_username) > 0 & length(Id_password) > 0) {
                if (Id_username == Id_password) {
                    Logged <<- TRUE
                    values$authenticated <- TRUE
                    obs1$suspend()
                    removeModal()
                } else {
                    values$authenticated <- FALSE
                    Logged <<- FALSE
                }
            }
        })
    })
}




# library(shiny)
# library(shinydashboard)

# Logged = FALSE
# my_username <- "test"
# my_password <- "test"

# js <- '
# $(document).keyup(function(event) {
#   if ($("#password").is(":focus") && (event.keyCode == 13)) {
#       $("#login").click();
#   }
# });
# '

# ui <- dashboardPage(skin = "blue",
#                     dashboardHeader(title = "Dashboard"),
#                     dashboardSidebar(),
#                     dashboardBody("Test",
#                                   verbatimTextOutput("dataInfo")
#                     )
# )

# server = function(input, output, session) {

#   values <- reactiveValues(authenticated = FALSE)

#   # Return the UI for a modal dialog with data selection input. If 'failed' 
#   # is TRUE, then display a message that the previous value was invalid.
#   dataModal <- function(failed = FALSE) {
#     modalDialog(
#       tags$script(HTML(js)),
#       textInput("username", "Username:"),
#       passwordInput("password", "Password:"),
#       footer = tagList(
#         # modalButton("Cancel"),
#         actionButton("login", "Login")
#       )
#     )
#   }

#   # Show modal when button is clicked.  
#   # This `observe` is suspended only whith right user credential

#   obs1 <- observe({
#     showModal(dataModal())
#   })

#   # When OK button is pressed, attempt to authenticate. If successful,
#   # remove the modal. 

#   obs2 <- observe({
#     req(input$login)
#     isolate({
#       Username <- input$username
#       Password <- input$password
#     })
#     Id_username <- which(my_username == Username)
#     Id_password <- which(my_password == Password)
#     if (length(Id_username) > 0 & length(Id_password) > 0) {
#       if (Id_username == Id_password) {
#         Logged <<- TRUE
#         values$authenticated <- TRUE
#         obs1$suspend()
#         removeModal()

#       } else {
#         values$authenticated <- FALSE
#       }     
#     }
#   })


#   output$dataInfo <- renderPrint({
#     if(values$authenticated){
#       "OK!!!!!"
#     } else {
#       "You are NOT authenticated"
#     }
#   })

# }

# shinyApp(ui,server)