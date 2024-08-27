# Return the UI for a modal dialog with data selection input. If 'failed'
# is TRUE, then display a message that the previous value was invalid.
loginModal <- function(failed = FALSE) {
  modalDialog(
    tags$script(HTML('
                      $(document).keyup(function(event) {
                        if ($("#password").is(":focus") && (event.keyCode == 13)) {
                            $("#login").click();
                        }
                      });
                    ')),
    tags$h3("Please enter your ICES credentials:"),
    textInput("username", "Username:", placeholder = "Your ICES username"),
    passwordInput("password", "Password:", placeholder = "Your ICES password"),
    if (failed) {
      div(tags$h5("Invalid username or password", style = "color: red;"))
    },
    footer = tagList(
      # modalButton("Cancel"),
      actionButton("login", "Login")
    )
  )
}
