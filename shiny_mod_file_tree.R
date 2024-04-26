mod_file_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(outputId = ns("html_tree"), inline = FALSE)
  )
}


mod_file_tree_server <- function(id, file_tree) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    id_int <- as.integer(gsub("file_tree_", "", id))

    output$html_tree <- renderUI({
      if (!is.null(file_tree[[id]])) {
        HTML(CreateInteractiveTreeHTML(file_tree[[id]], ns))
      }
    })
  })
}
