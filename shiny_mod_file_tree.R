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
        getInteractiveTreeUI(file_tree[[id]], ns)
      }
    })
  })
}

# UI utilities
getInteractiveTreeUI <- function(output, ns) {
  makeOne <- function(i) {
    paste0(
      paste(rep("  ", output$level[i] - 1), collapse = ""),
      "* ",
      sapply(output$FileFormats[i], get_icon),
      " ",
      tags$a(href = "#", id = ns(i), output$filename[i], class = "taf-tree-node")
    )
  }

  all <- paste(
    sapply(1:nrow(output), makeOne),
    collapse = "\n"
  )

  HTML(markdown::mark(text = all))
}
