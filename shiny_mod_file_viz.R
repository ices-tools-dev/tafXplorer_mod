mod_file_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("file_viz"))
  )
}



mod_file_viz_server <- function(id, repos, file_tree, filenames) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    id_int <- as.integer(gsub("file_viz_", "", id))
    file_tree_id <- paste0("file_tree_", id_int)

    tree <- reactive({
      file_tree[[file_tree_id]]
    })


    files <- reactive({
      files <- grep(file_tree_id, filenames(), value = TRUE)
      as.integer(gsub(paste0(file_tree_id, "-"), "", files))
    })

    observe({
      print(paste(repos[[file_tree_id]], ":", paste(tree()$filename[files()], collapse = ", ")))
    })

    output$file_viz <- renderUI({
      lapply(
        files(),
        function(i) {
          card(
            card_header(
                tags$span(
                  "File: ",
                  tags$a(
                    tree()$filename[i],
                    href = URLencode(tree()$ServerUrlString[i]),
                    target = "_blank"
                  )
                )
              ),
            card_body(
              getFileUI(tree()[i, ], ns)
            )
          )
        }
      )
    })
  })
}
