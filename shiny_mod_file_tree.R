mod_file_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(outputId = ns("html_tree"), inline = FALSE)
  )
}



mod_file_tree_server <- function(id, repos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    html_treeDF <- reactive({
      # reacts to url changes
      id_int <- as.integer(gsub("file_tree_", "", id))

      CreateInteractiveTreeDF(repo = repos()[id_int])
      #CreateInteractiveTreeDF(repo = "2022_sol.27.8c9a_assessment")
    })

    output$html_tree <- renderUI({
      # HTML(create_interactive_tree("./Data/ices_cat_3_template", "testRepo"))
      HTML(CreateInteractiveTreeHTML(html_treeDF()))
    })


    observeEvent(input$clicked_text, {
      print(paste0("clicked_text = ", input$clicked_text))
    })

    observeEvent(input$clicked_text, {
      validate(
        need(input$clicked_text != "", "No file selected")
      )
      # browser()
      # Check if a valid URL is provided
      # if (!grepl("^https?://", input$urlInput)) {
      #   shinyjs::alert("Please enter a valid URL starting with http:// or https://.")
      #   return()
      # }

      fileName <- URLencode(html_treeDF()$pathString[as.numeric(input$clicked_text)])
      # remove repo from file path
      fileName <- substring(fileName, nchar(repo) + 2)

      browser()

      updateURL(repo = repo, file = fileName)

      reactiveVal(fileName)
    })
  })
}
