mod_file_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # uiOutput(outputId = ns("file_viz"))
    div(id = ns("card_container"))
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
      browser()
      as.integer(gsub(paste0(file_tree_id, "-"), "", files))
    })

    observe({
      print(paste(repos[[file_tree_id]], ":", paste(tree()$filename[files()], collapse = ", ")))
    })
    
    observe({
      lapply(
        rev(files()), 
        function(i) {
        # Observe events to open cards
        # observeEvent(input[[paste0("open_card", i)]], {
          insertUI(
            selector = paste0("#", ns("card_container")),
            ui = create_card(
              cardId = paste0("card", i),
              title = #tags$span(
                  "File: ",
                  tags$a(
                    tree()$filename[i],
                    href = URLencode(tree()$ServerUrlString[i]),
                    target = "_blank"
                  #)
                ), 
              body = getFileUI(tree()[i, ], ns) 
              # footer = card$footer
            )
          )
        # })
        
        # Observe events to close cards (this does not work)
        observeEvent(input[[paste0("close_","card", i)]], {          
          removeUI(selector = paste0("#", ns(paste0("card", i))))
        })
      })
    })
    # output$file_viz <- renderUI({
      
    #   lapply(
    #     rev(files()),
    #     function(i) {
    #       create_card(
    #           id = paste0("card", i),
    #           title = tags$span(
    #               "File: ",
    #               tags$a(
    #                 tree()$filename[i],
    #                 href = URLencode(tree()$ServerUrlString[i]),
    #                 target = "_blank"
    #               )
    #             ), 
    #           body = getFileUI(tree()[i, ], ns) 
    #           # footer = card$footer
    #         )
    # })
    # })
  })
}
