mod_fileTree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      column(
        5,
        htmlOutput(outputId = ns("html_tree"), inline = FALSE)
      ),
      column(
        7,
        uiOutput(outputId = ns("file_viz"))  
      )
    )
  )
}


mod_fileTree_server <- function(id) {
  moduleServer(id, function(input, output, session, stock_select_server) {
    ns <- session$ns
    
    html_treeDF <- reactive({
    # reacts to url changes
    query_string <- getQueryString()
    CreateInteractiveTreeDF(repo = query_string$repo)
    # CreateInteractiveTreeDF(repo = "2022_sol.27.8c9a_assessment")
  })

  output$html_tree <- renderUI({
      # HTML(create_interactive_tree("./Data/ices_cat_3_template", "testRepo"))
      HTML(CreateInteractiveTreeHTML(html_treeDF()))
    })
  })
}