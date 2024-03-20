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

    
    observeEvent(input$clicked_text, {
    
    print(input$clicked_text)
    
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

        query_string <- getQueryString()

        fileName <- URLencode(html_treeDF()$pathString[as.numeric(input$clicked_text)])
        # remove repo from file path
        fileName <- substring(fileName, nchar(query_string$repo) + 2)

        updateQueryString(
            paste0("?tab=Assessment%20results&repo=", query_string$repo, "&file=", fileName),
            "push"
        )
    })
    
#   observeEvent(selectedFile$name, {
#       id <- which(html_treeDF()$pathString == selectedFile$name)

#       if (length(id) == 1) {
#           # Download the file from the URL
#           file_extension <- tolower(tools::file_ext(html_treeDF()$ServerUrlString[id]))
#           # print(file_extension)
#           fileURL <- html_treeDF()$ServerUrlString[id]

#           if (file_extension == "csv") {
#               # data <- read.table(fileURL, sep = ",", header = TRUE)

#               output$file_viz <- renderTable({
#                   fileToDisplay <- read.table(fileURL, sep = ",", header = TRUE)
#               })
#               # output$downloadCSV <- downloadHandler(
#               #   filename = function() {
#               #     paste("downloaded_data.csv")
#               #   },
#               #   content = function(file) {
#               #     write.csv(data, file)
#               #   }
#               # )
#           } else if (file_extension == "png") {
#               output$file_viz <- renderText({
#                   c('<img src="', fileURL, '" width="85%" height="85%">')
#               })
#               # output$fileViewer <- renderImage({
#               #   list(src = input$urlInput, contentType = "image/png")
#               # }, deleteFile = FALSE)
#           } else if (file_extension == "bib") {
#               output$file_viz <- renderUI({
#                   fileToDisplay <- getURL(fileURL)
#                   # html_text <- gsub("\r\n", "</br>", fileToDisplay)
#                   # HTML(html_text)

#                   aceEditor(
#                       outputId = "code_bib",
#                       value = fileToDisplay,
#                       mode = "yaml",
#                       theme = "clouds_midnight",
#                       fontSize = 14,
#                       height = "80vh",
#                       readOnly = TRUE
#                   )
#               })
#           } else if (file_extension %in% c("r", "R", "Rmd")) {
#               output$file_viz <- renderUI({
#                   fileToDisplay <- getURL(fileURL)
#                   # print(fileToDisplay)
#                   # html_text <- gsub("\r\n", "</br>", fileToDisplay)
#                   # HTML(html_text)
#                   # HTML(paste("<pre><code>", html_text, "</code></pre>"))


#                   aceEditor(
#                       outputId = "code",
#                       value = fileToDisplay,
#                       mode = "r",
#                       theme = "chrome",
#                       fontSize = 14,
#                       height = "80vh",
#                       readOnly = TRUE
#                   )
#               })
#           } else if (file_extension == "md") {
#               output$file_viz <- renderUI({
#                   fileToDisplay <- getURL(fileURL)
#                   HTML(markdown::mark(fileToDisplay))
#                   # print(fileToDisplay)
#                   # html_text <- gsub("\r\n", "</br>", fileToDisplay)
#                   # HTML(html_text)
#               })
#           } else if (file_extension == "html") {
#               output$file_viz <- renderUI({
#                   fileToDisplay <- getURL(fileURL)
#                   HTML(fileToDisplay)
#                   # print(fileToDisplay)
#                   # html_text <- gsub("\r\n", "</br>", fileToDisplay)
#                   # HTML(html_text)
#               })
#           } else if (file_extension %in% c("txt", "dat")) {
#               output$file_viz <- renderUI({
#                   fileToDisplay <- getURL(fileURL)
#                   aceEditor(
#                       outputId = "code",
#                       value = fileToDisplay,
#                       mode = "text",
#                       theme = "chrome",
#                       fontSize = 14,
#                       height = "80vh",
#                       readOnly = TRUE
#                   )
#               })
#           } else {
#               # shinyjs::alert("Invalid file type or file format.")
#           }
#       } else {
#           # render an image or text saying file doesnt exist
#           shinyjs::alert("requested file not found.")
#       }
#   })

  })
}