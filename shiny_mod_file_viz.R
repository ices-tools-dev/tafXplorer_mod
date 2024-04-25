mod_file_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("file_viz"))
  )
}



mod_file_viz_server <- function(id, repos, filenames) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    id_int <- as.integer(gsub("file_viz_", "", id))
    file_tree_id <- paste0("file_tree_", id_int)

    files <- reactive({
      files <- grep(file_tree_id, filenames(), value = TRUE)
      gsub(paste0(file_tree_id, "-"), "", files)
    })

observe({
  print(paste(repos[[file_tree_id]], ":", paste(files(), collapse = ", ")))
})

output$file_viz <- renderUI({
  card(
    card_header(paste0("File: ")),
    card_body("not working yet")
  )
})


    if (FALSE) {
      observeEvent(selectedFile$name, {
        id <- which(html_treeDF()$pathString == selectedFile$name)

        if (length(id) == 1) {
          # Download the file from the URL
          file_extension <- tolower(tools::file_ext(html_treeDF()$ServerUrlString[id]))
          # print(file_extension)
          fileURL <- html_treeDF()$ServerUrlString[id]

          if (file_extension == "csv") {
            # data <- read.table(fileURL, sep = ",", header = TRUE)

            output$file_viz <- renderTable({
              fileToDisplay <- read.table(fileURL, sep = ",", header = TRUE)
            })
            # output$downloadCSV <- downloadHandler(
            #   filename = function() {
            #     paste("downloaded_data.csv")
            #   },
            #   content = function(file) {
            #     write.csv(data, file)
            #   }
            # )
          } else if (file_extension == "png") {
            output$file_viz <- renderText({
              c('<img src="', fileURL, '" width="85%" height="85%">')
            })
            # output$fileViewer <- renderImage({
            #   list(src = input$urlInput, contentType = "image/png")
            # }, deleteFile = FALSE)
          } else if (file_extension == "bib") {
            output$file_viz <- renderUI({
              fileToDisplay <- getURL(fileURL)
              # html_text <- gsub("\r\n", "</br>", fileToDisplay)
              # HTML(html_text)

              aceEditor(
                outputId = "code_bib",
                value = fileToDisplay,
                mode = "yaml",
                theme = "clouds_midnight",
                fontSize = 14,
                height = "80vh",
                readOnly = TRUE
              )
            })
          } else if (file_extension %in% c("r", "R", "Rmd")) {
            output$file_viz <- renderUI({
              fileToDisplay <- getURL(fileURL)
              # print(fileToDisplay)
              # html_text <- gsub("\r\n", "</br>", fileToDisplay)
              # HTML(html_text)
              # HTML(paste("<pre><code>", html_text, "</code></pre>"))


              aceEditor(
                outputId = "code",
                value = fileToDisplay,
                mode = "r",
                theme = "chrome",
                fontSize = 14,
                height = "80vh",
                readOnly = TRUE
              )
            })
          } else if (file_extension == "md") {
            output$file_viz <- renderUI({
              fileToDisplay <- getURL(fileURL)
              HTML(markdown::mark(fileToDisplay))
              # print(fileToDisplay)
              # html_text <- gsub("\r\n", "</br>", fileToDisplay)
              # HTML(html_text)
            })
          } else if (file_extension == "html") {
            output$file_viz <- renderUI({
              fileToDisplay <- getURL(fileURL)
              HTML(fileToDisplay)
              # print(fileToDisplay)
              # html_text <- gsub("\r\n", "</br>", fileToDisplay)
              # HTML(html_text)
            })
          } else if (file_extension %in% c("txt", "dat")) {
            output$file_viz <- renderUI({
              fileToDisplay <- getURL(fileURL)
              aceEditor(
                outputId = "code",
                value = fileToDisplay,
                mode = "text",
                theme = "chrome",
                fontSize = 14,
                height = "80vh",
                readOnly = TRUE
              )
            })
          } else {
            # shinyjs::alert("Invalid file type or file format.")
          }
        } else {
          # render an image or text saying file doesnt exist
          shinyjs::alert("requested file not found.")
        }
      })
    }
  })
}
