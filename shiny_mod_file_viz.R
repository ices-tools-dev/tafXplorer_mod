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
        rev(files()),
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
                )#, container = htmltools::h3, padding = "0px"
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


# UI utilities
getFileUI <- function(info, ns) {
  # Download the file from the URL
  file_extension <- tolower(tools::file_ext(info$ServerUrlString))
  # print(file_extension)
  fileURL <- URLencode(info$ServerUrlString)

  if (file_extension == "csv") {
    # data <- read.table(fileURL, sep = ",", header = TRUE)

    renderTable({
      fileToDisplay <- read.table(fileURL, sep = ",", header = TRUE)
    })
  } else if (file_extension %in% c("png", "jpg")) {
    renderUI({
      HTML(c('<div><img src="', fileURL, '" width="85%" height="85%"></div>'))
    })
  } else if (file_extension == "bib") {
    renderUI({
      fileToDisplay <- getURL(fileURL)
      # html_text <- gsub("\r\n", "</br>", fileToDisplay)
      # HTML(html_text)

      aceEditor(
        outputId = ns("code_bib"),
        value = fileToDisplay,
        mode = "yaml",
        theme = "clouds_midnight",
        fontSize = 14,
        height = "80vh",
        readOnly = TRUE
      )
    })
  } else if (file_extension %in% c("r", "R", "rmd")) {
    renderUI({
      fileToDisplay <- getURL(fileURL)
      aceEditor(
        outputId = ns("code"), ,
        value = fileToDisplay,
        mode = "r",
        theme = "chrome",
        fontSize = 14,
        height = "80vh",
        readOnly = TRUE
      )
    })
  } else if (file_extension == "html") {
    renderUI({
      fileToDisplay <- getURL(fileURL)
      HTML(fileToDisplay)
      # print(fileToDisplay)
      # html_text <- gsub("\r\n", "</br>", fileToDisplay)
      # HTML(html_text)
    })
  } else if (file_extension %in% c("txt", "dat")) {
    renderUI({
      fileToDisplay <- getURL(fileURL)
      aceEditor(
        outputId = ns("dat"),
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
}
