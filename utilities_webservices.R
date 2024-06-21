# extras - need to put these in a more sensibly named utilitied script

updateURL <- function(tab = NULL, repo = NULL, file = NULL, mode = "push") {
  query <- list(tab = tab, repo = repo, file = file)
  if (all(sapply(query, is.null))) {
    query <- "?"
  } else {
    query <- paste0("?", httr:::compose_query(httr:::compact(query)))
  }

  updateQueryString(query, mode)
}

## tab title with close button
tab_title <- function(name) {
  tags$span(
    name,
    tags$span(icon("times"),
      style = "margin-left: 5px;",
      onclick = paste0("Shiny.setInputValue(\"", paste0("remove_tab"), "\", \"", name, "\", {priority: \"event\"})")
    )
  )
}

# Function to perform a GET request with JSON web tokens
get_with_token <- function(url, token) {
  # Create the HTTP header with the token
  headers <- c(Authorization = paste("Bearer", token))

  # Perform the GET request
  response <- httr::GET(url, httr::add_headers(headers))

  # Check if the request was successful
  if (httr::http_status(response)$category == "Success") {
    # Return the response content
    return(httr::content(response))
  } else {
    # Return an error message
    return(paste("GET request failed with status code", httr::http_status(response)$status_code))
  }
}





getListStockAssessments <- function() {
  stocklist <- jsonlite::read_json("https://adminweb06.ices.dk/minapi/getListStockAssessments", simplifyVector = TRUE)
  return(stocklist)
}

getEGStatistics <- function() {
  EGStats <- jsonlite::read_json("https://adminweb06.ices.dk/minapi/getEGStatistics", simplifyVector = TRUE)
  return(EGStats)
}

getTAFStocksStatistics <- function() {
  TAFStats <- jsonlite::read_json("https://adminweb06.ices.dk/minapi/getTAFStocksStatistics", simplifyVector = TRUE)
  return(TAFStats)
}



########### test
# Function to apply shiny::icon() based on a condition
get_icon <- function(text) {
  if (nchar(text) == 0) {
    x <- paste(shiny::icon("folder-open"))
  } else if (text == "csv") {
    x <- paste(shiny::icon("file-csv"))
  } else if (text == "png") {
    x <- paste(shiny::icon("file-image"))
  } else if (text == "rds" | text == "R" | text == "r") {
    x <- paste(shiny::icon("r-project"))
  } else if (text == "txt") {
    x <- paste(shiny::icon("code"))
  } else if (text == "bib") {
    x <- paste(shiny::icon("book"))
  } else {
    x <- ""
  }
  return(x)
}


CreateInteractiveTreeDF <- function(repo) {
  # paths <- list.files(jsonlite::read_json("https://adminweb06.ices.dk/api/dir/ices_cat_3_template", simplifyVector = TRUE),
  #   recursive = TRUE, full.names = TRUE,
  #   include.dirs = TRUE
  # )

  paths <- jsonlite::read_json(paste0("https://adminweb06.ices.dk/minapi/dir/", repo), simplifyVector = TRUE)

  # to clean off initial path -  will not need this in production
  paths <- paths[!(grepl("/[.]git", paths) | grepl("(bootstrap|boot)/library", paths))]

  tree <- as.Node(data.frame(pathString = paths))

  output <- ToDataFrameTree(tree, "pathString", "isLeaf", "level")
  output$filename <- basename(output$pathString)
  # output$filename <- paste0("`r shiny::icon('markdown')` ", output$filename)

  output$urlString <- paste0("https://ices-tools-dev.shinyapps.io/tafxplorer/?repo=", repo)
  output$ServerUrlString <- paste0("https://adminweb06.ices.dk/minapi/blob/", output$pathString)
  # could be handy for file icons
  output$FileFormats <- tools::file_ext(output$filename)

  return(output)
}

CreateInteractiveTreeHTML <- function(output, ns) {
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
  # cat(all)
  html <- markdown::mark(text = all)
  # browser()
  return(html)
}




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






# Return the UI for a modal dialog with data selection input. If 'failed'
# is TRUE, then display a message that the previous value was invalid.
dataModal <- function(failed = FALSE) {
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

    if (failed)
          div(tags$h5("Invalid username or password", style = "color: red;")),


    footer = tagList(
      # modalButton("Cancel"),
      actionButton("login", "Login")
    )
  )
}



# shinyApp(
#   ui = basicPage(
#     actionButton("show", "Show modal dialog"),
#     verbatimTextOutput("dataInfo")
#   ),

#   server = function(input, output) {
#     # reactiveValues object for storing current data set.
#     vals <- reactiveValues(data = NULL)

#     # Return the UI for a modal dialog with data selection input. If 'failed' is
#     # TRUE, then display a message that the previous value was invalid.
#     dataModal <- function(failed = FALSE) {
#       modalDialog(
#         textInput("dataset", "Choose data set",
#           placeholder = 'Try "mtcars" or "abc"'
#         ),
#         span('(Try the name of a valid data object like "mtcars", ',
#              'then a name of a non-existent object like "abc")'),
#         if (failed)
#           div(tags$b("Invalid name of data object", style = "color: red;")),

#         footer = tagList(
#           modalButton("Cancel"),
#           actionButton("ok", "OK")
#         )
#       )
#     }

#     # Show modal when button is clicked.
#     observeEvent(input$show, {
#       showModal(dataModal())
#     })

#     # When OK button is pressed, attempt to load the data set. If successful,
#     # remove the modal. If not show another modal, but this time with a failure
#     # message.
#     observeEvent(input$ok, {
#       # Check that data object exists and is data frame.
#       if (!is.null(input$dataset) && nzchar(input$dataset) &&
#           exists(input$dataset) && is.data.frame(get(input$dataset))) {
#         vals$data <- get(input$dataset)
#         removeModal()
#       } else {
#         showModal(dataModal(failed = TRUE))
#       }
#     })

#     # Display information about selected data
#     output$dataInfo <- renderPrint({
#       if (is.null(vals$data))
#         "No data selected"
#       else
#         summary(vals$data)
#     })
#   }
# )