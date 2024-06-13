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
  } else if (file_extension %in% c("r", "R", "Rmd")) {
    renderUI({
      fileToDisplay <- getURL(fileURL)
      # print(fileToDisplay)
      # html_text <- gsub("\r\n", "</br>", fileToDisplay)
      # HTML(html_text)
      # HTML(paste("<pre><code>", html_text, "</code></pre>"))

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
  } else if (file_extension == "md") {
    renderUI({
      fileToDisplay <- getURL(fileURL)
      HTML(markdown::mark(fileToDisplay))
      # print(fileToDisplay)
      # html_text <- gsub("\r\n", "</br>", fileToDisplay)
      # HTML(html_text)
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






# path <- "./Data/ices_cat_3_template"
# repo <- "ices_cat_3_template"
# CreateInteractiveTreeDF(repo)
# CreateInteractiveTreeHTML(CreateInteractiveTreeDF(path, repo))

# HTML(create_interactive_tree(path, repo))
