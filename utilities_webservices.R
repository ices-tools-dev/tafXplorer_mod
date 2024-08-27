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
