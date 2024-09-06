
# Function to perform a GET request with JSON web tokens, ... is passed to content() function
get_with_token <- function(url, token, ...) {
  # Create the HTTP header with the token
  headers <- c(Authorization = paste("Bearer", token))

  # Perform the GET request
  response <- httr::GET(url, httr::add_headers(headers))

  # Check if the request was successful
  if (httr::http_status(response)$category == "Success") {
    # Return the response content
    return(httr::content(response, ...))
  } else {
    # Return an error message
    return(paste("GET request failed with status code", httr::http_status(response)$status_code))
  }
}

getListStockAssessments <- function() {
  stocklist <- jsonlite::read_json("https://adminweb06.ices.dk/minapi/getListStockAssessments", simplifyVector = TRUE)
  return(stocklist)
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
