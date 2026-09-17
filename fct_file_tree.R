# UI utilities
getInteractiveTreeUI <- function(output, ns) {
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

  HTML(markdown::mark(text = all))
}

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
