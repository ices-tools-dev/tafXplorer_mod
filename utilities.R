# extras

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
