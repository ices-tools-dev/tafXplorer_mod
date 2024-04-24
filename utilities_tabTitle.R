## tab title with close button
tab_title <- function(name, type = "data") {
  tags$span(
    name,
    tags$span(icon("times"),
              style = "margin-left: 5px;",
              onclick = paste0("Shiny.setInputValue(\"", paste0("remove_", type, "_tab"), "\", \"", name, "\", {priority: \"event\"})"))
  )
}
