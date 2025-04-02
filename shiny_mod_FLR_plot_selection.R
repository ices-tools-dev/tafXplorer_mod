mod_FLR_plot_selection_ui <- function(id) {
    ns <- NS(id)
    tagList(
        selectInput(ns("plotType"), "Choose a plot:",
            choices = c(
                "Histogram" = "hist",
                "Boxplot" = "boxplot",
                "Scatterplot" = "scatter"
            )
        )                
    )
}


mod_FLR_plot_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # print("mod_file_tree_server running")
    ns <- session$ns
    
  })
}