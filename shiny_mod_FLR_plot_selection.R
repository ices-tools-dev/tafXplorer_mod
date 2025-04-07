mod_FLR_plot_selection_ui <- function(id) {
    # ns <- NS(id)
    # tagList(
    #     selectInput(ns("plotType"), "Choose a plot:",
    #         choices = c(
    #             "Histogram" = "hist",
    #             "Boxplot" = "boxplot",
    #             "Scatterplot" = "scatter"
    #         )
    #     )                
    # )
    ns <- NS(id)
  tagList(
    actionButton(ns("plot1"), "Show FLQuant"),
    actionButton(ns("plot2"), "Show FLQuants"),
    actionButton(ns("plot3"), "Show FLStock"),
    actionButton(ns("plot4"), "Show FLSR"),
  )
}


mod_FLR_plot_selection_server <- function(id) {
    moduleServer(id, function(input, output, session) {
    selected_plot <- reactiveVal(NULL)

    observeEvent(input$plot1, {
      selected_plot("plot1")
    })

    observeEvent(input$plot2, {
      selected_plot("plot2")
    })

    observeEvent(input$plot3, {
      selected_plot("plot3")
    })

    observeEvent(input$plot4, {
      selected_plot("plot4")
    })

    return(selected_plot)
})
}