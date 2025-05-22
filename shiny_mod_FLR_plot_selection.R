mod_FLR_plot_selection_ui <- function(id) {
    
    ns <- NS(id)
  tagList(
    actionButton(ns("plot1"), "Show FLQuant"),
    actionButton(ns("plot2"), "Show FLQuants"),
    actionButton(ns("plot3"), "Show FLStock"),
    actionButton(ns("plot4"), "Show FLSR"),
    actionButton(ns("plot5"), "Show Stock at Age")
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

    observeEvent(input$plot5, {
      selected_plot("plot5")
    })

    return(selected_plot)
})
}