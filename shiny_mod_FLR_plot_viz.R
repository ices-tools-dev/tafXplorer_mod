mod_FLR_plot_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("plot"))
  )
}


mod_FLR_plot_viz_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      req(input$plotType)
      if (input$plotType == "hist") {
        hist(rnorm(100), col = "skyblue", main = "Histogram")
      } else if (input$plotType == "boxplot") {
        boxplot(rnorm(100), col = "tomato", main = "Boxplot")
      } else if (input$plotType == "scatter") {
        plot(rnorm(100), rnorm(100), col = "blue", pch = 19, main = "Scatterplot")
      }
    })
  })
}