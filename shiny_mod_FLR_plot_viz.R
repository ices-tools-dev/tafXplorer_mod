mod_FLR_plot_viz_ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}


mod_FLR_plot_viz_server <- function(id, plot_name) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      req(plot_name())
      switch(plot_name(),
             "plot1" = ggplot(data = catch(ple4), aes(year, data)) + geom_point() + geom_line() + ylab("Catch (t)") + xlab("Year"),
             "plot2" = ggplot(data=FLQuants(Yield=catch(ple4), SSB=ssb(ple4), F=fbar(ple4)), aes(year, data)) + 
                        geom_line() + facet_wrap(~qname, scales="free_y", nrow=3) + labs(x="", y=""),
             "plot3" = ggplot(data=ple4, aes(year, data)) + geom_line(aes(group=age, colour=factor(age))) + 
                        facet_wrap(~slot, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "none"),
              "plot4" = plot(nsher)
    )
})
})
}