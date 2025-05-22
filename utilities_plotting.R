### plotting functions
TAFStatsPlot <- function(TAFStatistics, category, percentage) {
  TAFStatistics$TotStocks <- TAFStatistics$stocks + TAFStatistics$taF_Stocks
  TAFStatistics$stocks_perc <- TAFStatistics$stocks / TAFStatistics$TotStocks * 100
  TAFStatistics$taF_stocks_perc <- TAFStatistics$taF_Stocks / TAFStatistics$TotStocks * 100

  if (isTRUE(percentage)) {
    fig1 <- plot_ly(
      data = TAFStatistics %>% filter(categories == category),
      x = ~year,
      y = ~stocks_perc,
      type = "bar",
      name = "Assessments not in TAF"
    )
    fig1 <- fig1 %>% add_trace(
      y = ~taF_stocks_perc,
      name = "Assessments in TAF"
    )
    fig1 <- fig1 %>% layout(
      yaxis = list(title = "% of stocks"),
      barmode = "stack",
      font = list(size = 18)
    )
  } else {
    fig1 <- plot_ly(
      data = TAFStatistics %>% filter(categories == category),
      x = ~year,
      y = ~stocks,
      type = "bar",
      name = "Assessments not in TAF"
    )
    fig1 <- fig1 %>% add_trace(
      y = ~taF_Stocks,
      name = "Assessments in TAF"
    )
    fig1 <- fig1 %>% layout(
      yaxis = list(title = "N. of stocks"),
      barmode = "stack",
      font = list(size = 18)
    )
  }
  fig1
}



EGStatsPlot <- function(EGStatistic) {
  # Pre-plotting operations
  df2 <-
    data.frame(
      expertGroup = unique(EGStatistic$expertGroup),
      colors = colorRampPalette(wes_palette("Darjeeling1"))(length(unique(EGStatistic$expertGroup)))
    )
  EGStatistic <- dplyr::inner_join(EGStatistic, df2)
  EGStatistic$percent <- round(EGStatistic$percent, digits = 0)

  # Plotting
  g <- highlight_key(EGStatistic, ~expertGroup)

  p <- plot_ly(g) %>%
    group_by(expertGroup) %>%
    add_trace(
      x = ~year,
      y = ~percent,
      color = ~expertGroup,
      mode = "lines+markers",
      line = list(
        shape = "spline",
        smoothing = .9,
        width = 4,
        color = ~colors
      ),
      marker = list(
        size = 8,
        color = ~colors
      )
    ) %>%
    layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "% of EG stocks in TAF"),
      font = list(size = 18)
    ) %>%
    highlight(
      on = "plotly_hover",
      selected = attrs_selected(showlegend = FALSE)
    )


  p
}


library(FLCore)
data(ple4)
library(ggplot2)
# ple4@catch.n
## stock at age w/ ssb (stacked) #####
df <- as.data.frame(ple4@catch.wt * ple4@catch.n)
df$age <- factor(df$age, levels = 3:10)
df2 <- as.data.frame(ssb(ple4))
df2$age <- factor(df2$age)
levels(df2$age) <- levels(df$age)[1]

p <- ggplot(df, aes(x = year, y = data, fill = age)) +
    ## geom_line(aes(linetype = as.factor(age)), lwd = 1.25) +
    geom_area() +
    scale_fill_brewer(palette = "Spectral") +
    ggtitle(label = "Stock at age (solid line = SSB)") +
    ylab("tonnes") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
    geom_line(data = df2, mapping = aes(x = year, y = data)) #+
    # geom_hline(yintercept = 473850, linetype = 2, color = "red")
# plotly::ggplotly(p)
print(p)
dev.off()



plot_stock_at_age <- function(stock) {
  # Calculate catch weight * numbers at age
  df <- as.data.frame(stock@catch.wt * stock@catch.n)
  
  # Set age factor levels (adjust as needed for your specific stock)
  df$age <- factor(df$age, levels = 3:10)
  
  # Get SSB data
  df2 <- as.data.frame(ssb(stock))
  df2$age <- factor(df2$age)
  levels(df2$age) <- levels(df$age)[1]  # Harmonize levels
  
  # Create the plot
  ggplot(df, aes(x = year, y = data, fill = age)) +
    geom_area() +
    scale_fill_brewer(palette = "Spectral") +
    ggtitle(label = "Stock at age (solid line = SSB)") +
    ylab("tonnes") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    geom_line(data = df2, mapping = aes(x = year, y = data))
}


