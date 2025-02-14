scatter_products_attr <- function(df) {
  df <- df %>%
    filter(sale) |>
    summarise(
      value = round(sum(value) / 100),
      n = sum(amount),
      .by = c(offerId, desc)
    )
  
  plot_ly(
    data = df,
    x = ~value,
    y = ~n,
    customdata = ~offerId,
    source = "scatter",
    type = 'scatter',
    mode = 'markers',
    color = I("#edc720"),
    text = ~paste("Item:", desc, "<br>Dynks:", value, "<br>Transactions:", n),
    hoverinfo = "text",
    marker = list(
      symbol = "square",
      size = ~(((value) - min(value))/(max(value) - min(value)) * 30),
      opacity = 1,
      color = "#edc720",
      stroke = "#edc720"
    )
  ) |>
    layout(
      font = list(family = "Press Start 2P"),
      hoverlabel = list(font = list(family = "Press Start 2P")),
      xaxis = list(
        title = "Total Amount of Dynks",
        titlefont = list(color = "#d3d3d3"),
        tickfont = list(color = "#d3d3d3")
      ),
      yaxis = list(
        title = "# of Transactions",
        titlefont = list(color = "#d3d3d3"),
        tickfont = list(color = "#d3d3d3")
      ),
      hovermode = TRUE,
      dragmode = "select",
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = FALSE)
}


# scatter_products_attr(df)
