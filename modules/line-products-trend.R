line_products_trend <- function(df) {
  data <- df %>%
    filter(sale) |>
    mutate(
      created = as.Date(created),
      value = value / 100
    ) %>%
    summarise(value = round(sum(value)), .by = created) %>%
    mutate(total = cumsum(value))
  
  plot_ly(source = "line") %>%
    add_bars(
      data = data,
      x = ~created,
      y = ~value,
      name = "Sold Daily",
      color = I("#edc720"),
      marker = list(
        color = "#edc720",
        stroke = "#edc720"
      ),
      hoverinfo = "x+y"
    ) %>%
    add_lines(
      data = data,
      x = ~created,
      y = ~total,
      name = "Total Sold",
      yaxis = "y2",
      line = list(color = "#ccc", width = 5),
      hoverinfo = "x+y"
    ) %>%
    layout(
      font = list(family = "Press Start 2P", color = "#d3d3d3"),
      dragmode = "select",
      xaxis = list(
        title = "Date",
        titlefont = list(family = "Press Start 2P", color = "#d3d3d3"),
        tickfont = list(family = "Press Start 2P", color = "#d3d3d3")
      ),
      yaxis = list(
        title = "Daily Dynks",
        showgrid = TRUE,
        gridcolor = "lightgray",
        gridwidth = 0.5,
        gridDash = "dot",
        zeroline = FALSE,
        tickmode = "auto",
        titlefont = list(family = "Press Start 2P", color = "#d3d3d3"),
        tickfont = list(family = "Press Start 2P", color = "#d3d3d3")
      ),
      yaxis2 = list(
        title = "Total Dynks",
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        titlefont = list(family = "Press Start 2P", color = "#d3d3d3"),
        tickfont = list(family = "Press Start 2P", color = "#d3d3d3"),
        automargin = TRUE
      ),
      legend = list(x = 0.1, y = 0.9),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "#edc720", font = list(color = "black")),
      margin = list(l = 50, r = 100),  # zwiększony prawy margines dla yaxis2
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = FALSE)
}

# line_products_trend(df)
