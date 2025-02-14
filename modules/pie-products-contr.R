bar_products_contr <- function(df, offer) {
  data <- df %>%
    filter(sale) %>%
    mutate(
      analyzed = ifelse(offerId == offer, "Chosen Offer", "The Rest"),
      value = round(value / 100)
    ) %>%
    group_by(analyzed) %>%
    summarise(totals = sum(value)) %>%
    mutate(analyzed = factor(analyzed, levels = c("The Rest", "Chosen Offer")))
  
  plot_ly(
    data = data,
    x = ~"Total",  
    y = ~totals,
    color = ~analyzed,
    type = "bar",
    text = ~paste(totals, "\n", analyzed),
    hoverinfo = "text",
    textposition = "inside",
    insidetextfont = list(size = 14, color = "black"),
    marker = list(line = list(width = 0)),
    colors = c("The Rest" = "#ccc", "Chosen Offer" = "#edc720")
  ) %>%
    layout(
      barmode = "stack",
      dragmode = "select",
      font = list(family = "Press Start 2P", color = "#d3d3d3"),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      showlegend = FALSE,
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
      margin = list(l = 5, r = 5, t = 5, b = 5) # minimalne marginesy
    ) |>
    config(displayModeBar = FALSE)
}

# bar_products_contr(df)
