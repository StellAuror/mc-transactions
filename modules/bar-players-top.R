bar_players_top <- function(df) {
  df %>%
    filter(sale) %>%
    mutate(
      created = as.Date(created),
      value = value / 100
    ) %>%
    summarise(value = round(sum(value)), .by = playerName) %>%
    arrange(-value) |> 
    top_n(5) |>
    mutate(customdata = playerName) %>% 
    ggplot(aes(
      x = value,
      y = reorder(playerName, value),
      text = paste0(playerName, ": ", value),  # Dodanie tooltipa
      customdata = customdata  # Przypisanie customdata do aes
    )) +
    geom_col(fill = "#edc720") + 
    geom_text(aes(label = value, x = value /2), hjust = 0, color = "white", size = 5) +  # Wyświetlanie wartości wewnątrz słupków
    theme_minimal(base_family = "Press Start 2P") +
    labs(
      x = NULL, 
      y = "",
      title = ""
    ) +
    theme(
      plot.background = element_rect(fill = "#46464c", color = NA),
      panel.background = element_rect(fill = "#46464c", color = NA),
      axis.text = element_text(color = "#d3d3d3"),
      axis.title = element_text(color = "#d3d3d3"),
      plot.title = element_text(color = "#d3d3d3", hjust = 0.5, size = 14),
      panel.grid = element_blank()
    ) -> p
  
  ggplotly(p, tooltip = "text", source = "bar") %>% 
    layout(clickmode = "select", dragmode = "select") |>
    config(displayModeBar = FALSE)
}


# bar_players_top(df)