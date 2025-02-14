waterfall_players_history <- function(df) {
  df <- df |> 
    select(
      id,
      dynksAfter,
      dynksBefore,
      earnedDynks,
      created, 
      sale,
      desc
    ) |>
    arrange(id) |>
    slice_tail(n = 1, by = id) |>
    ungroup() |>
    mutate(
      id = row_number(),
      p_dynksAfter = lag(dynksAfter),
      p_dynksBefore = lag(dynksBefore),
      p_id = lag(id),
      transaction_type = if_else(sale, "Sold", "Bought"),
      earned_label = ifelse(abs(earnedDynks) > 5000, paste0(round(earnedDynks / 100)), ""),
      transition_label = paste0("Change: ", round((dynksBefore - p_dynksAfter) / 100)),
      Item = paste0(
        desc, "<br>", 
        created, "<br>",
        "Before: ", dynksBefore / 100, 
        "<br>After: ", dynksAfter / 100, 
        "<br>Type: ", transaction_type
      )
    )
  
  p <- ggplot(df) +
    geom_segment(
      aes(
        y = dynksBefore / 100,
        yend = dynksAfter / 100,
        xend = id,
        x = id,
        color = transaction_type
      ), size = 5
    ) +
    geom_segment(
      aes(
        y = p_dynksAfter / 100,
        yend = dynksBefore / 100,
        xend = (id + p_id) / 2,
        x = (id + p_id) / 2,
        color = "Bonus"
      ), size = 2, inherit.aes = FALSE 
    ) +
    geom_point(
      aes(
        y = dynksBefore / 100,
        x = (id + p_id) / 2,
        color = "Bonus"
      ), size = 3.5, inherit.aes = FALSE 
    ) +
    geom_text(aes(
      y = dynksBefore / 100,
      x = (id + p_id) / 2,
      label = round((dynksBefore - p_dynksAfter) / 100)
    ), angle = 90, nudge_y = 40, size = 3, color = "#CCCCCC", hjust = 0) +
   # geom_text(aes(
   #   y = (dynksBefore + dynksAfter) / 200,
   #   x = id,
   #   label = earned_label
   # ), angle = 90, size = 4, color = "#FFFFFF") +
    geom_text(
      aes(
        x = id,
        y = (dynksBefore + dynksAfter) / 200,
        label = Item
      ), alpha = 0
    ) +  
    scale_color_manual(
      name = "", 
      values = c("Bought" = "#FF5733", "Sold" = "#FFD700", "Bonus" = "#222223"),
      labels = c("Bought", "Bonus", "Sold")
    ) +
    labs(y = "Dynks", x = "Transactions #") +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#46464c", color = NA),
      panel.background = element_rect(fill = "#46464c", color = NA),
      legend.position = "top",  
      legend.title = element_blank(),  
      legend.text = element_text(size = 15, color = "#CCCCCC"),  
      axis.text = element_text(color = "#CCCCCC"),
      axis.title = element_text(color = "#CCCCCC"),
      legend.key = element_rect(fill = "#46464c", color = NA)
    ) +
    guides(
      color = guide_legend(
        override.aes = list(size = 6) 
      )
    )
  
  ggplotly(p, tooltip = "label") |>
    config(displayModeBar = F) |>
    layout(dragmode = "zoom")
}

# waterfall_players_history(df)