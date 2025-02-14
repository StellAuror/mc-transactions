table_products_attr <- function(df) {
  df |>
    filter(sale) |>
    summarise(
      val = round(sum(value) / 100),
      n = sum(amount),
      .by = c(offerId, desc)
    ) -> data
  
  data |>
    rename("Item" = desc) |>
    select(-offerId) |>
    reactable(
      resizable = TRUE,
      showPageSizeOptions = FALSE,
      selection = "single",
      onClick = "select",
      highlight = TRUE,
      defaultPageSize = 5,
      theme = reactableTheme(
        color = "#d3d3d3",
        backgroundColor = "transparent",
        borderColor = "transparent",
        cellPadding = "8px",
        style = list(fontSize = "14px"),
        headerStyle = list(
          background = "transparent",
          border = "none"
        )
      ),
      columns = list(
        n = colDef(
          name = "Transactions",
          align = "left",
          cell = function(value) {
            width <- paste0(value / max(data$n) * 100, "%")
            bar_chart(value, width = width, fill = "#edc720", background = "transparent")
          }
        ),
        val = colDef(
          name = "Total Dynks",
          align = "left",
          cell = function(value) {
            width <- paste0((value / max(data$val)) * 100, "%")
            bar_chart(value, width = width, fill = "#edc720", background = "transparent")
          }
        )
      )
    )
}

