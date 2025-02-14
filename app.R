source("run.R")

dfRaw <- get_data()
directoryPlayers <- dfRaw$playerName |> unique()
directoryOffers <- dfRaw$description |> unique()

my_theme <- bs_theme(
  bg = "#fff",
  fg = "black",
  primary = "#00ff00",
  secondary = "#8B4513",
  base_font = font_google("Press Start 2P")
)

ui <- page_sidebar(
  title = "MC - Shop Transactions",
  theme = my_theme,
  fillable = F,
  includeCSS("www/style.css"),
  sidebar = sidebar(
    open = F,
    width = "20%",
    shinyWidgets::pickerInput(
      "offer", "Offers",
      choices = directoryOffers, selected = directoryOffers,
      multiple = T
    ),
    shinyWidgets::pickerInput(
      "player", "Players",
      choices = directoryPlayers, selected = directoryPlayers,
      multiple = T
    )
  ),
  layout_columns(
    col_widths = c(7, 5),
    row_heights = "500px",
    card(
      card_header("What items are being sold? - Overview"),
      plotlyOutput("L1.1")
    ),
    card(
      card_header("What items are being sold? - Closer Look"),
      reactableOutput("L1.2")
    )
  ),
  layout_columns(
    col_widths = c(2, 10),
    card(
      card_header("Contribution of the selected item to the total."),
      plotlyOutput("L2.1")
    ),
    card(
      card_header("The selected item over the time."),
      plotlyOutput("L2.2")
    )
  ),
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Top 5 The most dedicated players."),
      plotlyOutput("L3.1")
    ),
    card(
      card_header("Balance sheet (total) of the selected player."),
      plotlyOutput("L3.2")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactiveValues()
  observe({
    data$dfRaw <- dfRaw |>
      filter(
        description %in% if (length(input$offer) == 0) {directoryOffers} else {input$offer},
        playerName %in% if (length(input$player) == 0) {directoryPlayers} else {input$player}
      )
  })
  
# Register any changes made on L1.1
# Modifies df_L1.1 accordingly
  observe({
    event_data("plotly_selected", source = "scatter")$customdata |>
      unique() -> x
    
    if (is.null(x)) {
      data$dfRaw -> data$df_L1.1
    } else {
      data$dfRaw |> dplyr::filter(offerId %in% x) -> data$df_L1.1
    }
  })
  
# Register value selected in table (L1.2)
# Modifies v_L1.2 accordingly
  observe({
    req(getReactableState("L1.2", "selected"))
    
    selected <- getReactableState("L1.2", "selected")
    
    data$v_L1.2 <- 
      (data$df_L1.1 |>
        filter(sale) |>
        summarise(
          val = round(sum(value) / 100),
          n = sum(amount),
          .by = c(offerId, desc)
        ))[selected, "offerId"][[1]]
  })
  
  observe({
    req(data$df_L1.1)
    req(data$v_L1.2)
    x <- event_data("plotly_selected", source = "line")
    
    if (length(x) == 0) {
      data$df_L2.2 <- 
        data$df_L1.1 |>
          filter(offerId == data$v_L1.2)
    } else {
      x <- x |> pull("x") |> as.Date()
      x <- c(min(x, na.rm = T), max(x, na.rm = T))
      
      data$df_L2.2 <- 
        data$df_L1.1 |>
        mutate(created = as.Date(created)) |>
          filter(
            created >= x[1], created <= x[2],
            offerId == data$v_L1.2
          )
    }
  })
  
  observe({
    req(event_data("plotly_click", source = "bar"))
    
    x <- event_data("plotly_click", source = "bar")$customdata
    data$df_L3.1 <-  
      data$dfRaw |> filter(playerName %in% x) 
  })
  
# ----------- Rendering Objects
  
  output$L1.1 <- renderReactable({
    req(data$dfRaw)
    data$dfRaw |>  scatter_products_attr()
  })
  
  output$L1.2 <- renderReactable({
    req(data$df_L1.1)
    data$df_L1.1 |>  table_products_attr()
  })
  
  
  
  output$L2.1 <- renderPlotly({
    req(data$v_L1.2)
    
    data$dfRaw |>  bar_products_contr(data$v_L1.2)
  })
  
  output$L2.2 <- renderPlotly({
    req(data$v_L1.2)
    
    data$df_L1.1 |>
      filter(offerId == data$v_L1.2) |>
      line_products_trend()
  })
  
  output$L3.1 <- renderPlotly({
    req(data$df_L2.2)
    
    data$df_L2.2 |> bar_players_top()
  })
  
  output$L3.2 <- renderPlotly({
    req(data$df_L3.1)
    
    data$df_L3.1 |> waterfall_players_history()
  })
}

shinyApp(ui, server)
