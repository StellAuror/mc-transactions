print("Loading libraries...")
pacman::p_load(
  jsonlite,
  bslib,
  dplyr,
  ggplot2,
  tidyr,
  lubridate,
  stringdist,
  reactable,
  htmltools,
  plotly,
  fs,
  shinyWidgets,
  shiny
)

print("Libraries Loaded.")
      
print("Loading Modules...")
lapply(dir_ls( "modules", glob = "*.R"), source)
print("Modules Loaded.")

