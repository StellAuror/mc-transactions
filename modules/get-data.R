# ------  Raw Data Processing
# gathering JSONs from a given directory
# and convering to tibble format 
get_data <- function(folder_path = "data") {
  print("Trying to load data...")
  
  list.files(folder_path, pattern = "\\.json$", full.names = TRUE) |>
    lapply(fromJSON) |>
    tibble(data = _) |>
    unnest_wider(data) |>
    mutate(
      elements = purrr::map(elements, as_tibble),
      created = ymd_hm(created)
    ) |>
    unnest(cols = c(elements)) -> df
  
  inner_join(
    df |>
      group_by(offerId) |>
      summarise(desc = find_subs(description)),
    df,
    join_by(offerId == offerId)
  ) |> arrange(created)
}
print("Data successfuly loaded.")

