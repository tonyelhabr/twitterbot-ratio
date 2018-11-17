

# screen_name <- config$screen_name
# screen_name <- "RealSkipBayless"
screen_name <- setdiff(config$screen_name, "RealSkipBayless")
purrr::walk(
  screen_name,
  ~do_scrape_ratio(.x)
)
