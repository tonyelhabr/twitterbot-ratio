

screen_name <- config$screen_name
screen_name <- "bomani_jones"
purrr::walk(
  screen_name,,
  ~do_get_ratio(.x)
)
