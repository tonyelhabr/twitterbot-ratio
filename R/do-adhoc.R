

# # screen_name <- config$screen_name
# # screen_name <- "RealSkipBayless"
# screen_name <- setdiff(config$screen_name, "RealSkipBayless")
# purrr::walk(
#   screen_name,
#   ~do_scrape_ratio(.x)
# )

tl_self <- .get_tl_self_possibly()
tl_self
status_id_destroy <- tl_self %>% slice(2) %>% pull(status_id)
rtweet::post_tweet(destroy_id = status_id_destroy)
