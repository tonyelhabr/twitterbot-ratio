
# tl_self <- .get_tl_self()
# tl_self
# status_id_destroy <- tl_self %>% slice(2) %>% pull(status_id)
# rtweet::post_tweet(destroy_id = status_id_destroy)

# # Reference: https://github.com/mkearney/cspan_data/blob/master/make.R.
# .get_cspan_list <- function(slug) {
#   x <- rtweet::lists_members(slug = slug, owner_user = "CSPAN")
#   x$cspan_list <- slug
#   x
# }
# cols_user_info <-
#   names(cspan_data)
# cols_user_info %>% paste(collapse = ",", sep = "")
#
# ## members of congress
# cspan_data <-
#   # c("members-of-congress", "the-cabinet", "governors") %>%
#   c("governors", "senators") %>%
#   purrr::map(.get_cspan_list) %>%
#   dplyr::bind_rows(cspan_data)
# cspan_data
#
# list_sen <-
#   rtweet::lists_members(slug = "senators", owner_user = "cspan")
# list_sen
# list_gov2 <-
#   rtweet::lists_members(slug = "governors", owner_user = "cspan")
# list_gov

ratio_log_scrape <- import_ratio_log_scrape()
user <-
  ratio_log_scrape %>%
  tetidy::pull_distinctly(user)
user <- get_user_toscrape()
user_info <-
  user %>%
  rtweet::lookup_users() %>%
  select(user, followers_count, statuses_count, name, description)
user_info
user_info %>% teproj::export_path(config$path_user_info)

