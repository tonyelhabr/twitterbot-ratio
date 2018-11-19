

# tl_self <- .get_tl_self()
# tl_self
# status_id_destroy <- tl_self %>% slice(2) %>% pull(status_id)
# rtweet::post_tweet(destroy_id = status_id_destroy)

?rtweet::lists_members

list_sen <-
  rtweet::lists_members(slug = "senators", owner_user = "cspan")
list_sen
list_gov2 <-
  rtweet::lists_members(slug = "governors", owner_user = "cspan")
list_gov
