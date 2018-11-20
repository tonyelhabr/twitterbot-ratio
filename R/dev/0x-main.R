
library("tidyverse")

.user <- "RealSkipBayless"
.n_last <- 2L
tl_raw <-
  rtweet::get_timeline(
    user = .user,
    n = .n_last
  )
tl_raw %>% glimpse()
tl_raw_last <-
  tl_raw %>%
  slice(2)
tl_raw_last$status_id
tl_raw_last$user_id
tl_raw_last$created_at

q_search <- glue::glue("@{.user} OR to:{.user} OR {.user}")
q_search

reply_raw <-
  rtweet::search_tweets(
    q = q_search,
    # n = 1000L,
    include_rts = FALSE,
    since_id = tl_raw_last$status_id,
    retryonratelimit = FALSE
  )
reply_raw
reply_raw %>% 
  select(created_at) %>% 
  arrange(created_at) %>% 
  ggplot(aes(x = created_at)) +
  geom_density()

reply_cnt <-
  reply_raw %>% 
  filter(reply_to_status_id %in% tl_raw_last$status_id)
reply_cnt

n_replies <-
  replies_raw %>% 
  count() %>% 
  pull(n)
tl_raw_last$retweet_count
tl_raw_last$favorite_count
ratio <-
  n_replies / tl_raw_last$favorite_count
ratio
