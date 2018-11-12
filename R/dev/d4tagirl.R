

library("tidyverse")

tweets_rstats <- rtweet::search_tweets("#rstats", n = 100, include_rts = FALSE)
tweets_rstats <-
  tweets_rstats %>%
  distinct()
tweets_rstats
orig_tweets <-
  tweets_rstats %>%
  filter(is.na(reply_to_status_id),
         favorite_count > 1) %>%
  select(status_id, screen_name, text, favorite_count, retweet_count) %>%
  distinct()

orig_tweets_mentions <-
  orig_tweets %>%
  distinct(screen_name) %>%
  mutate(
    query = paste0("@", screen_name, " OR ", "to:", screen_name, " OR ", screen_name)) %>%
  mutate(tweets = pmap(
    list(
      q = .$query,
      n = 1,
      retryonratelimit = TRUE
    ),
    rtweet::search_tweets
  )) %>%
  select(tweets) %>%
  unnest()
