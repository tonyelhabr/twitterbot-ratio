
ratio_log_scrape <- import_ratio_log_scrape()
user_info # <- import_user_info()

# ratio_log_scrape
ratio_log_scrape %>% count(user, sort = TRUE)
ratio_log_tidy <-
  ratio_log_scrape %>%
  left_join(
    user_info %>%
      # select(user, n_follower = followers_count, n_tweet = statuses_count)
      select(user, followers_count, statuses_count)
  ) %>%
  group_by(user) %>%
  mutate_at(vars(created_at), funs(max(.), min(.), n())) %>%
  # select(max, min, n, everything()) %>%
  mutate(hour_between = (difftime(max, min, units = "hours") / n) %>% as.numeric()) %>%
  # select(hour_between, everything()) %>%
  summarise_at(vars(matches("count|between")), funs(mean)) %>%
  ungroup() %>%
  mutate(ratio = reply_count / (retweet_count + favorite_count)) %>%
  filter(!is.na(ratio)) %>%
  gather(metric, value, matches("count"))

ratio_log_wide <-
  ratio_log_tidy %>%
  spread(metric, value) %>%
  # arrange(desc(reply_count)) %>%
  arrange(desc(ratio)) %>%
  mutate_at(vars(matches("ratio")), funs(round(., 3L))) %>%
  mutate_at(vars(matches("between")), funs(round(., 1L))) %>%
  mutate_at(vars(matches("count")), funs(round(., 0L)))
ratio_log_wide

ratio_log_wide %>% filter(ratio > 1)
ratio_log_wide %>% arrange(ratio)

ratio_log_wide %>%
  filter(hour_between < 24) %>%
  filter(ratio < 1) %>%
  filter(followers_count > 1e6) %>%
  ggplot(aes(hour_between, ratio)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_smooth(method = "loess", color = "blue") +
  teplot::theme_te()

ratio_log_tidy %>%
  ggplot(aes(x = value, fill = metric)) +
  # geom_density() +
  geom_histogram(bins = 10) +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~metric, scales = "fixed") +
  teplot::theme_te()
