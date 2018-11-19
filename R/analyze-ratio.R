

ratio_log_scrape <- import_ratio_log_scrape()
# ratio_log_scrape
ratio_log_scrape %>% count(screen_name, sort = TRUE)
ratio_log_tidy <-
  ratio_log_scrape %>%
  group_by(screen_name) %>%
  mutate_at(vars(created_at), funs(max(.), min(.), n())) %>%
  # select(max, min, n, everything()) %>%
  mutate(hour_between = (difftime(max, min, units = "hours") / n) %>% as.numeric()) %>%
  # select(hour_between, everything()) %>%
  summarise_at(vars(matches("count|between")), funs(mean)) %>%
  ungroup() %>%
  mutate(ratio = reply_count / (retweet_count + favorite_count)) %>%
  filter(!is.na(ratio)) %>%
  gather(metric, value, matches("count"))

ratio_log_tidy_pretty <-
  ratio_log_tidy %>%
  spread(metric, value) %>%
  arrange(desc(reply_count)) %>%
  mutate_at(vars(matches("ratio")), funs(sprintf("%0.3f", .))) %>%
  mutate_at(vars(matches("between")), funs(sprintf("%0.1f", .))) %>%
  mutate_if(is.numeric, funs(pretty = scales::comma(.))) %>%
  mutate_if(is.numeric, funs(round(., 0L)))

ratio_log_tidy %>%
  ggplot(aes(x = value, fill = metric)) +
  # geom_density() +
  geom_histogram(bins = 10) +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~metric, scales = "fixed") +
  teplot::theme_te()
