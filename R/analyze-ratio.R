
ratio_log_scrape <- import_ratio_log_scrape()
user_info <- import_user_info()

# ratio_log_scrape
ratio_log_scrape %>% count(user, sort = TRUE)

.frac <- 0.0001
# .frac <- 0.001
# .frac <- 0.0005
# .frac
ratio_log_aug <-
  ratio_log_scrape %>%
  # .add_ratio_numden_cols() %>%
  left_join(
    user_info %>%
      select(user, followers_count, statuses_count),
    by = "user"
  ) %>%
  mutate(
    ratio_den = (favorite_count + retweet_count),
    ratio_num = reply_count
  ) %>%
  mutate(
    ratio_den_frac = ratio_den / followers_count,
    ratio_num_frac = ratio_num / followers_count
  ) %>%
  mutate(
    ratio_den_exc = if_else(ratio_den_frac >= .frac, 1L, 0L),
    ratio_num_exc = if_else(ratio_num_frac >= .frac, 1L, 0L)
  )

ratio_log_aug %>%
  filter(!is.na(ratio_den_exc)) %>%
  .count_frac(ratio_den_exc)

# tone <- import_tone()
user <- import_user()
ratio_log_aug_proc <-
  ratio_log_aug %>%
  filter(!is.na(ratio_den_exc)) %>%
  count(user, ratio_den_exc) %>%
  group_by(user) %>%
  .add_frac_cols(n) %>%
  ungroup() %>%
  spread(ratio_den_exc, n, fill = 0) %>%
  # rename_at(vars(matches("0|1")), funs(paste0("x", .))) %>%
  left_join(user, by = c("user")) %>%
  mutate_at(vars(category1), funs(if_else(. == "politics", ., "non-politics")))
ratio_log_aug_proc

..visualize_ratio_log_proc <-
  function(.data, col, ...) {
    col_enquo <- enquo(col)
    .data %>%
      ggplot(aes(x = `1`, fill = !!col_enquo)) +
      geom_histogram(binwidth = 0.1) +
      facet_wrap(vars(!!col_enquo), scales = "free") +
      teplot::theme_te() +
      expand_limits(x = c(0, 1)) +
      labs(y = NULL, x = NULL)
  }
ratio_log_aug_proc %>% ..visualize_ratio_log_proc(category1)
ratio_log_aug_proc %>% ..visualize_ratio_log_proc(user_tone)
ratio_log_aug_proc %>% ..visualize_ratio_log_proc(audience_tone)

ratio_log_aug %>%
  filter(!is.na(ratio_den_exc)) %>%
  .count_frac(user, ratio_den_exc)

# .user_ratio_lo <- "PFTCommenter"
# .user_ratio_hi <- "DalrympleforGov"
# .user_ratio_lohi <- c(.user_ratio_lo, .user_ratio_hi)
# ratio_log_aug %>%
#   .count_frac(ratio_den_exc)
# bind_rows(
#   ratio_log_aug %>%
#     filter(user %in% .user_ratio_lo) %>%
#     .count_frac(user, ratio_den_exc),
#   ratio_log_aug %>%
#     filter(user %in% .user_ratio_hi) %>%
#     .count_frac(user, ratio_den_exc)
# )


ratio_log_tidy <-
  ratio_log_aug %>%
  select(-matches("ratio")) %>%
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
  mutate_at(vars(matches("ratio")), funs(round(., 5L))) %>%
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
