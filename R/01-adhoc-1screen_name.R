
options(readr.num_columns = 20)
config$path_log %>% readr::read_csv()
ratio_log_import <-
  config$path_log %>% 
  readr::read_csv(
    col_types = 
      cols(
        user_id = col_character(),
        screen_name = col_character(),
        created_at = col_datetime(format = ""),
        status_id = col_character(),
        ratio = col_double(),
        ratio_inv = col_double(),
        favorite_count = col_integer(),
        retweet_count = col_integer(),
        reply_count = col_integer(),
        text = col_character(),
        timestamp_scrape = col_datetime(format = "")
      )
    )

ratio_log_import <- import_ratio_log()
.SCREEN_NAME <- "stephenasmith"
.TIME
.N_HOUR_LAG

ratio_log_filt <-
  ratio_log_import %>%
  filter(created_at <= (.TIME - lubridate::hours(.N_HOUR_LAG))) %>% 
  filter(screen_name %in% .SCREEN_NAME) %>% 
  .slice_ratio_df_at()
ratio_log_filt
.get_timeline_verbosely
ratio_log_filt %>% pull(status_id) %>% dput()
tl_raw <-
  ratio_log_filt %>% pull(status_id) %>% dput()
  # mutate(data = purrr::map2(screen_name, status_id, ~.get_timeline_verbosely_possibly(user = .x, since_id = .y)))
  # mutate(data = purrr::map2(screen_name, status_id, ~.get_timeline_verbosely(user = .x, since_id = .y)))
  mutate(data = purrr::map2(screen_name, status_id, ~rtweet::get_timeline(user = .x, max_id = .y, n = 10L)))
tl_raw %>%
  select(data) %>% 
  unnest(data) %>% 
  arrange(desc(created_at))

tl_raw <-
  # ratio_last_import %>%
  ratio_log_filt %>% 
  select(screen_name, status_id) %>% 
  mutate(
    data = 
      purrr::pmap(
        list(screen_name, status_id), 
        ~.get_timeline_verbosely_possibly(user = ..1, since_id = ..2)
      )
  )

tl <-
  tl_raw %>% 
  select(data) %>% 
  unnest(data)
