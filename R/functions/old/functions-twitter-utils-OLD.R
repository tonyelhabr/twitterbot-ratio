
..get_tl_df_default <-
  function(...) {
    tibble(
      user_id = character(),
      screen_name = character(),
      created_at = lubridate::as_datetime(character()),
      status_id = character(),
      favorite_count = integer(),
      retweet_count = integer(),
      text = character()
    ) %>%
      .reorder_tl_cols_at()
  }

..ratio_scrape_df_default <-
  function(...) {
    bind_cols(
      ..get_tl_df_default(),
      tibble(
        reply_count = double(),
        ratio = double(),
        ratio_inv = double(),
        timestamp_scrape = lubridate::as_datetime(character())
      )
    ) %>%
      .reorder_ratio_cols_at()
  }


.validate_ratio_last_df <-
  function(ratio_last, screen_name, ...) {
    n_status_id <-
      ratio_last %>%
      count(status_id) %>%
      filter(n > 1L)
    if(ifelse(nrow(n_status_id) > 0L, TRUE, FALSE)) {
      msg <- sprintf("Expected 1 `status_id`. Instead, found %s statuses for `%s`.", n_status_id, !!screen_name)
      stop(msg, call. = FALSE)
    }
  }

.unconvert_id_cols_at <-
  function(data, cols = str_subset(names(data), "user_id|status_id"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character))
  }

.unconvert_datetime_cols <-
  function(data, cols = str_subset(names(data), "^created_at$|^timestamp"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character))
  }
