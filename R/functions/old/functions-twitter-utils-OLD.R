
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

..get_ratio_df_default <-
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
