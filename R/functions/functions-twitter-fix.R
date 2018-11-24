
.rebind_ratio_log_scrape <-
  function(x,
           y,
           ...,
           how = "left",
           by = setdiff(.COLS_RATIO_BASE_ORDER, setdiff(names(x), names(y))),
           verbose = FALSE) {
    # Note: This verbose is essentially a replacement for the regular message
    # output by `dplyr::*join()` when `by` isn't specified.
    if (verbose) {
      msg <-
        paste0("Joining by `", paste(by, collapse = "`, `", sep = ""), "`.")
      message(msg)
    }
    f_join <- sprintf("%s_join", how)
    suppressWarnings(y <- y %>% .select_ratio_cols_at())
    x <- x %>% mutate(rn = row_number())

    res <-
      purrr::invoke(
        f_join,
        x = x,
        y = y,
        by = by,
        suffix = c("", "_y")
      )
    res %>%
      mutate(
        considered = coalesce(considered_y, considered),
        posted = coalesce(posted_y, posted),
        status_id_post = coalesce(status_id_post_y, status_id_post),
        text_post = coalesce(text_post_y, text_post),
        timestamp_post = coalesce(timestamp_post_y, timestamp_post)
      ) %>%
      arrange(rn) %>%
      select(-rn) %>%
      .select_ratio_cols_at()
  }

.rebind_ratio_log_scrape_loosely <-
  purrr::partial(.rebind_ratio_log_scrape, how = "left", by = .COLS_RATIO_BASE_ORDER)
.rebind_ratio_log_scrape_strictly <-
  purrr::partial(.rebind_ratio_log_scrape, how = "inner")

.refresh_ratio_log_scrape <-
  function(tweets_self = NULL, ratio_log_scrape = NULL, ..., verbose = TRUE) {
    if(is.null(tweets_self)) {
      tweets_self <- .get_tweets_self()
    }
    # favs_self <- tweets_self %>% filter(!favorited_by %>% is.na())
    # rt_self <- tweets_self %>% filter(is_retweet)
    post_self <- tweets_self %>% filter(str_detect(text, "^Congratulations"))
    # other_self <-
    #   bind_rows(
    #     favs_self,
    #     rt_self,
    #     post_self
    #   ) %>%
    #   select_if(~!is.list(.)) %>%
    #   anti_join(tweets_self %>% select_if(~!is.list(.)))
    if(is.null(ratio_log_scrape)) {
      ratio_log_scrape <- import_ratio_log_scrape()
    }

    posted_self <-
      post_self %>%
      rename(
        text_post = text,
        status_id_post = status_id,
        timestamp_post = created_at
      ) %>%
      select(-user_id, -user, -favorite_count, -retweet_count) %>%
      rename(
        user_id = quoted_user_id,
        user = quoted_screen_name,
        status_id = quoted_status_id #,
        # Note: Could also join these, but it might cause "mismatches".
        # (Haven't tested this though.)
        # created_at = quoted_created_at,
        # favorite_count = quoted_favorite_count,
        # retweet_count = quoted_retweet_count,
        # text = quoted_text
      ) %>%
      mutate(considered = 1L, posted = 1L) %>%
      .select_ratio_cols_at()

    ratio_log_scrape_distinct <-
      ratio_log_scrape %>%
      distinct(text, .keep_all = TRUE)

    ratio_log_rebind <-
      .rebind_ratio_log_scrape_strictly(
        x = ratio_log_scrape_distinct,
        y = posted_self
      )

    .compare_n_row_eq(
      data1 = ratio_log_rebind,
      data2 = posted_self
    )

    ratio_log_rebind_export <-
      .rebind_ratio_log_scrape_loosely(
        x = ratio_log_scrape,
        y = ratio_log_rebind
      )

    .compare_n_row_eq(
      data1 = ratio_log_rebind_export,
      data2 = ratio_log_scrape
    )

    path_ratio_log_export <- export_ratio_log_scrape(ratio_log_rebind_export)
    invisible(ratio_log_rebind_export)

  }

refresh_ratio_log_scrape <-
  function(...,
           path = config$path_ratio_log_scrape,
           backup = TRUE,
           clean = TRUE,
           n_keep = 10, # Note: This is passed to `.clean_backup()` in `.create_backup()`.
           verbose = TRUE) {
    if (backup & !is.null(path)) {
      .create_backup(path = path, clean = clean, n_keep = 10)
    }
    .refresh_ratio_log_scrape(...)
  }
