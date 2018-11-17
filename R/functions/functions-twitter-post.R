

.post_ratio <-
  function(screen_name,
             status_id,
             ratio,
             ...,
             # update = TRUE,
             # reply = TRUE,
             # retweet = TRUE,
             # favorite = TRUE,
             update = FALSE,
             reply = FALSE,
             retweet = FALSE,
             favorite = FALSE,
             token = config$token,
             verbose = config$verbose_post) {
    text_post <-
      sprintf(
        "Congratulations @%s on your ratio of %.02f!",
        screen_name,
        ratio
      )
    if (favorite) {
      # TODO: Check if this actually returns a status.
      status_id_post <- rtweet::post_favorite(status_id, token = token)
    }
    if (retweet && !reply) {
      status_id_post <-
        rtweet::post_tweet(
          status = text_post,
          retweet_id = status_id,
          token = token
        )
    }
    if (reply) {
      status_id_post <-
        rtweet::post_tweet(
          status = text_post,
          retweet_id = status_id,
          token = token
        )
      if (verbose) {
        msg <- sprintf("Posted the following tweet at %s:\n\"%s.\"", Sys.time(), text_post)
        message(msg)
      }
    }
    # Debuggning...
    # status_id_post <- status_id
    status_id_post <- NULL
    # invisible(status_id_post)

    invisible(tibble(
      status_id_post = !!status_id_post,
      text_post = !!text_post
    ))
  }

.add_timestamp_post_col_at <-
  purrr::partial(.add_timestamp_scrape_col_at, col = "timestamp_post")

do_post_ratio <-
  function(screen_name = NULL, ratio_log = NULL, ...) {

    # screen_name = NULL
    # ratio_log = NULL

    message(rep("-", 80L))
    stopifnot(length(screen_name) == 1L)

    if (is.null(ratio_log)) {
      ratio_log <- .import_ratio_log_possibly()
      if (is.null(ratio_log)) {
        msg <- sprintf("`ratio_log` cannot be found.")
        stop(msg, call. = FALSE)
      }
    }

    ratio_log_filt <- ratio_log
    if (!is.null(screen_name)) {
      ratio_log_filt <-
        ratio_log_filt %>%
        filter(screen_name == !!screen_name)
    }

    .validate_ratio_onerow_df(ratio_log_filt)

    ratio_log_filt <-
      ratio_log_filt %>%
      filter(!considered, !posted) %>%
      mutate(considered = 1L)

    ratio_topost <-
      ratio_log_filt %>%
      filter(ratio == max(ratio, na.rm = TRUE))

    ratio_notposted <-
      ratio_log_filt %>%
      anti_join(ratio_topost)

    ratio_wasposted_raw <-
      ratio_topost %>%
      mutate(
        data =
          purrr::pmap(
            list(screen_name, status_id, ratio),
            ~.post_ratio(screen_name = ..1, status_id = ..2, ratio = ..3)
          )
      )

    ratio_wasposted <-
      ratio_wasposted_raw %>%
      select(-status_id_post, -text_post) %>%
      unnest(data) %>%
      # mutate(posted = 1L)
      mutate(posted = 1L, timestamp_posted = Sys.time())
    # NOTE: The columns of `ratio_notposted` should already be in the correct order.
    ratio_notposted <- ratio_notposted %>% .select_ratio_cols_at()
    ratio_wasposted <- ratio_wasposted %>% .select_ratio_cols_at()

    ratio_log_export <-
      ratio_log %>%
      mutate(rn = row_number()) %>%
      select(one_of(c("rn", .COLS_RATIO_BASE_ORDER))) %>%
      left_join(
        bind_rows(
          ratio_notposted,
          ratio_wasposted
        )
      ) %>%
      arrange(rn) %>%
      select(-rn)

    stopifnot(nrow(ratio_log) != nrow(ratio_log_export))

    path_ratio_log <- export_ratio_log(ratio_log_export)

    ratio_last_export <-
      ratio_log_export %>%
      .convert_ratio_log_to_last()

    path_ratio_last <- export_ratio_last(ratio_last_export)

    invisible(ratio_post)
  }
