
# TODO: Programmatically figure out .N_SCREEN_NAME?
# .N_SCREEN_NAME <- 3L
# .N_SCREEN_NAME <- ifelse(file.exists(config$path_last), import_ratio_last() %>% nrow(), 3L)
.N_SCREEN_NAME <- 4L
.MAX_PER_SESSION <- 18000L
# .N_SCALE <- 1L
.N_SCALE <- 1L / (.N_SCREEN_NAME)
.N_SEARCH <- .N_SCALE * .MAX_PER_SESSION %>% floor() %>% as.integer()
# .N_SEARCH <- .MAX_PER_SESSION

.get_replies_since <-
  function(screen_name,
           since_id,
           ...,
           q = sprintf("@%s OR to:%s OR %s", screen_name, screen_name, screen_name),
           # n = 10000L,
           n = .N_SEARCH,
           include_rts = FALSE,
           retryonratelimit = TRUE,
           verbose = config$verbose) {

    rtweet::search_tweets(
      q = q,
      since_id = since_id,
      ...,
      n = n,
      include_rts = include_rts,
      retryonratelimit = retryonratelimit,
      verbose = TRUE
    )
  }

.get_reply_count <-
  function(data, status_id, ...) {

    data %>%
      filter(reply_to_status_id == !!status_id) %>%
      count() %>%
      pull(n)
  }

.filter_tweet_type <-
  function(data, ..., .n = NULL) {
    res <-
      data %>%
      filter(!is_retweet, is.na(reply_to_status_id)) %>%
      arrange(created_at)
    if(!is.null(.n)) {
      res <-
        res %>%
        filter(row_number() <= .n)
    }
    res %>%
      ungroup() %>%
      arrange(screen_name, created_at)
  }


.add_ratio_cols_at <-
  function(data, ...) {
    data %>%
      mutate(
        # ratio_reply2fav = reply_count / favorite_count,
        # ratio_reply2retweet = reply_count / retweet_count,
        ratio = reply_count / (favorite_count + retweet_count)
      ) %>%
      mutate(
        ratio_inv = coalesce(1 / ratio, 0)
      )
  }

.convert_ratio_log_to_last <-
  function(data, ...) {
    data %>%
      # filter(!is.na(ratio_inv)) %>%
      group_by(screen_name) %>%
      arrange(desc(created_at), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(screen_name, desc(created_at))
  }

.get_tl <-
  function(user, since_id, ..., verbose = config$verbose) {
    if(verbose) {
      msg <- sprintf("Getting timeline for %s since last evaluated tweet: %s", user, since_id)
      message(msg)
    }

    rtweet::get_timeline(user = user, since_id = since_id, ...) %>%
      .filter_tweet_type() %>%
      .select_tl_cols_at()
  }


.validate_twitter_df <-
  function(data, cols, ...) {
    stopifnot(is.data.frame(data))
    # stopifnot(nrow(data) > 0L)
    stopifnot(all(cols %in% names(data)))
  }

.validate_ratio_df <- purrr::partial(.validate_twitter_df, cols = .COLS_RATIO_ORDER)
.validate_tl_df <- purrr::partial(.validate_twitter_df, cols = .COLS_TL_ORDER)

.validate_ratio_last_df <-
  function(ratio_last, cols = .COLS_RATIO_ORDER, ...) {
    .validate_twitter_df(data = ratio_last, cols = cols)
    ratio_last_cnt <-
      ratio_last %>%
      count(screen_name, status_id) %>%
      filter(n > 1L)
    cnd <- ifelse(nrow(ratio_last_cnt) > 0L, TRUE, FALSE)
    if(cnd) {
      msg_fmt <- "Expected 1 `status_id` for each `screen_name`. Found %s statuses for `%s`."
      ratio_last_cnt %>%
        mutate(msg = sprintf(msg_fmt, n, screen_name)) %>%
        pull(msg) %>%
        purrr::walk(stop, .call = FALSE)
    }
  }

.filter_tl_bytime <-
  function(data, n_hour_lag = config$n_hour_lag, ...) {
    data %>%
      filter(created_at <= (.TIME - lubridate::hours(n_hour_lag)))
  }

do_get_ratio1 <-
  function(screen_name,
           tl = NULL,
           # tl_cache = NULL, # TODO.
           since_id = NULL,
           ratio_log = NULL,
           ratio_last = NULL,
           ...,
           verbose = config$verbose) {

    # screen_name = "bomani_jones"
    # tl = NULL
    # since_id = NULL
    # ratio_log = NULL
    # ratio_last = NULL
    # verbose = config$verbose

    # message(rep("-", getOption("width")))
    message(rep("-", 80L))

    stopifnot(length(screen_name) == 1L)

    if (is.null(ratio_log)) {
      ratio_log <- purrr::possibly(import_ratio_log, otherwise = NULL)()
      if (is.null(ratio_log)) {
        msg <- "Is this the first time you are doing this?"
        stop(msg, call. = FALSE)
      }
    }

    ratio_last <-
      purrr::possibly(import_ratio_last, otherwise = NULL)()

    if (is.null(ratio_last)) {
      ratio_last <- .convert_ratio_log_to_last(ratio_log)
      if (verbose) {
        msg <-
          sprintf("Creating missing `ratio_last` file from `ratio_log`.")
        message(msg)
      }
      export_ratio_last(ratio_last)
    }

    # TODO: Replace this with something like the following?
    .validate_ratio_last_df(ratio_last)
    # n_status_id <-
    #   ratio_last %>%
    #   count(status_id) %>%
    #   filter(n > 1L)
    # if(ifelse(nrow(n_status_id) > 0L, TRUE, FALSE)) {
    #   msg <- sprintf("Expected 1 `status_id`. Instead, found %s statuses for `%s`.", n_status_id, screen_name)
    #   stop(msg, call. = FALSE)
    # }

    if (is.null(tl)) {
      if (is.null(since_id)) {
        since_id <-
          ratio_last %>%
          filter(screen_name == !!screen_name) %>%
          pull(status_id)
      }
      .get_tl_possibly <-
        purrr::possibly(.get_tl, otherwise = NULL)
      tl <-
        .get_tl_possibly(user = screen_name, since_id = since_id)
      if(is.null(tl)) {
        msg <- sprintf("Did not find any tweets for %s.", screen_name)
        warning(msg)
        return(NULL)
      }
    }

    .validate_tl_df(tl)

    tl <- .select_tl_cols_at(tl)

    if (verbose) {
      n <- nrow(tl)
      msg <- sprintf("Scoring %s tweet(s) for %s.", n, screen_name)
      message(msg)
    }

    path_tl_cache <- export_tl_cache1(tl, screen_name)
    # NOTE: Not sure if this is a good idea...
    on.exit(path_tl_cache)

    reply_raw <-
      tl %>%
      arrange(created_at) %>%
      slice(1) %>%
      mutate(data =
               purrr::pmap(
                 list(screen_name, since_id),
                 ~ .get_replies_since(screen_name = ..1, since_id = ..2)
               ))

    reply <-
      reply_raw %>%
      select(data) %>%
      unnest(data)

    ratio <-
      tl %>%
      .filter_tl_bytime() %>%
      mutate(reply_count =
               purrr::map_int(status_id, ~ .get_reply_count(data = reply, status_id = .x)))

    ratio_log <-
      ratio %>%
      .add_ratio_cols_at() %>%
      .add_timestamp_scrape_col_at() %>%
      arrange(desc(ratio))

    path_ratio_log <- export_ratio_log(ratio_log)

    ratio_last_export <-
      bind_rows(
        ratio_last,
        ratio_log %>% .filter_tl_bytime()
      ) %>%
      .convert_ratio_log_to_last()

    path_ratio_last <- export_ratio_last(ratio_last_export)

    invisible(path_tl_cache)
  }
