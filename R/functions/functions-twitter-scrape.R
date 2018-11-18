
.make_twitter_url_reply <-
  function(.screen_name, .status_id, ...) {
    sprintf("https://twitter.com/%s/status/%s", .screen_name, .status_id)
  }

.make_twitter_selector_reply <-
  function(.status_id, ...) {
    sprintf("#profile-tweet-action-reply-count-aria-%s", .status_id)
  }

.get_reply_count_hack <-
  function(screen_name, status_id, ..., sleep = TRUE, time = 0.1) {

    stopifnot(length(screen_name) == 1L, length(status_id) == 1L)
    url <- .make_twitter_url_reply(.screen_name = screen_name, .status_id = status_id)
    selector <- .make_twitter_selector_reply(.status_id = status_id)

    reply_count <-
      url %>%
      xml2::read_html() %>%
      rvest::html_nodes(css = selector) %>%
      rvest::html_text() %>%
      str_replace_all("[^[0-9]]", "") %>%
      as.integer()
    if(sleep) {
      Sys.sleep(time)
    }
    reply_count
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
      )
  }

.add_score_cols_at <-
  function(data, ...) {
    data %>%
      mutate(
        considered = 0L,
        posted = 0L,
        status_id_post = NA_character_,
        text_post = NA_character_,
        timestamp_posted = NA_character_
      )
  }

.get_tl_first <-
  function(user, ..., n = config$n_tl_new, token = config$token, verbose = config$verbose_scrape) {
    if(verbose) {
      msg <- sprintf("Getting last %d tweets from timeline for \"%s\".", n, user)
      message(msg)
    }

    suppressMessages(
      rtweet::get_timeline(user = user, n = n, token = token, ...) %>%
        .filter_tweet_type() %>%
        .select_tl_cols_at()
    )
  }

.get_tl_first_possibly <-
  purrr::possibly(.get_tl_first, otherwise = NULL)

.get_tl_self <-
  purrr::partial(.get_tl_first, user = "punditratio")

.get_tl_self_possibly <- purrr::possibly(.get_tl_self, otherwise = NULL)

.get_tl_since <-
  function(user, since_id, ..., token = config$token, verbose = config$verbose_scrape) {
    if(verbose) {
      msg <- sprintf("Getting timeline for \"%s\" since last evaluated tweet: %s", user, since_id)
      message(msg)
    }

    suppressMessages(
      rtweet::get_timeline(user = user, since_id = since_id, token = token, ...) %>%
        .filter_tweet_type()
    )
  }

.get_tl_since_possibly <-
  purrr::possibly(.get_tl_since, otherwise = NULL)


.filter_tl_bytime <-
  function(data, n_hour_lag_scrape = config$n_hour_lag_scrape, ...) {
    data %>%
      filter(created_at <= (.TIME - lubridate::hours(n_hour_lag_scrape)))
  }

.do_scrape_ratio <-
  function(screen_name,
           tl = NULL,
           since_id = NULL,
           ratio_log_scrape = NULL,
           ratio_last_scrape = NULL,
           ...,
           cache = config$tl_cache,
           verbose = config$verbose_scrape) {

    # screen_name = "RealSkipBayless"
    # tl = NULL
    # since_id = NULL
    # ratio_log_scrape = NULL
    # ratio_last_scrape = NULL
    # cache = config$cache
    # verbose = config$verbose_scrape

    # message(rep("-", getOption("width")))
    message(rep("-", 80L))
    .validate_screen_name_1(screen_name)

    if (is.null(ratio_log_scrape)) {
      ratio_log_scrape <- .import_ratio_log_scrape_possibly()
      if (is.null(ratio_log_scrape)) {
        msg <-
          paste0(
            "Is this the first time you are doing this? ",
            "(If so, you should create the `ratio_log_scrape` file explicitly.)"
          )
        stop(msg, call. = FALSE)
      }
    }

    .validate_ratio_df_robustly(ratio_log_scrape)

    if (is.null(ratio_last_scrape)) {
      ratio_last_scrape <- .import_ratio_last_scrape_possibly()

      if (is.null(ratio_last_scrape)) {
        if (verbose) {
          msg <-
            sprintf("Creating missing `ratio_last_scrape` file from `ratio_log_scrape`.")
          message(msg)
        }
        ratio_last_scrape <- .convert_ratio_log_scrape_to_last_scrape(ratio_log_scrape)
        export_ratio_last_scrape(ratio_last_scrape)
      }
    }

    .compare_n_row_le(
      data1 = ratio_last_scrape,
      data2 = ratio_log_scrape
    )
    .validate_ratio_df_robustly(ratio_last_scrape)
    .validate_ratio_onerowpergrp_df(ratio_last_scrape)

    if (is.null(tl)) {
      if (is.null(since_id)) {
        ratio_last_scrape_filt <-
          ratio_last_scrape %>%
          filter(screen_name == !!screen_name)
        if(nrow(ratio_last_scrape_filt) > 0L) {
          since_id <-
            ratio_last_scrape_filt %>%
            pull(status_id)
          tl <- .get_tl_since_possibly(user = screen_name, since_id = since_id)
        } else {
          tl <- .get_tl_first_possibly(user = screen_name)
        }
      }

      if(nrow(tl) == 0L) {
        msg <- sprintf("Did not find any tweets for \"%s\".", screen_name)
        message(msg)
        return(NULL)
      }

      if(is.null(tl)) {
        msg <- sprintf("Something went wrong when retrieving tweets for \"%s\".", screen_name)
        message(msg)
        return(NULL)
      }
    }

    .validate_tl_df(tl)

    tl <- .select_tl_cols_at(tl)
    tl_filt <- .filter_tl_bytime(tl)

    n_row_tl_filt <- nrow(tl_filt)
    if (verbose) {
      if(n_row_tl_filt == 0L) {
        msg <- sprintf("No tweets to evaluate for %s.", screen_name)
        message(msg)
        return(NULL)
      }
      msg <- sprintf("Evaluating %s tweet(s) for %s.", n_row_tl_filt, screen_name)
      message(msg)
    }

    if(cache) {
      path_tl_cache <- .export_tl_cache(tl_filt, screen_name)
    }

    reply <-
      tl_filt %>%
      mutate(
        reply_count =
          purrr::pmap_int(
            list(screen_name, status_id),
            ~.get_reply_count_hack(screen_name = ..1, status_id = ..2)
            )
      )

    ratio_log_scrape_export <-
      reply %>%
      .add_ratio_cols_at() %>%
      .add_timestamp_scrape_col_at() %>%
      .add_score_cols_at() %>%
      arrange(created_at)

    path_ratio_log_scrape <- export_ratio_log_scrape_scrape(ratio_log_scrape_export)

    ratio_last_scrape_export <-
      bind_rows(
        ratio_last_scrape,
        ratio_log_scrape_export
      ) %>%
      .convert_ratio_log_scrape_to_last_scrape()

    path_ratio_last_scrape <- export_ratio_last_scrape(ratio_last_scrape_export)

    invisible(reply)
  }

do_scrape_ratio_all <-
  function(screen_name = NULL, ...) {
    if(is.null(screen_name)) {
      screen_name <- get_screen_name_toscrape()
    }
    .validate_screen_name_vector(screen_name)
    .create_backup(path = config$path_ratio_log_scrape)
    purrr::walk(screen_name, ~.do_scrape_ratio(screen_name = .x))
  }

