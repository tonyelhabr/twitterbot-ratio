
# TODO: Programmatically figure out .N_SCREEN_NAME?
# .N_SCREEN_NAME <- 3L
# .N_SCREEN_NAME <- ifelse(file.exists(config$path_last), import_ratio_last() %>% nrow(), 3L)
.N_SCREEN_NAME <- 3L
.MAX_PER_SESSION <- 18000L
# .N_SCALE <- 1L
.N_SCALE <- 1L / (.N_SCREEN_NAME)
.N_SEARCH <- .N_SCALE * .MAX_PER_SESSION %>% floor() %>% as.integer()
# .N_SEARCH <- .MAX_PER_SESSION

.get_replies_since <-
  function(screen_name,
           since_id,
           ...,
           # NOTE: Use `sprintf()` just to make everything consistent.
           # .q = glue::glue("@{screen_name} OR to:{screen_name} OR {screen_name}"),
           .q = sprintf("@%s OR to:%s OR %s", screen_name, screen_name, screen_name),
           .n = .N_SEARCH,
           # .n = 10000L,
           .include_rts = FALSE,
           .retryonratelimit = TRUE) {
    
    rtweet::search_tweets(
      q = .q,
      since_id = since_id,
      ...,
      n = .n,
      include_rts = .include_rts,
      retryonratelimit = .retryonratelimit
    )
  }

.get_reply_count <-
  function(data, status_id, ...) {
    
    data %>%
      filter(reply_to_status_id == !!status_id) %>% 
      count() %>% 
      pull(n)
  }


..filter_tl_df_at <-
  function(data, ..., .n = NULL) {
    res <-
      data %>%
      filter(!is_retweet, is.na(reply_to_status_id)) %>%
      group_by(screen_name) %>%
      arrange(created_at, .by_group = TRUE)
    if(!is.null(.n)) {
      res <-
        res %>%
        filter(row_number() <= .n)
    }
    res %>% 
      ungroup() %>%
      arrange(screen_name, created_at)
  }
# .filter_tl_df_at <- purrr::partial(..filter_tl_df_at, .n = .N_LAST)
.filter_tl_df_at <- ..filter_tl_df_at

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

.arrange_ratio_df_at <-
  function(data, ...) {
    data %>%
      group_by(screen_name) %>% 
      arrange(desc(ratio), .by_group = TRUE) %>% 
      ungroup()
  }

.slice_ratio_df_at <-
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
      .filter_tl_df_at() %>% 
      .select_tl_cols_at()
  }

.get_tl_possibly <-
  purrr::possibly(
    .f = .get_tl,
    # .f = rtweet::get_timeline,
    otherwise = ..get_tl_df_default(),
    quiet = FALSE
  )

.validate_twitter_df <-
  function(data, cols, ...) {
    stopifnot(is.data.frame(data))
    # stopifnot(nrow(data) > 0L)
    stopifnot(all(cols %in% names(data)))
  }

.validate_ratio_df <- purrr::partial(.validate_twitter_df, cols = .COLS_RATIO_ORDER)
.validate_tl_df <- purrr::partial(.validate_twitter_df, cols = .COLS_TL_ORDER)
.validate_ratio_last_df <-
  function(data, cols = .COLS_RATIO_ORDER, ...) {
    .validate_twitter_df(data = data, cols = cols)
    ratio_last_cnt <-
      ratio_last %>%
      count(screen_name, status_id) %>% 
      # group_by(screen_name, status_id) %>% 
      # summarise(nn = n()) %>% 
      # ungroup() %>% 
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

.describe_screen_name <-
  function(tl, ratio, ...) {
    
    nm_ratio <- ratio %>% .pull_distinctly(screen_name)
    nm_tl <- tl %>% .pull_distinctly(screen_name)
    nm_diff1 <- setdiff(nm_ratio, nm_tl)
    nm_diff2 <- setdiff(nm_tl, nm_ratio)
    if(length(nm_diff1) > 0L) {
      nm_diff_coll <- paste(nm_diff1, sep = "", collapse = ", ")
      msg <- sprintf("No new tweets to score for some names: %s.", nm_diff_coll)
      message(msg)
    }
    
    if(length(nm_diff2) > 0L) {
      nm_diff_coll <- paste(nm_diff2, sep = "", collapse = ", ")
      msg <- sprintf("New screen name(s) to evaluate: %s.", nm_diff_coll)
      message(msg)
    } else {
      msg <- "New tweets to score for all screen name(s) (and no new screen names)."
      message(msg)
    }
    invisible(tl)
  }

.describe_tl_before_ratio <-
  function(tl, ...) {
    tl_cnt <-
      tl %>%
      count(screen_name)
    tl_cnt %>% 
      mutate(msg = sprintf("Scoring %s tweet(s) for %s.", n, screen_name)) %>% 
      pull(msg) %>% 
      purrr::walk(message)
    invisible(tl)
  }

.N_HOUR_LAG <- 24L
.filter_ratio_df_at <-
  function(data, ...) {
    data %>% 
      filter(created_at <= (.TIME - lubridate::hours(.N_HOUR_LAG))) 
  }

do_get_ratio1 <-
  function(screen_name,
           tl = NULL,
           since_id = NULL,
           ratio_log = NULL,
           ratio_last = NULL,
           ...,
           verbose = config$verbose) {
    
    # screen_name <- "NateSilver538"
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
      ratio_last <- .slice_ratio_df_at(ratio_log)
      if (verbose) {
        msg <-
          sprintf("Creating missing `ratio_last` file from `ratio_log`.")
        message(msg)
      }
      export_ratio_last(ratio_last)
    }
    
    .validate_ratio_last_df(ratio_last)
    
    if (is.null(tl)) {
      if (is.null(since_id)) {
        since_id <-
          ratio_last %>%
          filter(screen_name == !!screen_name) %>%
          pull(status_id)
      }
      tl <-
        .get_tl_possibly(user = screen_name, since_id = since_id)
    }
    
    .validate_tl_df(tl)
    
    tl <- .select_tl_cols_at(tl)
    
    if (verbose) {
      # nms <- ratio_last %>% tetidy::pull_distinctly(screen_name)
      # cnd <- any(screen_name %in% nms)
      n <- nrow(tl)
      msg <- sprintf("Scoring %s tweet(s) for %s.", n, screen_name)
      message(msg)
    }
    
    export_tl_cache(t, screen_name)
    
    reply_raw <-
      tl %>%
      arrange(created_at, .by_group = TRUE) %>%
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
      mutate(reply_count =
               purrr::map_int(status_id, ~ .get_reply_count(data = reply, status_id = .x)))
    
    ratio_log <-
      ratio %>%
      .add_ratio_cols_at() %>%
      .add_timestamp_scrape_col_at() %>%
      .arrange_ratio_df_at() %>%
      .filter_ratio_df_at()
    
    export_ratio_log(ratio_log)
    
    ratio_last_export <-
      bind_rows(ratio_last,
                ratio_log) %>%
      .filter_ratio_df_at() %>%
      .slice_ratio_df_at()
    
    export_ratio_last(ratio_last)
    invisible(ratio_log)
  }

do_get_ratio_multi <-
  function(screen_name, ...) {
    
  }

do_get_ratio <-
  function(tl = NULL,
           ratio_last = NULL,
           ...,
           verbose = config$verbose) {
    if (is.null(ratio_last)) {
      ratio_last <- import_ratio_last()
    }
    
    .validate_ratio_last_df(ratio_last)
    if (is.null(tl)) {
      if (verbose) {
        msg <-
          "Retrieving latest tweets based on imported most recent ratios/statuses."
        message(msg)
      }
      tl_raw <-
        ratio_last %>%
        select(screen_name, status_id) %>%
        mutate(data =
                 purrr::pmap(
                   list(screen_name, status_id),
                   ~ .get_tl_possibly(user = ..1, since_id = ..2)
                 ))
      
      tl <-
        tl_raw %>%
        select(data) %>%
        unnest(data)
      tl
    }
    .validate_tl_df(tl)
    
    # NOTE: Do this to make sure that `tl` is trim (whether it is provided by the user
    # or generated for the condition `tl = NULL` (even if th `.get_tl_possibly()`
    # function already calls this function.)).
    tl <- .select_tl_cols_at(tl)
    if (verbose) {
      .describe_screen_name(tl = tl, ratio = ratio_last)
    }
    
    if (verbose) {
      .describe_tl_before_ratio(tl = tl)
    }
    tl %>% export_tl_cache()
    reply_raw <-
      tl %>%
      group_by(screen_name) %>%
      arrange(created_at, .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(data =
               purrr::pmap(
                 list(screen_name, status_id),
                 ~ .get_replies_since(screen_name = ..1, since_id = ..2)
               ))
    reply <-
      reply_raw %>%
      select(data) %>%
      unnest(data)
    
    ratio <-
      tl %>%
      mutate(reply_count =
               purrr::map_int(status_id, ~ .get_reply_count(data = reply, status_id = .x)))
    
    ratio_log <-
      ratio %>%
      .add_ratio_cols_at() %>%
      .add_timestamp_scrape_col_at() %>%
      .arrange_ratio_df_at() %>%
      .filter_ratio_df_at()
    
    export_ratio_log(ratio_log)
    
    ratio_last_export <-
      bind_rows(# ratio_last %>% mutate_at(vars(ratio_inv), funs(as.numeric)),
        ratio_last,
        ratio_log) %>%
      .filter_ratio_df_at() %>%
      .slice_ratio_df_at()
    
    export_ratio_last(ratio_last)
    invisible(ratio_log)
  }
