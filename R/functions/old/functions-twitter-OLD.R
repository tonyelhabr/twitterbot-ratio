

.arrange_ratio_df_at <-
  function(data, ...) {
    data %>%
      group_by(screen_name) %>% 
      arrange(desc(ratio), .by_group = TRUE) %>% 
      ungroup()
  }

.get_tl_possibly <-
  purrr::possibly(
    .f = .get_tl,
    # .f = rtweet::get_timeline,
    otherwise = ..get_tl_df_default(),
    quiet = FALSE
  )

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
      .filter_tl_df_at()
    
    export_ratio_log(ratio_log)
    
    ratio_last_export <-
      bind_rows(
        ratio_last,
        ratio_log) %>%
      .filter_tl_df_at() %>%
      .slice_ratio_df_at()
    
    export_ratio_last(ratio_last)
    invisible(ratio_log)
  }
