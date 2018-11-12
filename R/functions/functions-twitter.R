
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
    
    # .status_id <- status_id
    data %>%
      # filter(reply_to_status_id %in% .status_id) %>% 
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
        ratio_inv = 1 / ratio
      ) %>% 
      mutate_at(vars(ratio_inv), funs(na_if(., Inf)))
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

.get_timeline_verbosely <-
  function(user, since_id, ..., verbose = config$verbose) {
    if(verbose) {
      msg <- sprintf("Getting timeline for %s since last evaluated tweet: %s", screen_name, status_id)
      message(msg)
    }
    
    rtweet::get_timeline(user = user, since_id = since_id, ...) %>% 
      .filter_tl_df_at() %>% 
      .select_tl_cols_at()
  }

.get_timeline_verbosely_possibly <-
  purrr::possibly(
    .f = .get_timeline_verbosely,
    # .f = rtweet::get_timeline,
    otherwise = ..get_tl_df_default(),
    quiet = FALSE
  )

.N_HOUR_LAG <- 24L
# NOTE: `tl` is made to be an argument so that this funciton
# can be used to "initialize" new `screen_name`(s) that have not been evaluated before.
do_get_ratio <-
  function(tl = NULL, ..., verbose = config$verbose) {
    
    ratio_last_import <- import_ratio_last()
    if(is.null(tl)) {
      if(verbose) {
        msg <- "Retrieving latest tweets based on imported most recent ratios/statuses."
        message(msg)
      }
      tl_raw <-
        ratio_last_import %>%
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

    }
    
    stopifnot(is.data.frame(tl))
    stopifnot(all(c("screen_name", "status_id") %in% names(tl)))
    
    # NOTE: Do this to make sure that `tl` is trim (whether it is provided by the user
    # or generated for the condition `tl = NULL` (even if th `.get_timeline_verbosely_possibly()`
    # function already calls this function.)).
    tl <-
      tl %>%
      .select_tl_cols_at()
    
    if(verbose) {
      screen_name_before <- ratio_last_import %>% .pull_distinctly(screen_name)
      screen_name_after <- tl %>% .pull_distinctly(screen_name)
      screen_name_diff1 <- setdiff(screen_name_before, screen_name_after)
      screen_name_diff2 <- setdiff(screen_name_after, screen_name_before)
      if(length(screen_name_diff1) > 0L) {
        screen_name_diff_coll <- paste(screen_name_diff1, sep = "", collapse = ", ")
        msg <- sprintf("No new tweets to score for some names: %s.", screen_name_diff_coll)
        message(msg)
      } else if(length(screen_name_diff2) > 0L) {
        screen_name_diff_coll <- paste(screen_name_diff2, sep = "", collapse = ", ")
        msg <- sprintf("New screen names to evluate: %s.", screen_name_diff_coll)
        message(msg)
      } else {
        msg <- "New tweets to score for all screen names (and no new screen names)."
        message(msg)
      }
    }
    
    if(verbose) {
      tl_cnt <-
        tl %>%
        count(screen_name)
      tl_cnt %>% 
        mutate(msg = sprintf("Scoring %s tweet(s) for %s.", n, screen_name)) %>% 
        pull(msg) %>% 
        purrr::walk(message)
        
    }
    
    reply_raw <-
      tl %>%
      group_by(screen_name) %>% 
      arrange(created_at, .by_group = TRUE) %>% 
      slice(1) %>% 
      ungroup() %>% 
      mutate(
        data =
          purrr::pmap(
            list(screen_name, status_id), 
            ~.get_replies_since(screen_name = ..1, since_id = ..2)
          )
      )
    reply <-
      reply_raw %>% 
      select(data) %>%
      unnest(data)

    ratio <-
      tl %>% 
      mutate(
        reply_count = 
          purrr::map_int(status_id, ~.get_reply_count(data = reply, status_id = .x)
          )
      )
    
    ratio_log <-
      ratio %>% 
      .add_ratio_cols_at() %>%
      .add_timestamp_scrape_col_at() %>% 
      .arrange_ratio_df_at()
    
    ratio_log %>% export_ratio_log()
    
    # browser()
    ratio_last_export <-
      bind_rows(
        ratio_last_import,
        ratio_log
      ) %>% 
      filter(created_at <= (.TIME - lubridate::hours(.N_HOUR_LAG))) %>% 
      .slice_ratio_df_at()
    
    ratio_last_export %>% export_ratio_last()
    ratio_log
  }

