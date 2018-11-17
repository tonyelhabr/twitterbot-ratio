
# TODO: Programmatically figure out .N_SCREEN_NAME?
# .N_SCREEN_NAME <- 3L
# .N_SCREEN_NAME <- ifelse(file.exists(config$path_last), import_ratio_last() %>% nrow(), 3L)
.N_SCREEN_NAME <- 4L
.MAX_PER_SESSION <- 18000L
.N_SCALE <- 1L
# .N_SCALE <- 1L / (.N_SCREEN_NAME)
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
           token = config$token,
           verbose = config$verbose_scrape) {

    rtweet::search_tweets(
      q = q,
      since_id = since_id,
      ...,
      n = n,
      include_rts = include_rts,
      retryonratelimit = retryonratelimit,
      verbose = TRUE,
      token = token
    )
  }

.get_reply_count <-
  function(data, status_id, ...) {

    data %>%
      filter(reply_to_status_id == !!status_id) %>%
      count() %>%
      pull(n)
  }


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

count_replies <-
  function(tl_filt, ...) {
    tl1 <-
      tl_filt %>%
      arrange(created_at) %>%
      slice(1)

    reply_raw <-
      tl1 %>%
      mutate(
        data =
          purrr::pmap(
            list(screen_name, since_id),
            ~ .get_replies_since(screen_name = ..1, since_id = ..2)
          )
      )

    reply_data <-
      reply_raw %>%
      select(data) %>%
      unnest(data)

    reply <-
      tl_filt %>%
      mutate(
        reply_count =
          purrr::map_int(status_id, ~ .get_reply_count(data = reply_data, status_id = .x))
      )
  }