
.make_twitter_selector_reply <-
  function(.status_id, ...) {
    sprintf("#profile-tweet-action-reply-count-aria-%s", .status_id)
  }

.get_reply_count_hack <-
  function(user, status_id, ..., sleep = TRUE, time = 0.1) {

    stopifnot(length(user) == 1L, length(status_id) == 1L)
    url <- .make_twitter_url_reply(.user = user, .status_id = status_id)
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

# Note: It might be the case that the tweet was deleted(?). I think this is
# why there has been an error here occassionally.
.get_reply_count_hack_possibly <-
  purrr::possibly(
    .get_reply_count_hack,
    otherwise =
      function(x) {
        message("Could not retrieve the reply count. Returning a dummy default value.")
        return(-1L)
      }
  )

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
      arrange(user, created_at)
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

.rename_tl <-
  function(data, ...) {
    data %>% rename(user = screen_name)
  }

.get_tl_first <-
  function(user, ..., n = config$n_tl_new, token = rtweet::get_token(), verbose = config$verbose_scrape) {
    if(verbose) {
      msg <- sprintf("\nGetting last %d tweets from timeline for \"%s\".", n, user)
      message(msg)
    }

    suppressMessages(
      rtweet::get_timeline(user = user, n = n, token = token, ...) %>%
        .rename_tl() %>%
        .filter_tweet_type()
    )
  }

.get_tl_first_possibly <-
  purrr::possibly(.get_tl_first, otherwise = NULL)

.get_tl_self <-
  purrr::partial(.get_tl_first, user = "punditratio")

.get_tl_self_possibly <- purrr::possibly(.get_tl_self, otherwise = NULL)

.get_tl_since <-
  function(user, since_id, ..., token = rtweet::get_token(), verbose = config$verbose_scrape) {
    if(verbose) {
      msg <- sprintf("\nGetting timeline for \"%s\" since last evaluated tweet: %s", user, since_id)
      message(msg)
    }

    suppressMessages(
      rtweet::get_timeline(user = user, since_id = since_id, token = token, ...) %>%
        .rename_tl() %>%
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
