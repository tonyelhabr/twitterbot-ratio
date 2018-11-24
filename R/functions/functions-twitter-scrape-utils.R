
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
    # Note: Can't return a function, so return a `sentinel` instead.
    otherwise = as.integer(config$scrape_reply_sentinel)
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
        timestamp_post = NA_character_
      )
  }

.rename_tl <-
  function(data, ...) {
    data %>% rename(user = screen_name)
  }

.get_tweets_first <-
  function(.user,
           type = c("tl", "favs"),
           ...,
           .n = config$n_tl_new,
           token = .TOKEN,
           # token = rtweet::get_token(),
           rename = TRUE,
           filter = TRUE,
           verbose = config$verbose_scrape) {
    type <- match.arg(type)
    if(type == "tl") {
      f_get <- rtweet::get_timeline
    } else if (type == "favs") {
      f_get <- rtweet::get_favorites
    }
    if (verbose) {
      if(type == "tl") {
        txt <- "tweets from timeline"
      } else if (type == "favs") {
        txt <- "favorites"
      }
      msg <-
        sprintf(
          paste0(
            "\nGetting last %d %s for \"%s\"."
          ),
          .n,
          txt,
          .user
        )
      message(msg)
    }

    suppressMessages(
      res <-
        f_get(
          user = .user,
          n = .n,
          token = token,
          ...
        )
    )
    if(rename) {

      res <-
        res %>%
        .rename_tl()
    }
    if(filter) {

      res <-
        res %>%
        .filter_tweet_type()
    }
    res
  }
.get_tl_first <-
  function(type = "tl", ...) {
    .get_tweets_first(type = type, ...)
  }
.get_favs_first <-
  function(type = "favs", ...) {
    .get_tweets_first(type = type, ...)
  }

.get_tl_first_possibly <-
  purrr::possibly(.get_tl_first, otherwise = NULL)

# purrr::partial(.get_tl_first, .user = "punditratio", .n = 1800L, filter = FALSE)
# function(.user = "punditratio", .n = 1800L, filter = FALSE, ...) {
#   .get_tl_first(
#     .user = .user,
#     .n = .n,
#     filter = filter,
#     ...
#   ) %>%
#     .select_tl_cols_at()
# }

.get_tweets_self <-
  function(.user = "punditratio", rename = TRUE, filter = FALSE, ...) {
    tl <-
      .get_tl_first(
        .user = .user,
        .n = 3200L,
        rename = rename,
        filter = filter,
        ...
      )
    favs <-
      .get_favs_first(
        .user = .user,
        .n = 3000L,
        rename = rename,
        filter = filter,
        ...
      )
    res <-
      bind_rows(tl, favs)
  }

.get_tweets_self_possibly <- purrr::possibly(.get_tweets_self, otherwise = NULL)

.get_tl_since <-
  function(.user,
           .id,
           ...,
           .n = config$n_tl_since,
           token = .TOKEN,
           # token = rtweet::get_token(),
           rename = TRUE,
           filter = TRUE,
           verbose = config$verbose_scrape) {
    if (verbose) {
      msg <- sprintf(
        paste0("\nGetting timeline for \"%s\" since last evaluated tweet: %s"),
        .user,
        .id
      )
      message(msg)
    }

    suppressMessages(
      res <-
        rtweet::get_timeline(
          user = .user,
          since_id = .id,
          token = token,
          # n = .n,
          ...
        )
    )
    if(rename) {

      res <-
        res %>%
        .rename_tl()
    }
    if(filter) {

      res <-
        res %>%
        .filter_tweet_type()
    }
    res
  }

.get_tl_since_possibly <-
  purrr::possibly(.get_tl_since, otherwise = NULL)

.get_tl_until <-
  function(.user,
           .id,
           ...,
           .n = config$n_tl_until,
           token = .TOKEN,
           # token = rtweet::get_token(),
           verbose = config$verbose_scrape) {
    if (verbose) {
      msg <- sprintf(
        paste0("\nGetting timeline for \"%s\" before first evaluated tweet: %s"),
        .user,
        .id
      )
      message(msg)
    }
    suppressMessages(
      rtweet::get_timeline(
        user = .user,
        max_id = .id,
        n = .n,
        token = token,
        ...
      ) %>%
        .rename_tl() %>%
        .filter_tweet_type()
    )
  }

.get_tl_until_possibly <-
  purrr::possibly(.get_tl_until, otherwise = NULL)

.filter_tl_bytime <-
  function(data, n_hour_lag_scrape = config$n_hour_lag_scrape, ...) {
    data %>%
      filter(created_at <= (.TIME - lubridate::hours(n_hour_lag_scrape)))
  }
