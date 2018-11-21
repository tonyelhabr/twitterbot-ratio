

..get_tl_df_default <-
  function(...) {
    tibble(
      user_id = character(),
      user = character(),
      created_at = lubridate::as_datetime(character()),
      status_id = character(),
      favorite_count = integer(),
      retweet_count = integer(),
      text = character()
    ) %>%
      .reorder_tl_cols_at()
  }

..ratio_scrape_df_default <-
  function(...) {
    bind_cols(
      ..get_tl_df_default(),
      tibble(
        reply_count = double(),
        ratio = double(),
        timestamp_scrape = lubridate::as_datetime(character())
      )
    ) %>%
      .reorder_ratio_cols_at()
  }


.validate_ratio_last_df <-
  function(ratio_last, user, ...) {
    n_status_id <-
      ratio_last %>%
      count(status_id) %>%
      filter(n > 1L)
    if(ifelse(nrow(n_status_id) > 0L, TRUE, FALSE)) {
      msg <- sprintf("Expected 1 `status_id`. Instead, found %s statuses for `%s`.", n_status_id, !!user)
      stop(msg, call. = FALSE)
    }
  }

.unconvert_id_cols_at <-
  function(data, cols = str_subset(names(data), "user_id|status_id"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character))
  }

.unconvert_datetime_cols <-
  function(data, cols = str_subset(names(data), "^created_at$|^timestamp"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character))
  }

# TODO: Programmatically figure out .N_USER?
# .N_USER <- 3L
# .N_USER <- ifelse(file.exists(config$path_ratio_last_scrape), import_ratio_last_scrape() %>% nrow(), 3L)
.N_USER <- 4L
.MAX_PER_SESSION <- 18000L
.N_SCALE <- 1L
# .N_SCALE <- 1L / (.N_USER)
.N_SEARCH <- .N_SCALE * .MAX_PER_SESSION %>% floor() %>% as.integer()
# .N_SEARCH <- .MAX_PER_SESSION

.get_replies_since <-
  function(user,
           since_id,
           ...,
           q = sprintf("@%s OR to:%s OR %s", user, user, user),
           # n = 10000L,
           n = .N_SEARCH,
           include_rts = FALSE,
           retryonratelimit = TRUE,
           token = .TOKEN,
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
      group_by(user) %>%
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

.describe_user <-
  function(tl, ratio, ...) {

    nm_ratio <- ratio %>% .pull_distinctly(user)
    nm_tl <- tl %>% .pull_distinctly(user)
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
      count(user)
    tl_cnt %>%
      mutate(msg = sprintf("Scoring %s tweet(s) for %s.", n, user)) %>%
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
            list(user, since_id),
            ~ .get_replies_since(user = ..1, since_id = ..2)
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

do_scrape_ratio_all <-
  function(user = NULL,
           ...,
           path = config$path_ratio_log_scrape,
           backup = TRUE,
           clean = TRUE,
           progress = TRUE) {
    if (is.null(user)) {
      user <- get_user_toscrape()
      user <- user[6:10]
    }
    .validate_user_vector(user)
    if (backup) {
      .create_backup(path = path, clean = clean)
    }
    if (progress) {
      pb <- .create_pb(total = length(user))
    } else {
      pb <- NULL
    }
    .f <- function(.user, .pb = NULL) {
      .do_scrape_ratio(user = .user)
      if (!is.null(.pb)) {
        .pb$tick()
      }
    }
    purrr::walk(user, ~ .f(.user = .x, .pb = pb))

  }
do_post_ratio_all <-
  function(user = NULL, ..., backup = TRUE, progress = config$progress_post) {
    if(is.null(user)) {
      user <- get_user_topost()
      # user <- user[1:5]
    }
    .validate_user_vector(user)
    if(backup) {
      .create_backup(path = config$path_ratio_log_scrape)
    }
    .f <- function(.user, .pb) {
      .do_post_ratio(user = .user)
      .pb$tick()
    }
    pb <- progress::progress_bar$new(total = length(user))
    purrr::walk(user, ~.f(.user = .x, .pb = pb))
  }

