
# Reference: rtweet::emojis %>% filter(description %>% str_detect("fire"))
.get_chr_emoji_fire <-
  function() {
    "\U0001f525"
  }

.slice_ratio_max <-
  function(data, ...) {
    res <-
      data %>%
      filter(ratio == max(ratio, na.rm = TRUE)) %>%
      slice(1)

    .validate_onerow_df(res)
    res
  }

.filter_ratio_log_posted <-
  function(data, .screen_name, .status_id, ...) {
    data %>%
      filter(screen_name == .screen_name) %>%
      filter(status_id != .status_id) %>%
      filter(posted == 1L)
  }

.create_text_post <-
  function(screen_name,
           status_id,
           ratio,
           ratio_log,
           ratio_last_post,
           ...,
           reply = config$post_reply) {

    text_post <-
      sprintf(
        "Congratulations @%s on your ratio of %.02f!",
        screen_name,
        ratio
      )

    ratio_pastposted <-
      ratio_log %>%
      .filter_ratio_log_posted(.screen_name = screen_name, .status_id = status_id)
    if(nrow(ratio_pastposted) == 0L) {
      text_post <-
        sprintf(
          paste0(
            "%s This is the first time I've scored you. ",
            "Be aware of your ratio with future tweets!"
          ),
          text_post
        )
    } else {

    if(!is.null(ratio_last_post)) {
      # NOTE: This data set should only have one record per screen name,
      # so there should not be any need to `slice()`.
      ratio_pastposted_last <-
        ratio_last_post %>%
        .filter_ratio_log_posted(.screen_name = screen_name, .status_id = status_id)
    } else {
      ratio_pastposted_last <-
        ratio_pastposted %>%
        arrange(desc(timestamp_post)) %>%
        slice(1)
      if(nrow(ratio_pastposted_last) > 0L) {
        msg <-
          paste0(
            "It was not expected that this condition would ever be met.\n",
            "(Check that `ratio_log_post` is getting properly updated.)"
          )
        stop(msg, call. = FALSE)
      }
    }

    if(nrow(ratio_pastposted_last) == 0L) {
      msg <-
        paste0(
          "It was not expected that this condition would ever be met!"
        )
      stop(msg, call. = FALSE)
    }
    ratio_past1 <- pull(ratio_pastposted_last, ratio)

    if(ratio_past1 < ratio) {
      chr_emoji_fire <- .get_chr_emoji_fire()
      text_post <-
        sprintf(
          "%s This ratio tops the previous one (%.02f) I tweeted about! %s",
          text_post,
          ratio_past1,
          chr_emoji_fire
        )
    }

    ratio_pastposted_max <-
      ratio_pastposted %>%
      .slice_ratio_max()

    ratio_max <- pull(ratio_pastposted_max, ratio)
    if(ratio_max < ratio) {
      text_post <-
        sprintf(
          "%s Also, this ratio breaks your previous all-time high (%.02f)! %s",
          text_post,
          ratio_max,
          chr_emoji_fire
        )
    }
    }

    if(reply) {
      text_post <-
        sprintf(
          "%s %s",
          text_post,
          .make_twitter_url_reply(.screen_name = screen_name, .status_id = status_id)
        )
    }
    text_post
  }

.post_ratio <-
  function(screen_name,
           status_id,
           text_post,
           ...,
           reply = config$post_reply,
           retweet = config$post_retweet,
           favorite = config$post_favorite,
           token = config$token,
           verbose = config$verbose_post) {
    if (favorite) {
      resp <- rtweet::post_favorite(status_id, token = token)
    }
    if (retweet && !reply) {
      resp <-
        rtweet::post_tweet(
          status = text_post,
          retweet_id = status_id,
          token = token
        )
    }
    if (reply) {
      # resp <-
      #   rtweet::post_tweet(
      #     status = text_post,
      #     in_reply_to_status_id = status_id,
      #     token = token
      #   )
      resp <-
        rtweet::post_tweet(
          status = text_post,
          token = token
        )
      if (verbose) {
        msg <- sprintf("Posted the following tweet at %s:\n\"%s.\"", Sys.time(), text_post)
        message(msg)
      }
    }
    # Debuggning...
    # status_id_post <- status_id
    if(!is.null(resp)) {
      httr::warn_for_status(resp)
    }
    # TODO: Get the status_id of the tweet?
    status_id_post <- ""
    invisible(status_id_post)
  }

# NOTE: Default for `screen_name` is `NULL` so that code can dynamically
# determine whether to post tweets for all screen names or just one.
do_post_ratio <-
  function(screen_name = NULL,
           ratio_log = NULL,
           ratio_last_post = NULL,
           ...,
           verbose = config$verbose_post) {

    # screen_name = "PFTCommenter"
    # ratio_log = NULL
    # ratio_last_post = NULL
    # verbose = config$verbose_post

    # if(verbose) {
    #   if(interactive()) {
    #     msg <- sprintf("Running interactively in `%s()`!", match.call()[[1]])
    #     message(msg)
    #   }
    # }

    # TODO (Long-term): Write a function to do the same pre-processing for the
    # `do_scrape/post_ratio()` functions.
    message(rep("-", 80L))
    stopifnot(length(screen_name) == 1L)

    if (is.null(ratio_log)) {
      ratio_log <- .import_ratio_log_possibly()
      if (is.null(ratio_log)) {
        msg <- sprintf("Could not import `ratio_log`.")
        stop(msg, call. = FALSE)
      }
    }

    .validate_ratio_df(ratio_log)

    if (is.null(ratio_last_post)) {
      ratio_last_post <- .import_ratio_last_post_possibly()
      if (is.null(ratio_last_post)) {
        if (verbose) {
          msg <-
            sprintf("Creating missing `ratio_last_post` file from `ratio_log`.")
          message(msg)
        }
        ratio_last_post <- .convert_ratio_log_to_last_post(ratio_log)
        export_ratio_last_post(ratio_last_post)
      }
    }
    .compare_n_row_le(
      data1 = ratio_last_post,
      data2 = ratio_log
    )
    .validate_ratio_df(ratio_last_post)
    .validate_ratio_onerowpergrp_df(ratio_last_post)

    ratio_log_filt <- ratio_log

    # TODO: What about the altnerative?
    if (!is.null(screen_name)) {
      .screen_name <- screen_name
      ratio_log_filt <-
        ratio_log_filt %>%
        filter(screen_name == .screen_name)
    }

    ratio_log_filt <-
      ratio_log_filt %>%
      filter(considered == 0L && posted == 0L)

    if(nrow(ratio_log_filt) == 0L) {
      if(verbose) {
        msg <- sprintf("No tweet to post about  \"%s\".", screen_name)
        message(msg)
        return(NULL)
      }
    }

    ratio_topost_raw <-
      ratio_log_filt %>%
      .slice_ratio_max()

    suppressMessages(
      ratio_notposted_raw <-
        ratio_log %>%
        anti_join(ratio_topost_raw)
    )

    .compare_n_row_eq(
      data1 = ratio_log,
      data2 = bind_rows(ratio_topost_raw, ratio_notposted_raw)
    )

    ratio_topost <-
      ratio_topost_raw %>%
      mutate(
        text_post =
          purrr::pmap_chr(
            list(screen_name, status_id, ratio),
            ~ .create_text_post(
              screen_name = ..1,
              status_id = ..2,
              ratio = ..3,
              ratio_log = ratio_log,
              ratio_last_post = ratio_last_post
            )
          )
      )

    # NOTE: Be careful with posting!
    if(TRUE) {
      ratio_wasposted_raw <-
        ratio_topost %>%
        mutate(
          status_id_post =
            purrr::pmap_chr(
              list(screen_name, status_id, text_post),
              ~.post_ratio(
                screen_name = ..1,
                status_id = ..2,
                text_post = ..3
              )
            )
        )
    }

    # NOTE: I believe that the columns of these data sets
    # should already be in the correct order.
    ratio_wasposted <-
      ratio_wasposted_raw %>%
      mutate(posted = 1L, timestamp_post = Sys.time()) %>%
      .select_ratio_cols_at()

    ratio_notposted <-
      ratio_notposted_raw %>%
      mutate(posted = 0L) %>%
      .select_ratio_cols_at()

    suppressMessages(
      ratio_log_export <-
        ratio_log %>%
        mutate(rn = row_number()) %>%
        select(one_of(c("rn", .COLS_RATIO_BASE_ORDER))) %>%
        inner_join(
          bind_rows(ratio_notposted, ratio_wasposted)
        ) %>%
        arrange(rn) %>%
        select(-rn)
    )

    .compare_n_row_eq(
      data1 = ratio_log_export,
      data2 = ratio_log
    )

    path_ratio_log <- export_ratio_log_post(ratio_log_export)

    ratio_last_post_export <- .convert_ratio_log_to_last_post(ratio_log_export)
    path_ratio_last_post <- export_ratio_last_post(ratio_last_post_export)

    invisible(ratio_wasposted)
  }

.do_post_ratio_possibly <-
  purrr::possibly(do_post_ratio, otherwise = NULL)

do_post_ratio_all <-
  function(screen_name = NULL, ...) {
    .pre_do_twitter(screen_name = screen_name)
    # purrr::walk(screen_name, ~.do_post_ratio_possibly(screen_name = .x, ...))
    purrr::walk(screen_name, ~.do_post_ratio_possibly(screen_name = .x))
  }

