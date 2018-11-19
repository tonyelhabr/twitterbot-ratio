
# Reference: rtweet::emojis %>% filter(description %>% str_detect("^fire$"))
.get_chr_emoji_fire <-
  function() {
    # "\U0001f525"
    ""
  }

# Reference: "^fearful face$"
.get_chr_emoji_fear <-
  function() {
    # "\U0001f628"
    ""
  }

# Reference: "^grinning face$"
.get_chr_emoji_smile <-
  function() {
    # "\U0001f600"
    ""
  }

# Reference: "^thumbs up$"
.get_chr_emoji_thumbsup <-
  function() {
    # "\U0001f44d"
    ""
  }

.prettify_timestamp_post <-
  function(x) {
    # stopifnot(lubridate::is.timepoint(x))
    strftime(x, "%I:%M %p, %m/%d/%Y")
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

.filter_ratio_log_scrape_posted <-
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
           ratio_log_scrape,
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
      ratio_log_scrape %>%
      .filter_ratio_log_scrape_posted(
        .screen_name = screen_name,
        .status_id = status_id
      )

    if(nrow(ratio_pastposted) == 0L) {
      chr_emoji <- .get_chr_emoji_fear()
      text_post <-
        sprintf(
          paste0(
            "%s This is the first time I've scored you. ",
            "Be aware of your ratio with future tweets! %s"
          ),
          text_post,
          chr_emoji
        )
    } else {

      if(!is.null(ratio_last_post)) {
        # Note: This data set should only have one record per screen name,
        # so there should not be any need to `slice()`.
        ratio_pastposted_last <-
          ratio_last_post %>%
          .filter_ratio_log_scrape_posted(
            .screen_name = screen_name,
            .status_id = status_id
          )
      } else {
        # Note: Doing this in case `ratio_last_post` is not provided.
        ratio_pastposted_last <-
          ratio_pastposted %>%
          arrange(desc(timestamp_post)) %>%
          slice(1)

      }

      if(nrow(ratio_pastposted_last) > 1L) {
        msg <-
          paste0(
            "It was not expected that this condition would ever be met.\n",
            "(Check that `ratio_log_scrape_post` is getting properly updated.)"
          )
        stop(msg, call. = FALSE)
      }

      ratio_past1 <- pull(ratio_pastposted_last, ratio)
      timestamp_post_past1 <- pull(ratio_pastposted_last, timestamp_post)
      timestamp_post_past1 <- .prettify_timestamp_post(timestamp_post_past1)

      if(ratio_past1 >= ratio) {
        chr_emoji <- .get_chr_emoji_thumbsup()
        text_post <-
          sprintf(
            "%s This ratio is lower the previous one (%.02f) I tweeted about (at %s)! %s",
            text_post,
            ratio_past1,
            timestamp_post_past1,
            chr_emoji
          )
      } else {

        ratio_pastposted_max <- .slice_ratio_max(ratio_pastposted)
        ratio_max <- pull(ratio_pastposted_max, ratio)

        chr_emoji <- .get_chr_emoji_fire()

        if(ratio_max > ratio) {

          text_post <-
            sprintf(
              "%s This ratio tops the previous one (%.02f) I tweeted about (at %s)! %s",
              text_post,
              ratio_past1,
              timestamp_post_past1,
              chr_emoji
            )
        } else {

          timestamp_post_max <- pull(ratio_pastposted_max, timestamp_post)
          timestamp_post_max <- .prettify_timestamp_post(timestamp_post_max)
          text_post <-
            sprintf(
              "%s This ratio breaks your previous all-time high (%.02f at %s)! %s",
              text_post,
              ratio_max,
              timestamp_post_max,
              chr_emoji
            )
        }
      }
    }

    if(reply) {
      text_post <-
        sprintf(
          "%s %s",
          text_post,
          .make_twitter_url_reply(
            .screen_name = screen_name,
            .status_id = status_id
          )
        )
    }
    text_post
  }

.ratio_post <-
  function(screen_name,
           status_id,
           text_post,
           ...,
           reply = config$post_reply,
           retweet = config$post_retweet,
           favorite = config$post_favorite,
           sentinel = config$post_status_id_sentinel,
           token = config$token,
           verbose = config$verbose_post) {
    # # Debugging...
    # reply = FALSE
    # retweet = FALSE
    # favorite = FALSE

    # Note: This is somewhat similar to `.preprocess_compare_n_rows()`.
    if(sum(c(favorite, retweet, reply), na.rm = TRUE) < 1) {
      if(verbose) {
        msg <-
          sprintf(
            paste0(
              "`reply`, `retweet`, and `favorite` are each `FALSE`. ",
              "Returning `sentinel` (for `status_id_post`) for \"%s\"."
            ),
            screen_name
          )
        message(msg)
      }
      return(invisible(sentinel))
    }
    if (favorite) {
      resp <- rtweet::post_favorite(status_id, token = token)
    }
    if (retweet) {
      resp <-
        rtweet::post_tweet(
          status = text_post,
          retweet_id = status_id,
          token = token
        )
    }
    if (reply) {
      # Note: This doesn't work for including source tweet with reply.
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
    # if(!is.null(resp)) {
    #   httr::warn_for_status(resp)
    # }
    # TODO: Get the status_id of the tweet (with a function run after `.do_post_ratio()`?
    res <- ""
    invisible(res)
  }

# Note: Default for `screen_name` is `NULL` so that code can dynamically
# determine whether to post tweets for all screen names or just one.
.do_post_ratio <-
  function(screen_name = NULL,
           ratio_log_scrape = NULL,
           ratio_last_post = NULL,
           ...,
           delay = config$post_delay,
           sentinel = config$post_status_id_sentinel,
           verbose = config$verbose_post) {

    # screen_name = "PFTCommenter"
    # ratio_log_scrape = NULL
    # ratio_last_post = NULL
    # verbose = config$verbose_post

    # TODO (Long-term): Write a function to do the same pre-processing for the
    # `do_scrape/ratio_post()` functions.
    message(rep("-", 80L))
    .validate_screen_name_1(screen_name)

    if (is.null(ratio_log_scrape)) {
      ratio_log_scrape <- .import_ratio_log_scrape_possibly()
      if (is.null(ratio_log_scrape)) {
        msg <- sprintf("Could not import `ratio_log_scrape`.")
        stop(msg, call. = FALSE)
      }
    }

    .validate_ratio_df_robustly(ratio_log_scrape)

    if (is.null(ratio_last_post)) {
      ratio_last_post <- .import_ratio_last_post_possibly()
      if (is.null(ratio_last_post)) {
        if (verbose) {
          msg <-
            sprintf("Creating missing `ratio_last_post` file from `ratio_log_scrape`.")
          message(msg)
        }
        ratio_last_post <- .convert_ratio_log_scrape_to_last_post(ratio_log_scrape)
        export_ratio_last_post(ratio_last_post)
      }
    }
    .compare_n_row_le(
      data1 = ratio_last_post,
      data2 = ratio_log_scrape
    )
    .validate_ratio_df_robustly(ratio_last_post)
    .validate_ratio_onerowpergrp_df(ratio_last_post)

    ratio_log_scrape_filt <- ratio_log_scrape

    # TODO: What about the alternative?
    # Note: This verbose function is only useful to avoid conflicts/renaming of `screen_name`
    # in the `.do_post_ratio()` function.
    .filter_ratio_log_scrape_byscreen_name <-
      function(data, .screen_name, ...) {
        data %>%
          filter(screen_name == .screen_name)
      }

    .filter_ratio_log_scrape_byconfig <-
      function(data, ...) {
        data_filt0 <-
          data %>%
          filter(considered == 0L) %>%
          filter(posted == 0L)

        data_alt <- head(data, 0)

        data_filt1 <-
          data_filt0 %>%
          arrange(desc(created_at))

        if(n_row_filt < config$post_hour_since_min) {

        }

        n_row_filt <- nrow(data_filt0)
        if(n_row_filt < config$post_n_since_min) {
          return(data_alt )
        }

      }

    if (!is.null(screen_name)) {
      ratio_log_scrape_filt <-
        ratio_log_scrape_filt %>%
        .filter_ratio_log_scrape_byscreen_name(
          screen_name = screen_name
          )
    }

    ratio_log_scrape_filt <-
      ratio_log_scrape_filt %>%
      filter(considered == 0L) %>%
      filter(posted == 0L)

    if(nrow(ratio_log_scrape_filt) == 0L) {
      if(verbose) {
        msg <- sprintf("No tweet to post about \"%s\".", screen_name)
        message(msg)
        return(NULL)
      }
    }

    ratio_topost_raw <- .slice_ratio_max(ratio_log_scrape_filt)

    suppressMessages(
      ratio_notposted_raw <-
        ratio_log_scrape_filt %>%
        anti_join(ratio_topost_raw)
    )

    .compare_n_row_eq(
      data1 = ratio_log_scrape_filt,
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
              ratio_log_scrape = ratio_log_scrape,
              ratio_last_post = ratio_last_post
            )
          )
      )

    if(delay) {
      text_post <- pull(ratio_topost, text_post)
      msg <-
        sprintf(
          paste0(
            "If `delay` were set to `TRUE`, the message that would have been posted is:\n",
            "%s"
          ),
          text_post
        )
      message(msg)
      # Note: Instead of returning, try out the rest of the code.
      # return(ratio_topost)
      ratio_wasposted_raw <- mutate(ratio_topost, status_id_post = sentinel)

    } else {

      ratio_wasposted_raw <-
        ratio_topost %>%
        mutate(
          status_id_post =
            purrr::pmap_chr(
              list(screen_name, status_id, text_post),
              ~.ratio_post(
                screen_name = ..1,
                status_id = ..2,
                text_post = ..3
              )
            )
        )
    }

    suppressMessages(
      ratio_log_scrape_export <-
        ratio_log_scrape %>%
        mutate(rn = row_number()) %>%
        left_join(
          bind_rows(
            ratio_wasposted_raw %>% mutate(posted = 1L),
            ratio_notposted_raw %>% mutate(posted = 0L)
            ) %>%
            mutate(considered = 1L, timestamp_post = Sys.time()) %>%
            .select_ratio_cols_at(),
          by = .COLS_RATIO_BASE_ORDER,
          suffix = c("", "_y")
        ) %>%
        mutate(
          considered = coalesce(considered_y, considered),
          posted = coalesce(posted_y, posted),
          status_id_post = coalesce(status_id_post_y, status_id_post),
          text_post = coalesce(text_post_y, text_post),
          timestamp_post = coalesce(timestamp_post_y, timestamp_post)
        )%>%
        arrange(rn) %>%
        select(-rn) %>%
        .select_ratio_cols_at()
    )

    .compare_n_row_eq(
      data1 = ratio_log_scrape_export,
      data2 = ratio_log_scrape
    )

    status_id_posted <- pull(ratio_wasposted_raw, status_id_post)
    was_posted <- ifelse(status_id_posted != sentinel, TRUE, FALSE)

    if(!was_posted) {
      if(verbose) {
        msg <-
          sprintf(
            paste0(
              "Potential updates to `ratio_log_scrape` and `ratio_last_scrape` are ",
              "being reverted (because `sentinel` was detected) for \"%s\"."
          ),
          screen_name
          )
        message(msg)
      }
      ratio_log_scrape_export <- ratio_log_scrape
    }

    path_ratio_log_scrape <- export_ratio_log_scrape_post(ratio_log_scrape_export)

    ratio_last_post_export <- .convert_ratio_log_scrape_to_last_post(ratio_log_scrape_export)
    path_ratio_last_post <- export_ratio_last_post(ratio_last_post_export)

    invisible(path_ratio_last_post)
  }


do_post_ratio_all <-
  function(screen_name = NULL, ...) {
    # Note: The interactive statement can be removed. It's purely for debugging purposes.
    if(interactive() | is.null(screen_name)) {
      screen_name <- get_screen_name_topost()
    }
    .validate_screen_name_vector(screen_name)
    # s.create_backup(path = config$path_ratio_log_post)
    purrr::walk(screen_name, ~.do_post_ratio(screen_name = .x))
  }

