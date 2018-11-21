
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

.get_timestamp_post_pretty <-
  function(data, ...) {
    # .validate_onerow_df(data)
    data %>%
      pull(timestamp_post) %>%
      .prettify_timestamp_post()
  }

.slice_ratio_f <-
  function(data, f, ..., na.rm = TRUE) {
    res <-
      data %>%
      filter(ratio == f(ratio, na.rm = na.rm)) %>%
      slice(1)
    # # Note: This is overkill.
    # .validate_onerow_df(res)
    res
  }

.slice_ratio_hi <-
  purrr::partial(.slice_ratio_f, f = max)

.slice_ratio_lo <-
  purrr::partial(.slice_ratio_f, f = min)


.slice_ratio_log_pastposted <-
  function(data, .user, .status_id, ...) {
    data %>%
      filter(user == .user) %>%
      filter(status_id != .status_id) %>%
      filter(posted == 1L)
  }

.get_default_data <-
  function(data, ...) {
    head(data, 0)
  }

# .check_ratio_lo_frac <-
#   function(data,
#            .n_row,
#            .nm = deparse(substitute(.n_row)),
#            ...,
#            data_alt = .get_default_data(data),
#            verbose = config$verbose_post) {
#     data_filt <-
#       data %>%
#       filter(!!col_sym >= value)
#     n_row <- nrow(data_filt)
#     if (n_row < .n_row) {
#       if (verbose) {
#         msg <-
#           sprintf(paste0(
#             "Not posting since there are not enough tweets with `%s` > %.03f."
#           ),
#           .nm,
#           .n_row)
#         message(msg)
#       }
#       return(data_alt)
#     }
#     data
#   }

# Note: Originally intended to use this for both `num` and `den`, but decided
# to just use for `den`.
.mark_ratio_frac_at <-
  function(data,
           col,
           value,
           nm_value = deparse(substitute(value)),
           ...,
           data_alt = .get_default_data(data),
           verbose = config$verbose_post) {
    stopifnot(is.character(col))
    stopifnot(any(col %in% names(data)))
    stopifnot(!any(value %in% names(data)))

    col_sym <- sym(col)
    data_marked <-
      data %>%
      mutate(considered = if_else(!!col_sym < value, 1L, 0L))

    data_filt <-
      data_marked %>%
      filter(!!col_sym >= value)

    n_row <- nrow(data_filt)

    if (n_row < value) {
      if (verbose) {
        msg <-
          sprintf(
            paste0(
              "Not posting since there are not enough tweets with `%s` > %.f."
            ),
            nm_value,
            value
          )
        message(msg)
      }
    }
    data_marked
  }

.filter_ratio_log_basic <-
  function(data, ...) {
    data %>%
      filter(considered == 0L) %>%
      filter(posted == 0L)
  }

.add_ratio_numden_cols <-
  function(data, user_info = NULL, ...) {
    if (is.null(user_info)) {
      user_info <- import_user_info()
    }

    data %>%
      inner_join(user_info, by = c("user")) %>%
      mutate(
        # ratio_num = reply_count,
        ratio_den = (favorite_count + retweet_count),
      ) %>%
      mutate(
        # ratio_num_frac = ratio_num / followers_count,
        ratio_den_frac = ratio_den / followers_count
      )
  }

.filter_ratio_log_byconfig <-
  function(data,
           .user,
           user_info = NULL,
           ...,
           data_alt = .get_default_data(data),
           hour_since_min = config$post_hour_since_min,
           n_since_min = config$post_n_since_min,
           # ratio_den_min = config$post_ratio_den_min,
           # ratio_num_min = config$post_ratio_num_min,
           ratio_den_min_frac = config$post_ratio_den_min_frac,
           # ratio_num_min_frac = config$post_ratio_num_min_frac,
           ratio_min = config$post_ratio_min,
           verbose = config$verbose_post) {
    # data_filt <- data
    data_filt <- .filter_ratio_log_basic(data)

    n_row <- nrow(data_filt)
    if (n_row < n_since_min) {
      if (verbose) {
        msg <-
          sprintf(
            paste0(
              "Not posting since this user hasn't made %d tweets since last post. ",
              "(User has made %d tweets.)"
            ),
            n_since_min,
            n_row
          )
        message(msg)
      }
      return(data_alt)
    }

    data_filt <-
      data_filt %>%
      arrange(desc(created_at)) %>%
      filter(created_at > hour_since_min)

    n_row <- nrow(data_filt)
    if (n_row < hour_since_min) {
      if (verbose) {
        msg <-
          sprintf(
            paste0(
              "Not posting since it hasn't been %s hour(s) since last post for this user."
            ),
            hour_since_min
          )
        message(msg)
      }
      return(data_alt)
    }

    if (is.null(user_info)) {
      user_info <- import_user_info()
      user_info <- .filter_byuser(user_info, .user = .user)
      if (is.null(user_info)) {
        msg <- "No info about user in `user_info` file!"
        stop(msg, call. = FALSE)
      }
    }
    data_filt <-
      data_filt %>%
      .add_ratio_numden_cols(user_info = user_info)

    data_filt <-
      data_filt %>%
      .mark_ratio_frac_at(
        col = "ratio_den_frac",
        value = ratio_den_min_frac
      )

    cols <- names(data)
    data_filt <-
      data_filt %>%
      .filter_ratio_log_basic() %>%
      select(one_of(cols))

    data_filt
  }

.create_text_post <-
  function(user,
           status_id,
           ratio,
           ratio_log_scrape,
           ratio_last_post,
           ...,
           ratio_min = config$post_ratio_min,
           reply = config$post_reply,
           verbose = config$verbose_post) {

    text_post <-
      sprintf(
        "Congratulations @%s on your ratio of %.03f!",
        user,
        ratio
      )

    ratio_pastposted <-
      ratio_log_scrape %>%
      .slice_ratio_log_pastposted(
        .user = user,
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
        # Note: Should probably use `ratio_pastposted` first instead of doing this.
        ratio_pastposted_past1 <-
          ratio_last_post %>%
          .slice_ratio_log_pastposted(
            .user = user,
            .status_id = status_id
          )
      } else {
        ratio_pastposted_past1 <-
          ratio_pastposted %>%
          arrange(desc(timestamp_post)) %>%
          slice(1)

      }
      .validate_onerow_df(ratio_pastposted_past1)

      ratio_pastposted_hi <- .slice_ratio_hi(ratio_pastposted)
      ratio_hi <- ratio_pastposted_hi %>% pull(ratio)
      ratio_pastposted_lo <- .slice_ratio_lo(ratio_pastposted)
      ratio_lo <- ratio_pastposted_hi %>% pull(ratio)
      ratio_past1 <- ratio_pastposted_past1 %>% pull(ratio)

      chr_emoji_lo <- .get_chr_emoji_thumbsup()
      chr_emoji_hi <- .get_chr_emoji_fire()

      if((ratio < ratio_lo) | (ratio > ratio_hi)) {
        if (ratio < ratio_lo) {
          term <- "LOW"
          ratio_pastposted_lohi <- ratio_pastposted_lo
          ratio_lohi <- ratio_lo
          chr_emoji <- chr_emoji_lo
        } else if (ratio > ratio_hi) {
          term <- "HIGH"
          ratio_pastposted_lohi <- ratio_pastposted_hi
          ratio_lohi <- ratio_hi
          chr_emoji <- chr_emoji_hi
        }
        timestamp_post <-
          ratio_pastposted_lohi %>%
          .get_timestamp_post_pretty()
        text_post <-
          sprintf(
            "%s This ratio breaks your previous all-time %s (%.02f at %s)! %s",
            text_post,
            term,
            ratio_lohi,
            timestamp_post,
            chr_emoji
          )
      } else {
        if (ratio < ratio_past1) {
          term <- "LOWER"
          chr_emoji <- chr_emoji_lo
        } else if (ratio >= ratio_past1) {
          term <- "HIGHER"
          chr_emoji <- chr_emoji_hi
        }
        timestamp_post_past1 <-
          ratio_pastposted_past1 %>%
          .get_timestamp_post_pretty()
        text_post <-
          sprintf(
            "%s This ratio is %s than the one (%.03f) I tweeted about last (at %s)! %s",
            text_post,
            term,
            ratio_past1,
            timestamp_post_past1,
            chr_emoji
          )
      }

      # Note: Overwrite anything from before if this is true.
      # TODO: Think about this!
      # (Could just move this up before the prior logic, but not sure if I want
      # to actually use this going forward.)
      if(ratio < ratio_min) {
        if(verbose) {
          sprintf(
            paste0(
              "Over-ruling in-development `text_post` with new one ",
              "(because `ratio` < `ratio_min`). (Before, it was \n\"%s\")"
            ),
            text_post
          )
        }
        if(!exists("timestamp_post_past1")) {
          timestamp_post_past1 <-
            ratio_pastposted_past1 %>%
            .get_timestamp_post_pretty()
        }
        text_post <-
          sprintf(
            paste0(
              "You've been doing a good job with keeping your ratio low since ",
              "the last time I tweeted about you (at %s). Keep it up! %s"
            ),
            timestamp_post_past1,
            .get_chr_emoji_smile()
          )
      }
    }

    if(reply) {
      text_post <-
        sprintf(
          "%s %s",
          text_post,
          .make_twitter_url_reply(
            .user = user,
            .status_id = status_id
          )
        )
    }
    if(nchar(text_post) > 280) {
      stop("`text_post` cannot exceed 280 characters.", call. = FALSE)
    }
    text_post
  }

.ratio_post <-
  function(user,
           status_id,
           text_post,
           ...,
           reply = config$post_reply,
           retweet = config$post_retweet,
           favorite = config$post_favorite,
           sentinel = config$post_status_id_sentinel,
           token = .TOKEN,
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
            user
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
    }
    if(resp$errors[[1]]$code == 261) {
      message(resp$errors[[1]]$message)
      return(invisible(sentinel))
    }
    if (verbose & reply) {
      msg <- sprintf("Posted the following tweet at %s:\n\"%s.\"", Sys.time(), text_post)
      message(msg)
    }
    # if(!is.null(resp)) {
    #   httr::warn_for_status(resp)
    # }
    # TODO: Get the status_id of the tweet (with a function run after `.do_post_ratio()`?
    res <- ""
    invisible(res)
  }


