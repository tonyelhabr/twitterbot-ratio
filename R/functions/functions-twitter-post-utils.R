


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

# .check_ratio_min_frac <-
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


.mark_ratio_min_frac_at <-
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
    browser()
    data %>% select(considered, everything())
    data_marked <-
      data %>%
      # filter(!!col_sym >= value)
      mutate(considered = if_else(!!col_sym < value, 1L, 0L))

    data_marked %>% filter(considered == 1L)
    # data_filt <- .check_ratio_min_frac(data = data_filt, .n_row = value)
    # if(nrow(data_filt) == 0L) {
    #   return(data_alt)
    # }
    # data_filt
    data_filt <-
      data_marked %>%
      filter(!!col_sym >= value)

    n_row <- nrow(data_filt)

    if (n_row < value) {
      if (verbose) {
        msg <-
          sprintf(
            paste0(
              "Not posting since there are not enough tweets with `%s` > %.03f."
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
           ratio_num_min_frac = config$post_ratio_num_min_frac,
           verbose = config$verbose_post) {
    # data_filt <- data
    data_filt <- .filter_ratio_log_basic(data)

    n_row <- nrow(data_filt)
    if (n_row < n_since_min) {
      if (verbose) {
        msg <-
          sprintf(
            paste0(
              "Not posting since this user hasn't made %d tweets since last post.",
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
      user_info <- .filter_byuser(user_info, .user = user)
      if (is.null(user_info)) {
        msg <- "No info about user in `user_info` file!"
        stop(msg, call. = FALSE)
      }
    }

    data_filt <-
      data_filt %>%
      inner_join(user_info, by = c("user"))

    data_filt <-
      data_filt %>%
      mutate(
        ratio_den = (favorite_count + retweet_count),
        ratio_num = reply_count
      ) %>%
      mutate(
        ratio_den_frac = ratio_den / followers_count,
        ratio_num_frac = ratio_num / followers_count
      )


    data_filt <-
      .mark_ratio_min_frac_at(
        data = data_filt,
        col = "ratio_den_frac",
        value = ratio_den_min_frac
      )
    # if(nrow(data_filt) == 0L) {
    #   return(data_alt)
    # }

    data_filt <- .filter_ratio_log_basic(data_filt)

    data_filt <-
      .mark_ratio_min_frac_at(
        data = data_filt,
        col = "ratio_num_frac",
        value = ratio_num_min_frac
      )

    data_filt <- .filter_ratio_log_basic(data_filt)
    data_filt
  }
