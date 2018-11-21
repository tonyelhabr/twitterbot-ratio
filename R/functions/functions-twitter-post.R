
# Note: Default for `user` is `NULL` so that code can dynamically
# determine whether to post tweets for all screen names or just one.
.do_post_ratio <-
  function(user = NULL,
           ratio_log_scrape = NULL,
           ratio_last_post = NULL,
           ...,
           post = config$post,
           sentinel = config$post_status_id_sentinel,
           verbose = config$verbose_post) {

    # user = "PFTCommenter"
    # ratio_log_scrape = NULL
    # ratio_last_post = NULL
    # verbose = config$verbose_post

    # TODO (Long-term): Write a function to do the same pre-processing for the
    # `do_scrape/ratio_post()` functions.
    # message(rep("-", 80L))
    # message(user)
    .validate_user_scalar(user)

    if (is.null(ratio_log_scrape)) {
      # Note: Don't throw an informative error message here. It's highly unlikely it would ever be encountered.
      ratio_log_scrape <- import_ratio_log_scrape()
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
        ratio_last_post <-
          .convert_ratio_log_scrape_to_last_post(ratio_log_scrape)
        export_ratio_last_post(ratio_last_post)
      }
    }
    .compare_n_row_le(
      data1 = ratio_last_post,
      data2 = ratio_log_scrape
    )
    .validate_ratio_df_robustly(ratio_last_post)
    .validate_ratio_onerowpergrp_df(ratio_last_post)

    # browser()
    ratio_log_scrape_filt <-
      ratio_log_scrape %>%
      .filter_byuser(.user = user) %>%
      .filter_ratio_log_basic() %>%
      .filter_ratio_log_byconfig(.user = user)

    if(nrow(ratio_log_scrape_filt) == 0L) {
      if(verbose) {
        msg <- sprintf("No tweet to post about \"%s\".", user)
        message(msg)
        return(NULL)
      }
    }

    # Note: Should I filter again because the `frac` values have only been
    # marked as considered and not filtered out yet?
    ratio_topost_raw <-
      ratio_log_scrape_filt %>%
      .filter_ratio_log_basic() %>%
      .slice_ratio_hi()

    suppressMessages(
      ratio_notposted_raw <-
        ratio_log_scrape_filt %>%
        anti_join(ratio_topost_raw)
    )

    .compare_n_row_eq(
      data1 = ratio_log_scrape_filt,
      data2 = bind_rows(ratio_topost_raw, ratio_notposted_raw)
    )

    # browser()
    ratio_topost <-
      ratio_topost_raw %>%
      mutate(
        text_post =
          purrr::pmap_chr(
            list(user, status_id, ratio),
            ~ .create_text_post(
              user = ..1,
              status_id = ..2,
              ratio = ..3,
              ratio_log_scrape = ratio_log_scrape,
              ratio_last_post = ratio_last_post
            )
          )
      )

    if(post) {
      ratio_wasposted_raw <-
        ratio_topost %>%
        mutate(
          status_id_post =
            purrr::pmap_chr(
              list(user, status_id, text_post),
              ~.ratio_post(
                user = ..1,
                status_id = ..2,
                text_post = ..3
              )
            )
        )
    } else {
      ratio_wasposted_raw <- ratio_topost %>%  mutate(status_id_post = sentinel)
    }

    status_id_post <- ratio_was_posted_raw %>% pull(status_id_post)
    was_posted <- ifelse(status_id_post != sentinel, TRUE, FALSE)
    if(!was_posted) {
      text_post <- ratio_topost %>% pull(text_post)
      if(post) {
        reason <- "if there were not an error"
      } else {
        reason <- "if `post` were set to `TRUE`"
      }
      msg <-
        sprintf(
          paste0(
            "The message that would have been posted (%s) is:\n",
            "\"%s\""
          ),
          reason,
          text_post
        )
      message(msg)
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

    if(!was_posted) {
      # Note: This is "too" verbose (since another message
      # is most likely sent before with the usage of `post`).
      if(verbose) {
        msg <-
          sprintf(
            paste0(
              "Potential updates to `ratio_log_scrape` and `ratio_last_scrape` are ",
              "being reverted (because `sentinel` was detected) for \"%s\"."
          ),
          user
          )
        message(msg)
      }
      ratio_log_scrape_export <- ratio_log_scrape
      return(NULL)
    }

    path_ratio_log_scrape <- export_ratio_log_scrape_post(ratio_log_scrape_export)

    ratio_last_post_export <- .convert_ratio_log_scrape_to_last_post(ratio_log_scrape_export)
    path_ratio_last_post <- export_ratio_last_post(ratio_last_post_export)

    invisible(path_ratio_log_scrape)
  }

