
.do_scrape_ratio <-
  function(user,
           tl = NULL,
           since_id = NULL,
           ratio_log_scrape = NULL,
           ratio_last_scrape = NULL,
           ...,
           cache = config$tl_cache,
           verbose = config$verbose_scrape) {

    # user = "RealSkipBayless"
    # tl = NULL
    # since_id = NULL
    # ratio_log_scrape = NULL
    # ratio_last_scrape = NULL
    # cache = config$cache
    # verbose = config$verbose_scrape

    # message(rep("-", getOption("width")))
    # message(rep("-", 80L))
    .validate_user_1(user)

    if (is.null(ratio_log_scrape)) {
      ratio_log_scrape <- .import_ratio_log_scrape_possibly()
      if (is.null(ratio_log_scrape)) {
        msg <-
          paste0(
            "Is this the first time you are doing this? ",
            "(If so, you should create the `ratio_log_scrape` file explicitly.)"
          )
        stop(msg, call. = FALSE)
      }
    }

    .validate_ratio_df_robustly(ratio_log_scrape)

    if (is.null(ratio_last_scrape)) {
      ratio_last_scrape <- .import_ratio_last_scrape_possibly()

      if (is.null(ratio_last_scrape)) {
        if (verbose) {
          msg <-
            sprintf("Creating missing `ratio_last_scrape` file from `ratio_log_scrape`.")
          message(msg)
        }
        ratio_last_scrape <- .convert_ratio_log_scrape_to_last_scrape(ratio_log_scrape)
        export_ratio_last_scrape(ratio_last_scrape)
      }
    }

    .compare_n_row_le(
      data1 = ratio_last_scrape,
      data2 = ratio_log_scrape
    )
    .validate_ratio_df_robustly(ratio_last_scrape)
    .validate_ratio_onerowpergrp_df(ratio_last_scrape)

    if (is.null(tl)) {
      if (is.null(since_id)) {
        ratio_last_scrape_filt <-
          ratio_last_scrape %>%
          filter(user == !!user)
        if(nrow(ratio_last_scrape_filt) > 0L) {
          since_id <-
            ratio_last_scrape_filt %>%
            pull(status_id)
          tl <- .get_tl_since_possibly(user = user, since_id = since_id)
        } else {
          tl <- .get_tl_first_possibly(user = user)
        }
      }

      if(is.null(tl)) {
        msg <- sprintf("Something went wrong when retrieving tweets for \"%s\".", user)
        message(msg)
        return(NULL)
      }

      if(nrow(tl) == 0L) {
        msg <- sprintf("Did not find any tweets for \"%s\".", user)
        message(msg)
        return(NULL)
      }

    }

    .validate_tl_df(tl)

    tl <- .select_tl_cols_at(tl)
    tl_filt <- .filter_tl_bytime(tl)

    n_row_tl_filt <- nrow(tl_filt)
    if (verbose) {
      if(n_row_tl_filt == 0L) {
        msg <- sprintf("No tweets to evaluate for \"%s\".", user)
        message(msg)
        return(NULL)
      }
      msg <- sprintf("Evaluating %d tweet(s) for \"%s\".", n_row_tl_filt, user)
      message(msg)
    }

    if(cache) {
      path_tl_cache <- .export_tl_cache(tl_filt, user)
    }

    reply <-
      tl_filt %>%
      mutate(
        reply_count =
          purrr::pmap_int(
            list(user, status_id),
            ~.get_reply_count_hack_possibly(user = ..1, status_id = ..2)
            )
      )

    ratio_log_scrape_export <-
      reply %>%
      .add_ratio_cols_at() %>%
      .add_timestamp_scrape_col_at() %>%
      .add_score_cols_at() %>%
      arrange(created_at)

    path_ratio_log_scrape <- export_ratio_log_scrape_scrape(ratio_log_scrape_export)

    ratio_last_scrape_export <-
      bind_rows(
        ratio_last_scrape,
        ratio_log_scrape_export
      ) %>%
      .convert_ratio_log_scrape_to_last_scrape()

    path_ratio_last_scrape <- export_ratio_last_scrape(ratio_last_scrape_export)

    invisible(reply)
  }

do_scrape_ratio_all <-
  function(user = NULL, ..., backup = TRUE, progress = TRUE) {
    if(is.null(user)) {
      user <- get_user_toscrape()
      user <- user[6:10]
    }
    .validate_user_vector(user)
    if(backup) {
      .create_backup(path = config$path_ratio_log_scrape)
    }
    if(progress) {
      pb <-
        progress::progress_bar$new(
          format = "[:bar] :percent eta :eta\n",
          total = length(user),
          width = 80
        )
      .f <- function(.user, .pb) {
        .do_scrape_ratio(user = .user)
        .pb$tick()
      }
      purrr::walk(user, ~.f(.user = .x, .pb = pb))
    } else {
      .f <- .do_scrape_ratio
      purrr::walk(user, ~.f(.user = .x))
    }
  }

