
.interprete_scrape_method <-
  function(method) {
    switch(method, since = "last", until = "first")
  }

..import_ratio_file_scrape_possibly <-
  function(method, data, ...) {
    meaning <- .interprete_scrape_method(method = method)
    f <- sprintf(".import_ratio_%s_scrape_possibly", meaning)
    purrr::invoke(f, ...)
  }


..convert_ratio_file_scrape <-
  function(method, data, ...) {
    meaning <- .interprete_scrape_method(method = method)
    f <- sprintf(".convert_ratio_log_scrape_to_%s_scrape", meaning)
    purrr::invoke(f, list(data = data, ...))
  }

..export_ratio_file_scrape <-
  function(method, data, ...) {
    meaning <- .interprete_scrape_method(method = method)
    f <- sprintf("export_ratio_%s_scrape", meaning)
    purrr::invoke(f, list(data = data, ...))
  }

..import_ratio_file_scrape <-
  function(method,
           ratio_log_scrape,
           ratio_file_scrape = NULL,
           ...,
           verbose = config$verbose_scrape) {

    if (is.null(ratio_file_scrape)) {

      ratio_file_scrape <- ..import_ratio_file_scrape_possibly(method = method)

      if (is.null(ratio_file_scrape)) {
        if (verbose) {
          meaning <- .interprete_scrape_method(method = method)
          msg <-
            sprintf("Creating `ratio_%s_scrape` file from `ratio_log_scrape`.", meaning)
          message(msg)
        }

        ratio_file_scrape <-
          ..convert_ratio_file_scrape(
            method = method,
            data = ratio_log_scrape
          )

        path_ratio_file_scrape <-
          ..export_ratio_file_scrape(
            method = method,
            data = ratio_file_scrape
          )
      }
    }

    .compare_n_row_le(
      data1 = ratio_file_scrape,
      data2 = ratio_log_scrape
    )
    .validate_ratio_df_robustly(ratio_file_scrape)
    # .validate_ratio_df(ratio_file_scrape)
    .validate_ratio_onerowpergrp_df(ratio_file_scrape)
    ratio_file_scrape
  }

..get_tl_possibly <-
  function(method, ...) {
    # meaning <- .interprete_scrape_method()
    f <- sprintf(".get_tl_%s_possibly", method)
    purrr::invoke(f, ...)
  }

.do_scrape_ratio <-
  function(user,
           method = c("since", "until"),
           ...,
           tl = NULL,
           id = NULL,
           ratio_log_scrape = NULL,
           ratio_file_scrape = NULL,
           cache = config$tl_cache,
           sentinel = config$scrape_reply_sentinel,
           verbose = config$verbose_scrape) {


    # .user = "RealSkipBayless"
    # tl = NULL
    # id = NULL
    # ratio_log_scrape = NULL
    # ratio_last_scrape = NULL
    # cache = config$cache
    # verbose = config$verbose_scrape

    # message(rep("-", getOption("width")))
    # message(rep("-", 80L))

    .validate_user_scalar(user)
    method <- match.arg(method)
    message(method)
    # meaning <- .interprete_scrape_method(method)

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

    # .validate_ratio_df_robustly(ratio_log_scrape)
    .validate_ratio_df(ratio_log_scrape)

    # browser()
    if (is.null(tl)) {
      ratio_file_scrape <-
        ..import_ratio_file_scrape(
          method = method,
          ratio_log_scrape = ratio_log_scrape
        )

      if (is.null(id)) {
        ratio_file_scrape_filt <-
          ratio_file_scrape %>%
          .filter_byuser(.user = user)
        if(nrow(ratio_file_scrape_filt) > 0L) {
          id <-
            ratio_file_scrape_filt %>%
            pull(status_id)
          # tl <- .get_tl_since_possibly(.user = user, .id = id)
          tl <- ..get_tl_possibly(method = method, .user = user, .id = id)
        } else {
          tl <- .get_tl_first_possibly(.user = user)
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

    # ..f <- function(user, status_id) {
    #   message(user)
    #   message(status_id)
    #   return(-1L)
    # }
    reply <-
      tl_filt %>%
      mutate(
        reply_count =
          purrr::pmap_int(
            list(user, status_id),
            ~.get_reply_count_hack_possibly(user = ..1, status_id = ..2)
            # ~..f(user = ..1, status_id = ..2)
          )
      )

    reply_sentinel <-
      reply %>%
      filter(reply_count == sentinel)

    n_row_sentinel <- nrow(reply_sentinel)
    n_row_reply <-  nrow(reply)
    if(n_row_sentinel > 0L) {
      if(verbose) {
        msg <-
          sprintf(
            paste0(
              "There was an error in retrieving `reply_count` for %s tweets (out of %d)."
            ),
            n_row_sentinel,
            n_row_reply
          )
        message(msg)
      }
      if(n_row_sentinel == n_row_reply) {
        return(NULL)
      } else {
        reply <-
          reply %>%
          filter(reply_count != sentinel)
      }
    }

    ratio_scrape <-
      reply %>%
      .add_ratio_cols_at() %>%
      .add_timestamp_scrape_col_at() %>%
      .add_score_cols_at() %>%
      arrange(created_at)

    # browser()
    ratio_log_scrape_export <-
      bind_rows(
        ratio_log_scrape,
        ratio_scrape
      )

    .compare_n_row_eq(
      data1 = ratio_log_scrape_export,
      data2 = bind_rows(ratio_log_scrape, ratio_scrape)
    )

    path_ratio_log_scrape <-
      export_ratio_log_scrape(ratio_log_scrape_export)

    ratio_file_scrape_export <-
      ..convert_ratio_file_scrape(
        method = method,
        data = ratio_log_scrape_export
      )

    path_ratio_file_scrape <-
      ..export_ratio_file_scrape(
        method = method,
        data = ratio_file_scrape_export
      )

    invisible(reply)
  }


