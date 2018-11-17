
# Reference: sports-predict project (functions-scrape) for `reorder*()` and `select*()` functions below.
.COLS_TL_ORDER <-
  c(
    "user_id",
    "screen_name",
    "created_at",
    "status_id",
    "favorite_count",
    "retweet_count",
    "text"
  )

.COLS_RATIO_BASE_ORDER <-
  c(
    .COLS_TL_ORDER,
    "reply_count",
    "ratio",
    "ratio_inv",
    "timestamp_scrape"
  )
.COLS_RATIO_SCORE_ORDER <-
  c(
    "considered",
    "posted",
    "status_id_post",
    "text_post",
    "timestamp_post"
  )
.COLS_RATIO_ORDER <-
  c(
    .COLS_RATIO_BASE_ORDER,
    .COLS_RATIO_SCORE_ORDER
  )

.validate_twitter_df <-
  function(data, cols, ...) {
    stopifnot(is.data.frame(data))
    # stopifnot(nrow(data) > 0L)
    stopifnot(all(cols %in% names(data)))
  }

.validate_ratio_df <- purrr::partial(.validate_twitter_df, cols = .COLS_RATIO_ORDER)
.validate_tl_df <- purrr::partial(.validate_twitter_df, cols = .COLS_TL_ORDER)

.validate_twitter_onerowpergrp_df <-
  function(data, col, ...) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col))
    stopifnot(any(col == names(data)))
    col_sym <- sym(col)
    n_percol <-
      data %>%
      count(!!col_sym) %>%
      filter(n > 1L)
    if (ifelse(nrow(n_percol) > 0L, TRUE, FALSE)) {
      msg <- sprintf("Expected 1 value for each `%s`.", col)
      stop(msg, call. = FALSE)
    }
    invisible(data)
  }

.validate_ratio_onerowpergrp_df <-
  purrr::partial(.validate_twitter_onerowpergrp_df, col = "screen_name")

.reorder_twitter_cols_at <-
  function(data, ..., col_names_order) {
    col_names <- names(data)
    col_names_in <- intersect(col_names_order, col_names)
    col_names_nin <- setdiff(col_names, col_names_order)
    # length(col_names); length(col_names_in); length(col_names_nin); length(col_names_order)
    # setdiff(col_names, c(col_names_in, col_names_nin))
    col_names_fct <-
      factor(col_names, levels = c(col_names_in, col_names_nin))
    data %>% select(one_of(levels(col_names_fct)))
  }
.reorder_tl_cols_at <-
  purrr::partial(.reorder_twitter_cols_at, col_names_order = .COLS_TL_ORDER)
.reorder_ratio_cols_at <-
  purrr::partial(.reorder_twitter_cols_at, col_names_order = .COLS_RATIO_ORDER)

.select_twitter_cols_at <-
  function(data, ..., col_names, col_names_order = col_names) {
    col_names_fct <- factor(col_names, levels = col_names_order)
    data %>% select(one_of(levels(col_names_fct)))
  }
.select_tl_cols_at <-
  purrr::partial(.select_twitter_cols_at, col_names = .COLS_TL_ORDER)
.select_ratio_cols_at <-
  purrr::partial(.select_twitter_cols_at, col_names = .COLS_RATIO_ORDER)


.reconvert_datetime_cols <-
  function(data, cols = str_subset(names(data), "^created_at$|^timestamp"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(lubridate::ymd_hms))
  }

.reconvert_integer_cols <-
  function(data, cols = str_subset(names(data), "count|^considered$|^posted$"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.integer)) %>%
      mutate_at(vars(one_of(cols)), funs(coalesce(., 0L)))
  }

.reconvert_double_cols <-
  function(data, cols = str_subset(names(data), "^ratio"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.double)) %>%
      mutate_at(vars(one_of(cols)), funs(coalesce(., 0)))
  }

.reconvert_chr_cols <-
  function(data, cols = str_subset(names(data), "_id|^text_|screen_name"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character)) %>%
      mutate_at(vars(one_of(cols)), funs(coalesce(., "")))
  }

.import_ratio_file <-
  function(..., path, verbose = config$verbose_file) {
    if (!file.exists(path)) {
      msg <- sprintf("%s does not exist.", path)
      stop(msg, call. = FALSE)
    }
    # data <- data %>% rtweet:::unprepend_ids()
    data <- path %>% rtweet::read_twitter_csv()
    data <-
      data %>%
      .reconvert_datetime_cols() %>%
      .reconvert_chr_cols() %>%
      .reconvert_integer_cols() %>%
      .reconvert_double_cols()
    if (verbose) {
      msg <- sprintf("Imported data from %s at %s.", path, Sys.time())
      message(msg)
    }
    data
  }
import_ratio_last_scrape <-
  purrr::partial(.import_ratio_file, path = config$path_ratio_last_scrape)
import_ratio_log <-
  purrr::partial(.import_ratio_file, path = config$path_ratio_log)
import_ratio_last_post <-
  purrr::partial(.import_ratio_file, path = config$path_ratio_last_post)

.import_ratio_last_scrape_possibly <-
  purrr::possibly(import_ratio_last_scrape, otherwise = NULL)
.import_ratio_log_possibly <-
  purrr::possibly(import_ratio_log, otherwise = NULL)
.import_ratio_last_post_possibly <-
  purrr::possibly(import_ratio_last_post, otherwise = NULL)

import_screen_name <-
  function(..., path = config$path_screen_name, verbose = config$verbose_file) {
    if (!file.exists(path)) {
      msg <- sprintf("%s does not exist.", path)
      stop(msg, call. = FALSE)
    }
    data <- path %>% readr::read_csv()
    if (verbose) {
      msg <- sprintf("Imported data from %s at %s.", path, Sys.time())
      message(msg)
    }
    pull(data, 1)
  }

.import_screen_name_possibly <-
  purrr::possibly(import_screen_name, otherwise = NULL)

.export_twitter_file <-
  function(data, ..., path, append, backup = config$backup_file, verbose = config$verbose_file) {
    if (backup) {
      path_backup <- .create_backup(path = path)
      .clean_backup(path = path)
    }
    # NOTE: Can't use rtweet::write_csv() because it doesn't have `append`.
    # data <- data %>% rtweet:::flatten() %>% rtweet:::prepend_ids()
    data <- data %>% rtweet:::prepend_ids()
    write_csv(data, path, append = append, ...)
    if (verbose) {
      msg <- sprintf("Exported data to %s at %s.", path, Sys.time())
      message(msg)
    }
    invisible(path)
  }

export_ratio_last_scrape <-
  purrr::partial(.export_twitter_file, path = config$path_ratio_last_scrape, append = FALSE, backup = FALSE)
export_ratio_log <-
  purrr::partial(.export_twitter_file, path = config$path_ratio_log, append = file.exists(config$path_ratio_log), backup = FALSE)
export_ratio_last_post <-
  purrr::partial(.export_twitter_file, path = config$path_ratio_last_post, append = FALSE, backup = FALSE)

export_tl_cache <-
  function(data,
             screen_name,
             ...,
             append = FALSE,
             backup = TRUE,
             path = config$path_tl_cache,
             file = tools::file_path_sans_ext(path),
             ext = tools::file_ext(path),
             suffix = screen_name,
             path_cache = sprintf("%s-%s.%s", file, suffix, ext)) {
    .export_twitter_file(
      data = data,
      path = path_cache,
      append = append,
      backup = backup,
      ...
    )
  }


# TODO: Implement plural version of `col_filt`?
.convert_ratio_log_to_last_file <-
  function(data,
           col_grp = "screen_name",
           col_sort = "created_at",
           col_filt = "ratio",
           ...,
           verbose = config$verbose_file) {
    stopifnot(
      is.character(col_grp),
      is.character(col_sort),
      is.character(col_filt)
    )
    stopifnot(
      any(col_grp == names(data)),
      any(col_sort == names(data)),
      any(col_filt == names(data))
    )
    col_grp_sym <- sym(col_grp)
    col_sort_sym <- sym(col_sort)
    col_filt_sym <- sym(col_filt)

    res <-
      data %>%
      filter(!is.na(!!col_filt_sym))

    if(nrow(res) == 0L) {
      if(verbose) {
        msg <-
          sprintf(
            "No records to return in \"last\" file because is.na(`%s`) == TRUE.", col_filt
          )
        message(msg)
      }
      return(res)
    }
    # TODO: Implement check that number of groups is as expected?
    res <-
      res %>%
      group_by(!!col_grp_sym) %>%
      arrange(desc(!!col_sort_sym), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(!!col_grp_sym, desc(!!col_sort_sym))
    res
  }

.convert_ratio_log_to_last_scrape <-
  purrr::partial(.convert_ratio_log_to_last_file, col_filt = "ratio")
# # NOTE: Change `col_sort` to "timestamp_post" here?
.convert_ratio_log_to_last_post <-
  purrr::partial(.convert_ratio_log_to_last_file, col_filt = "status_id_post")

