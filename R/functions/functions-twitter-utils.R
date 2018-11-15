
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

.COLS_RATIO_ORDER <-
  c(
    .COLS_TL_ORDER,
    "reply_count",
    "ratio",
    "ratio_inv",
    "timestamp_scrape"
  )

..reorder_twitter_cols_at <-
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
  purrr::partial(..reorder_twitter_cols_at, col_names_order = .COLS_TL_ORDER)
.reorder_ratio_cols_at <-
  purrr::partial(..reorder_twitter_cols_at, col_names_order = .COLS_RATIO_ORDER)

..select_twitter_cols_at <-
  function(data, ..., col_names, col_names_order = col_names) {
    col_names_fct <- factor(col_names, levels = col_names_order)
    data %>% select(one_of(levels(col_names_fct)))
  }
.select_tl_cols_at <-
  purrr::partial(..select_twitter_cols_at, col_names = .COLS_TL_ORDER)
.select_ratio_cols_at <-
  purrr::partial(..select_twitter_cols_at, col_names = .COLS_RATIO_ORDER)

# TODO: Figure out a better way to do this. (Maybe don't do this at all? Or maybe use `purrr::possiblye()`?)
..get_tl_df_default <-
  function(...) {
    tibble(
      user_id = character(),
      screen_name = character(),
      created_at = lubridate::as_datetime(character()),
      status_id = character(),
      favorite_count = integer(),
      retweet_count = integer(),
      text = character()
    ) %>%
      .reorder_tl_cols_at()
  }

..get_ratio_df_default <-
  function(...) {
    bind_cols(
      ..get_tl_df_default(),
      tibble(
        reply_count = double(),
        ratio = double(),
        ratio_inv = double(),
        timestamp_scrape = lubridate::as_datetime(character())
      )
    ) %>%
      .reorder_ratio_cols_at()
  }

.unconvert_id_cols_at <-
  function(data, cols = str_subset(names(data), "user_id|status_id"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character))
  }

.unconvert_datetime_cols <-
  function(data, cols = str_subset(names(data), "created_at|timestamp"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character))
  }

.reconvert_datetime_cols <-
  function(data, cols = str_subset(names(data), "created_at|timestamp"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(lubridate::ymd_hms))
  }


.import_ratio_file <-
  function(..., path, verbose = config$verbose_file) {

    if(!file.exists(path)) {
      if(verbose) {
        msg <- sprintf("%s does not exist.", path)
        message(msg)
      }
      return(NULL)
    }
    # data <- data %>% rtweet:::unprepend_ids()
    data <- path %>% rtweet::read_twitter_csv()
    data <- data %>% .reconvert_datetime_cols()
    if(verbose) {
      msg <- sprintf("Imported data from %s at %s.", path, Sys.time())
      message(msg)
    }
    data
  }
import_ratio_last <- purrr::partial(.import_ratio_file, path = config$path_last)
import_ratio_log <- purrr::partial(.import_ratio_file, path = config$path_log)

.export_twitter_file <-
  function(data, ..., path, append, backup = config$backup_file, verbose = config$verbose_file) {
    if(backup) {
      path_backup <- .create_backup(path = path)
      .clean_backup(path = path)
    }
    # NOTE: Can't use rtweet::write_csv() because it doesn't have `append`.
    # data <- data %>% rtweet:::flatten() %>% rtweet:::prepend_ids()
    data <- data %>% rtweet:::prepend_ids()
    write_csv(data, path, append = append, ...)
    if(verbose) {
      msg <- sprintf("Exported data to %s at %s.", path, Sys.time())
      message(msg)
    }
    invisible(path)
  }

export_ratio_last <- purrr::partial(.export_twitter_file, path = config$path_last, append = FALSE, backup = FALSE)
export_ratio_log <- purrr::partial(.export_twitter_file, path = config$path_log, append = file.exists(config$path_log), backup = FALSE)

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

