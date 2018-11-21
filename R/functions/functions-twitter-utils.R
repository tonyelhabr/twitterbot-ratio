

# Reference: sports-predict project (functions-scrape) for `reorder*()` and `select*()` functions below.
.COLS_USER_ORDER <-
  c("user",
    "category1",
    "category2",
    "user_tone",
    "audience_tone",
    "scrape",
    "post")

.COLS_USER_INFO_ORDER <-
  c("user",
    "followers_count",
    "statuses_count",
    "name",
    "description",
    "list")


.COLS_SENTIMENT_ORDER <-
  c("tone",
    "mood")

.COLS_TL_ORDER <-
  c(
    "user_id",
    "user",
    "created_at",
    "status_id",
    "favorite_count",
    "retweet_count",
    "text"
  )

.COLS_RATIO_BASE_ORDER <-
  c(.COLS_TL_ORDER,
    "reply_count",
    "ratio",
    "timestamp_scrape")
.COLS_RATIO_SCORE_ORDER <-
  c(# "qualified",
    "considered",
    "posted",
    "status_id_post",
    "text_post",
    "timestamp_post")
.COLS_RATIO_ORDER <-
  c(.COLS_RATIO_BASE_ORDER,
    .COLS_RATIO_SCORE_ORDER)

# validate_xxx_df ----
.validate_df <-
  function(data, cols, ...) {
    stopifnot(is.data.frame(data))
    # stopifnot(nrow(data) > 0L)
    stopifnot(all(cols %in% names(data)))
  }

.validate_ratio_df <-
  purrr::partial(.validate_df, cols = .COLS_RATIO_ORDER)
# Note: This function can be replaced with the simplified version above
# once all other functions are "stable"/"finalized". Until then, this is useful
# for checking the integrity of the data.
.validate_ratio_df_robustly <-
  function(data, cols = .COLS_RATIO_ORDER, ...) {
    # # Debugging...
    # data = import_ratio_log_scrape()
    # cols = cols = .COLS_RATIO_ORDER

    .validate_df(data = data, cols = cols)

    data_filt <- data %>% filter(!considered %in% c(0L, 1L))
    stopifnot(nrow(data_filt) == 0L)

    data_filt <- data %>% filter(!posted %in% c(0L, 1L))
    stopifnot(nrow(data_filt) == 0L)

    # Note: Can remove this check if this field is ever populated in the future.
    # (Currently, it is not used.)
    data_filt <- data %>% filter(status_id_post != "")
    stopifnot(nrow(data_filt) == 0L)

    data_filt <-
      data %>% filter((posted == 1L) & (considered == 0L))
    stopifnot(nrow(data_filt) == 0L)

    data_filt <-
      data %>% filter(considered == 1L & is.na(timestamp_post))
    stopifnot(nrow(data_filt) == 0L)

    data_filt <- data %>% filter((posted == 1L) & (text_post == ""))
    stopifnot(nrow(data_filt) == 0L)

    data_filt <- data %>% filter((posted == 0L) & (text_post != ""))
    stopifnot(nrow(data_filt) == 0L)

  }

.validate_tl_df <-
  purrr::partial(.validate_df, cols = .COLS_TL_ORDER)
.validate_user_df <-
  purrr::partial(.validate_df, cols = .COLS_USER_ORDER)
.validate_user_info_df <-
  purrr::partial(.validate_df, cols = .COLS_USER_INFO_ORDER)
.validate_tone_df <-
  purrr::partial(.validate_df, cols = .COLS_SENTIMENT_ORDER)

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
  purrr::partial(.validate_twitter_onerowpergrp_df, col = "user")

.validate_user_vector <-
  function(x, ...) {
    stopifnot(is.character(x))
  }

.validate_user_scalar <-
  function(x, ...) {
    stopifnot(is.character(x), length(x) == 1L)
  }

# reorder_xxx_cols_at  ----
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

# select_xxx_cols_at ----
.select_twitter_cols_at <-
  function(data, ..., col_names, col_names_order = col_names) {
    col_names_fct <- factor(col_names, levels = col_names_order)
    data %>% select(one_of(levels(col_names_fct)))
  }
.select_tl_cols_at <-
  purrr::partial(.select_twitter_cols_at, col_names = .COLS_TL_ORDER)
.select_ratio_cols_at <-
  purrr::partial(.select_twitter_cols_at, col_names = .COLS_RATIO_ORDER)

# reconvert_xxx_cols_at_at ----
.reconvert_datetime_cols_at <-
  function(data,
           cols = str_subset(names(data), "^created_at$|^timestamp"),
           ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(lubridate::ymd_hms))
  }

.reconvert_integer_cols_at <-
  function(data,
           cols = str_subset(names(data), "count|^considered$|^posted$"),
           ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.integer)) %>%
      mutate_at(vars(one_of(cols)), funs(coalesce(., 0L)))
  }

.reconvert_double_cols_at <-
  function(data, cols = str_subset(names(data), "^ratio$"), ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.double)) %>%
      mutate_at(vars(one_of(cols)), funs(coalesce(., 0)))
  }

.reconvert_chr_cols_at <-
  function(data,
           cols = str_subset(names(data), "_id|^text_|user"),
           ...) {
    data %>%
      mutate_at(vars(one_of(cols)), funs(as.character)) %>%
      mutate_at(vars(one_of(cols)), funs(coalesce(., "")))
  }

# import_xxx ----
.preprocess_import <-
  function(path, ..., verbose = config$verbose_file) {
    if (!file.exists(path)) {
      msg <- sprintf("%s does not exist.", path)
      stop(msg, call. = FALSE)
    }
  }

.postprocess_import <-
  function(data, path, ..., verbose = config$verbose_file) {
    if (verbose) {
      msg <- sprintf("Imported data from %s at %s.", path, Sys.time())
      message(msg)
    }
    invisible(data)
  }

.import_twitter_file <-
  function(...,
           path,
           f_validate = NULL,
           verbose = config$verbose_file) {
    .preprocess_import(path = path)
    # data <- data %>% rtweet:::unprepend_ids()
    data <- rtweet::read_twitter_csv(path)
    if (!is.null(f_validate)) {
      f_validate(data)
    }
    data <-
      data %>%
      .reconvert_datetime_cols_at() %>%
      .reconvert_chr_cols_at() %>%
      .reconvert_integer_cols_at() %>%
      .reconvert_double_cols_at()
    .postprocess_import(data = data, path = path)
  }

.import_ratio_file <- .import_twitter_file

import_ratio_last_scrape <-
  purrr::partial(
    .import_ratio_file,
    path = config$path_ratio_last_scrape,
    f_validate = .validate_ratio_df
  )
import_ratio_log_scrape <-
  purrr::partial(.import_ratio_file,
                 path = config$path_ratio_log_scrape,
                 f_validate = .validate_ratio_df)
import_ratio_last_post <-
  purrr::partial(.import_ratio_file,
                 path = config$path_ratio_last_post,
                 f_validate = .validate_ratio_df)

.import_ratio_last_scrape_possibly <-
  purrr::possibly(import_ratio_last_scrape, otherwise = NULL)
.import_ratio_log_scrape_possibly <-
  purrr::possibly(import_ratio_log_scrape, otherwise = NULL)
.import_ratio_last_post_possibly <-
  purrr::possibly(import_ratio_last_post, otherwise = NULL)


.import_nontwitter_file <-
  function(...,
           path,
           f_validate = NULL,
           verbose = config$verbose_file) {
    .preprocess_import(path = path, ...)
    data <- readr::read_csv(path)
    if (!is.null(f_validate)) {
      f_validate(data)
    }
    .postprocess_import(data = data, path = path, ...)
  }

import_user <-
  purrr::partial(.import_nontwitter_file,
                 path = config$path_user,
                 f_validate = .validate_user_df)
import_user_info <-
  purrr::partial(.import_nontwitter_file,
                 path = config$path_user_info,
                 f_validate = .validate_user_info_df)
import_tone <-
  purrr::partial(.import_nontwitter_file,
                 path = config$path_tone,
                 f_validate = .validate_tone_df)

.flatten_user <-
  function(data, ...) {
    data %>% pull(user)
  }

# Note: This function requires that both data sets be non-`NULL`.
.join_user_and_tone <-
  function(user, tone, ...) {
    suppressMessages(user %>%
                       left_join(tone %>% rename_all(funs(
                         paste0("user_", .)
                       ))) %>%
                       left_join(tone %>% rename_all(funs(
                         paste0("audience_", .)
                       ))))
  }

.filter_user_scrape <-
  function(data, tone = NULL, ...) {
    stopifnot(is.data.frame(data))
    # TODO: Do something with this?
    # if(is.null(tone)) {
    #   tone <- import_tone()
    # }
    data %>% filter(scrape == 1L)
  }

.filter_user_post <-
  function(data, ...) {
    data %>% filter(post == 1L)
  }

get_user_toscrape <-
  function(...) {
    data <- import_user()
    data %>%
      .filter_user_scrape() %>%
      .flatten_user()
  }

get_user_topost <-
  function(...) {
    ratio_log_scrape <- import_ratio_log_scrape()
    user <- import_user()
    data <-
      user %>%
      inner_join(ratio_log_scrape, by = "user") %>%
      distinct(user)
    data %>%
      .filter_user_post() %>%
      .flatten_user()
  }

# export_xxx ----
.preprocess_export <-
  function(path, backup, ..., verbose = config$verbose_file) {
    if (backup) {
      path_backup <- .create_backup(path = path)
      .clean_backup(path = path)
    }
  }

.postprocess_export <-
  function(data, path, ..., verbose = config$verbose_file) {
    if (verbose) {
      msg <- sprintf("Exported data to %s at %s.", path, Sys.time())
      message(msg)
    }
    invisible(path)
  }

.export_twitter_file <-
  function(data,
           ...,
           path,
           append,
           na = "",
           backup = config$backup_file,
           verbose = config$verbose_file) {
    .preprocess_export(path = path, backup = backup, ...)
    # Note: Can't use rtweet::write_csv() because it doesn't have `append`.
    # data <- data %>% rtweet:::flatten() %>% rtweet:::prepend_ids()
    data <- rtweet:::prepend_ids(data)
    write_csv(data, path, append = append, na = na, ...)
    .postprocess_export(data = data, path = path, ...)
  }

export_ratio_last_scrape <-
  purrr::partial(
    .export_twitter_file,
    path = config$path_ratio_last_scrape,
    append = FALSE,
    backup = FALSE
  )
export_ratio_log_scrape_scrape <-
  purrr::partial(
    .export_twitter_file,
    path = config$path_ratio_log_scrape,
    append = file.exists(config$path_ratio_log_scrape),
    backup = FALSE
  )
export_ratio_log_scrape_post <-
  purrr::partial(
    .export_twitter_file,
    path = config$path_ratio_log_scrape,
    append = FALSE,
    backup = FALSE
  )
export_ratio_last_post <-
  purrr::partial(
    .export_twitter_file,
    path = config$path_ratio_last_post,
    append = FALSE,
    backup = FALSE
  )

.export_tl_cache <-
  function(data,
           user,
           ...,
           append = FALSE,
           backup = TRUE,
           path = config$path_tl_cache,
           file = tools::file_path_sans_ext(path),
           ext = tools::file_ext(path),
           suffix = user,
           path_cache = sprintf("%s-%s.%s", file, suffix, ext)) {
    .export_twitter_file(
      data = data,
      path = path_cache,
      append = append,
      backup = backup,
      ...
    )
  }

regenerate_tone <-
  function(user = NULL,
           tone = NULL,
           ...,
           path = config$path_tone,
           backup = TRUE,
           na = "",
           append = FALSE,
           verbose = config$verbose_regenerate) {
    .preprocess_export(path = path, backup = backup)

    if (is.null(user)) {
      user <- import_user()
    }
    # Note: This validation is intended for the case where `user` is not `NULL`.
    .validate_user_df(user)

    # Note: `tone_old` is purely for comparison's sake.
    if (is.null(tone)) {
      # Note: Don't throw an error because the file may not exist
      # (before the first time it is created).
      .f <- purrr::possibly(import_tone, otherwise = NULL)
      tone_old <- .f()
    }

    tone_exist <-
      user %>%
      gather(actor, tone, matches("tone$")) %>%
      mutate_at(vars(actor), funs(str_remove_all(., "_tone$"))) %>%
      distinct(tone)

    suppressMessages(tone_new <-
                       tone_exist %>%
                       left_join(
                         tribble(
                           ~ tone,
                           ~ mood,
                           "debate",
                           "negative",
                           "fun",
                           "positive",
                           "mockery",
                           "negative",
                           "negative",
                           "negative",
                           "neutral",
                           "neutral",
                           "sarcastic",
                           "positive",
                           "supportive",
                           "positive",
                           "positive",
                           "positive",
                           "unknown",
                           "neutral"
                         )
                       ))

    .compare_n_row_le(
      data1 = tone_exist,
      data2 = tone_new,
      message = TRUE,
      stop = FALSE
    )

    if (!is.null(tone_old)) {
      .compare_n_row_le(
        data1 = tone_old,
        data2 = tone_new,
        message = TRUE,
        stop = FALSE
      )
    }

    write_csv(
      x = tone_new,
      path = path,
      na = na,
      append = append,
      ...
    )
    .postprocess_export(data = tone_new, path = path, ...)
  }

# TODO: Make a more general `regenarate*()` function to combine with the above one?
regenerate_user_info <-
  function(user = NULL,
           user_info = NULL,
           ...,
           path = config$path_scree_name_info,
           backup = TRUE,
           na = "",
           append = FALSE,
           verbose = config$verbose_regenerate) {
    .preprocess_export(path = path, backup = backup)

    if (is.null(user)) {
      user <- get_user_toscrape()
    }
    # Note: This validation is intended for the case where `user` is not `NULL`.
    .validate_user_vector(user)

    if (is.null(user_info)) {
      .f <- purrr::possibly(import_user_info, otherwise = NULL)
      user_info_old <- .f()
    }

    user_info_new <-
      user %>%
      rtweet::lookup_users() %>%
      mutate(list = "") %>%
      select(one_of(.COLS_USER_INFO_ORDER))

    if (!is.null(user_info_old)) {
      .compare_n_row_le(
        data1 = user_info_old,
        data2 = user_info_new,
        message = TRUE,
        stop = FALSE
      )
    }

    write_csv(
      x = user_info_new,
      path = path,
      na = na,
      append = append,
      ...
    )
    .postprocess_export(data = user_info_new, path = path, ...)
  }

# convert_xxx ----
# TODO: Implement plural version of `col_filt`?
.convert_ratio_log_scrape_to_last_file <-
  function(data,
           col_grp = "user",
           col_sort = "created_at",
           col_filt = "ratio",
           ...,
           verbose = config$verbose_file) {
    stopifnot(is.character(col_grp),
              is.character(col_sort),
              is.character(col_filt))
    stopifnot(any(col_grp == names(data)),
              any(col_sort == names(data)),
              any(col_filt == names(data)))
    col_grp_sym <- sym(col_grp)
    col_sort_sym <- sym(col_sort)
    col_filt_sym <- sym(col_filt)

    res <-
      data %>%
      filter(!is.na(!!col_filt_sym) & (!!col_filt_sym != ""))

    if (nrow(res) == 0L) {
      if (verbose) {
        msg <-
          sprintf("No records to return in \"last\" file because is.na(`%s`) == TRUE.",
                  col_filt)
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

.convert_ratio_log_scrape_to_last_scrape <-
  purrr::partial(.convert_ratio_log_scrape_to_last_file, col_filt = "ratio")
# # Note: Change `col_sort` to "timestamp_post" here?
.convert_ratio_log_scrape_to_last_post <-
  purrr::partial(.convert_ratio_log_scrape_to_last_file, col_filt = "text_post")

# do_xxx ----
.preprocess_do_action_ratio <-
  function(user = NULL, ...) {
    if (is.null(user)) {
      user <- get_user_toscrape()
    }
    .validate_user(user)
    invisible(user)
  }

# misc ----
# Note: This is used for both `scrape` and `post` (although it was initially
# only developed to used for `scrape`).
.make_twitter_url_reply <-
  function(.user, .status_id, ...) {
    sprintf("https://twitter.com/%s/status/%s", .user, .status_id)
  }

# Note: This verbose function is useful to avoid conflicts/renaming of `user`
# (initially in the `.do_post_ratio()` function, but perhaps elsewhere as well).
.filter_byuser <-
  function(data, .user, ...) {
    stopifnot(length(.user) == 1L)
    data %>% filter(user == .user)
  }

# unused ----
.COLS_LISTS_MEMBERS_ORDER <-
  c(
    "user_id",
    "user",
    "location",
    "description",
    "url",
    "protected",
    "followers_count",
    "friends_count",
    "listed_count",
    "created_at",
    "favourites_count",
    "utc_offset",
    "time_zone",
    "geo_enabled",
    "verified",
    "statuses_count",
    "lang",
    "contributors_enabled",
    "is_translator",
    "is_translation_enabled",
    "profile_background_color",
    "profile_background_image_url",
    "profile_background_image_url_https",
    "profile_background_tile",
    "profile_image_url",
    "profile_image_url_https",
    "profile_banner_url",
    "profile_link_color",
    "profile_sidebar_border_color",
    "profile_sidebar_fill_color",
    "profile_text_color",
    "profile_use_background_image",
    "has_extended_profile",
    "default_profile",
    "default_profile_image",
    "following",
    "follow_request_sent",
    "notifications",
    "translator_type"
  )

.COLS_LOOKUP_USERS_ORDER <-
  c(
    "user_id",
    "name",
    "user",
    "location",
    "description",
    "url",
    "protected",
    "followers_count",
    "friends_count",
    "listed_count",
    "created_at",
    "favourites_count",
    "verified",
    "statuses_count",
    "lang",
    "profile_image_url",
    "profile_banner_url"
  )
