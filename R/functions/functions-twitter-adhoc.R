

update_ratio_last <-
  function(...) {
    ratio_log_import <- import_ratio_log()
    ratio_last_export <-
      ratio_log_import %>% 
      .arrange_ratio_df_at() %>% 
      filter(created_at <= (.TIME - lubridate::hours(.N_HOUR_LAG))) %>% 
      .slice_ratio_df_at()
    ratio_last_export %>% readr::write_csv(config$path_last, append = FALSE)
  }

distinctify_ratio_log <-
  function(...) {
    ratio_log_import <- import_ratio_log()
    ratio_log_export <-
      ratio_log_import %>%
      # NOTE: This is "too naive".
      # distinct() %>% 
      group_by(status_id) %>% 
      # NOTE: Not sure which is the "better" method.
      filter(replies_count == max(replies_count)) %>% 
      # filter(timestamp_scrape == last(timestamp_scrape)) %>% 
      .arrange_ratio_df_at()
    ratio_last_export %>% readr::write_csv(config$path_last, append = FALSE)
  }

.add_timestamp_scrape_cols_at <-
  function(data, col = "timestamp_scrape") {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, col %in% names(data))
    # cls <- data %>% pull(!!sym(col)) %>% class()
    # stopifnot(cls == "POSIXct")
    stopifnot(data %>% pull(!!sym(col)) %>% lubridate::is.POSIXct())
    data %>%
      mutate(
        year_scrape = lubridate::year(timestamp_scrape),
        month_scrape = lubridate::month(timestamp_scrape),
        day_scrape = lubridate::day(timestamp_scrape),
        hour_scrape = lubridate::hour(timestamp_scrape),
        minute_scrape = lubridate::minute(timestamp_scrape),
        second_scrape = lubridate::second(timestamp_scrape)
      )
  }

# Reference: sports-predict project (functions-db-utils) for `reorder*()` and `select*()` functions below.
distinctify_data_at <-
  function(data, unit = "hour", col = "timestamp_scrape", rgx_exclude = NULL) {
    
    # browser()
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, col %in% names(data))
    lvls_time <-
      c("year", "month", "day", "hour", "minute", "second")
    # stopifnot(unit %in% lvls_time)
    unit <- match.arg(arg = unit, choices = lvls_time, several.ok = FALSE)
    fcts_time <-
      factor(lvls_time, levels = lvls_time)
    idx_slice <- which(unit == fcts_time)
    fcts_sliced <- as.character(fcts_time[c(1L:idx_slice)])
    rgx_fcts_sliced <- paste("^", fcts_sliced, sep = "", collapse = "|")
    rgx_fcts_sliced_scrape <- paste("^", fcts_sliced, "_scrape", sep = "", collapse = "|")
    
    data_aug <-
      data %>%
      .add_timestamp_scrape_cols_at(col = col)
    
    cols_fcts_sliced <-
      data_aug %>% 
      names() %>% 
      str_subset(rgx_fcts_sliced)
    
    cols_fcts_sliced_scrape <-
      data_aug %>% 
      names() %>% 
      str_subset(rgx_fcts_sliced_scrape)
    
    cols_include <-
      data %>%
      names() %>% 
      setdiff(col)
    
    if(!is.null(rgx_exclude)) {
      stopifnot(is.character(rgx_exclude), length(rgx_exclude) == 1)
      cols_include <-
        cols_include %>%
        setdiff(str_subset(names(data_aug), rgx_exclude))
    }
    
    cols_distinct <- c(cols_include, cols_fcts_sliced, cols_fcts_sliced_scrape)
    cols_select <- names(data)
    data_aug %>% 
      distinct(!!!syms(cols_distinct), .keep_all = TRUE) %>% 
      select(!!!syms(cols_select)) %>% 
      inner_join(data)
  }
