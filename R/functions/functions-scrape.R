
# NOTE: See the sports-predict project (functions-db-tr).
.TIME <- Sys.time()
.get_ymdhm <-
  function(time = .TIME, resolution = 1L) {
    stopifnot(lubridate::is.POSIXct(time))
    stopifnot(is.integer(resolution))
    m1 <- as.numeric(strftime(round.POSIXt(time, "mins"), "%M"))
    m2 <- resolution * round((m1 - resolution - 1L) / resolution)
    # ymdh <- strftime(time, "%Y%m%d%H")
    ymd <- strftime(time, "%Y-%m-%d")
    h <- strftime(time, "%H")
    # ymdh_round <- sprintf("%s%02.0f00", ymdh, m2)
    ymdh_round <- sprintf("%s %s:%02.0f:00", ymd, h, m2)
    lubridate::ymd_hms(ymdh_round)
  }

.add_timestamp_scrape_col_at <-
  function(data, col = "timestamp_scrape", ...) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, !(col %in% names(data)))
    col <- sym(col)
    data %>%
      mutate(!!col := .get_ymdhm(...))
  }


.add_scrape_cols_at <-
  function(data, ...) {
    data %>%
      .add_timestamp_scrape_col_at()
  }

.finalize_scrape_cols_at <-
  function(data, ...) {
    data %>%
      .add_scrape_cols_at() %>% 
      .reorder_twitter_cols_at()
  }