
# NOTE: See the sports-predict project (functions-db-tr).
.TIME <- Sys.time()
.get_ymdhm <-
  function(time = NULL, resolution = 1L) {
    if(is.null(time)) {
      time <- .TIME
    }
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
  function(data, col = "timestamp_scrape", value = NULL, ...) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1, !(col %in% names(data)))
    col <- sym(col)
    if(is.null(value)) {
      res <-
        data %>%
        mutate(!!col := .get_ymdhm(...))
    } else {
      res <-
        data %>%
        mutate(!!col := value)
    }
    res
  }


.add_scrape_cols_at <-
  function(data, ...) {
    data %>%
      .add_timestamp_scrape_col_at(...)
  }

