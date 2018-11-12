
.SCREEN_NAME <- c("RealSkipBayless", "stephenasmith", "pftcommenter")
# .N_SCREEN_NAME <- length(.SCREEN_NAME)
# .N_LAST <- 3L

tl_raw_init <-
  rtweet::get_timelines(
    user = .SCREEN_NAME,
    n = 100L
  )
tl_init <- 
  tl_raw_init %>%
  filter(created_at > lubridate::ymd("2018-11-08"), created_at <= lubridate::ymd("2018-11-10"))
ratio_init <-
  tl_init %>%
  do_get_ratio(tl = .)
ratio_init
