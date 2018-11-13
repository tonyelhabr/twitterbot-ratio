
# .SCREEN_NAME <- c("RealSkipBayless", "stephenasmith", "NateSilver538", "bomani_jones")
# .SCREEN_NAME <- c("RealSkipBayless")
# .SCREEN_NAME <- c("NateSilver538")
.SCREEN_NAME <- c("bomani_jones")
tl_raw_init <-
  rtweet::get_timelines(
    user = .SCREEN_NAME,
    n = 100L
  )

ratio_init <-
  tl_raw_init %>%
  # filter(
  #   created_at > lubridate::ymd("2018-11-09"), 
  #   created_at <= lubridate::ymd("2018-11-11")
  # ) %>% 
  do_get_ratio(tl = .)
ratio_init
