
# .SCREEN_NAME <- c("RealSkipBayless", "stephenasmith", "NateSilver538", "bomani_jones")
.SCREEN_NAME <- c("bomani_jones")
tl_init <-
  rtweet::get_timelines(
    user = .SCREEN_NAME,
    n = 100L
  )
purrr::pwalk(
  list(.SCREEN_NAME,, tl_init),
  ~do_get_ratio1(screen_name = ..1, tl = ..2)
)
