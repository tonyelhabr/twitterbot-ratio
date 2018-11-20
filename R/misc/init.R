
# .USER <- c("RealSkipBayless", "stephenasmith", "NateSilver538", "bomani_jones")
.USER <- c("bomani_jones")
tl_init <-
  rtweet::get_timelines(
    user = .USER,
    n = 100L
  )
purrr::pwalk(
  list(.USER,, tl_init),
  ~.do_scrape_ratio(user = ..1, tl = ..2)
)
