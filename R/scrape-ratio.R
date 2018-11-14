
setwd("C:/Users/aelhabr/Documents/projects/twitterbot-ratio")
invisible(source(".Rprofile"))

# .SCREEN_NAME <- c("bomani_jones")
.SCREEN_NAME <- c("RealSkipBayless", "stephenasmith", "NateSilver538", "bomani_jones")
purrr::walk(
  .SCREEN_NAME,
  ~do_get_ratio1(.x)
)

