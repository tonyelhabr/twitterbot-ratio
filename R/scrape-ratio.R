
if(!interactive()) {
  dir_wd <- "C:/Users/aelhabr/Documents/projects/"
  # dir_wd <- "O:/_other/projects/"
  prj <- "twitterbot-ratio"
  wd <- file.path(dir_wd, prj)
  setwd(wd)
  invisible(source(".Rprofile"))
  # token <- rtweet::get_token()
}

if(!interactive()) {
  msg <- sprintf("Started script at %s.", Sys.time())
  message(msg)
}

# .SCREEN_NAME <- c("bomani_jones")
.SCREEN_NAME <- c("RealSkipBayless", "stephenasmith", "NateSilver538", "bomani_jones")
purrr::walk(
  .SCREEN_NAME,
  ~do_get_ratio(.x)
)

if(!interactive()) {
  msg <- sprintf("Finished script at %s.", Sys.time())
  message(msg)
}
