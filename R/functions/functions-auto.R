
pre_auto <-
  function(...) {
    dir_wd <- "C:/Users/aelhabr/Documents/projects/"
    # dir_wd <- "O:/_other/projects/"
    prj <- "twitterbot-ratio"
    wd <- file.path(dir_wd, prj)
    setwd(wd)
    invisible(source(".Rprofile"))
    # token <- rtweet::get_token()

    message(rep("*", 80L))
    msg <- sprintf("Started script at %s.", Sys.time())
    message(msg)
  }

post_auto <-
  function(...) {
    msg <- sprintf("Finished script at %s.", Sys.time())
    message(msg)
    message(rep("*", 80L))
  }
