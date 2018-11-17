
# dir_wd <- "C:/Users/aelhabr/Documents/projects/"
# # dir_wd <- "O:/_other/projects/"
# prj <- "twitterbot-ratio"
# wd <- file.path(dir_wd, prj)
# setwd(wd)
# invisible(source(".Rprofile"))
# # token <- rtweet::get_token()
#
# message(rep("*", 80L))
# msg <- sprintf("Started script at %s.", Sys.time())
# message(msg)
pre_auto()
f_possibly <-
  purrr::possibly(do_scrape_ratio_all, otherwise = message("Encountered an error..."))
# do_scrape_ratio_all()
f_possibly()

# msg <- sprintf("Finished script at %s.", Sys.time())
# message(msg)
# message(rep("*", 80L))
post_auto()
