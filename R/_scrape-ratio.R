
dir_wd <- "C:/Users/aelhabr/Documents/projects/"
# dir_wd <- "O:/_other/projects/"
prj <- "twitterbot-ratio"
wd <- file.path(dir_wd, prj)
setwd(wd)
invisible(source(".Rprofile"))
# token <- rtweet::get_token()

pre_auto()
# f_possibly <- purrr::possibly(do_scrape_ratio_all, otherwise = message(paste0("Encountered an error...")))
do_scrape_ratio_all()
# f_possibly()
post_auto()
