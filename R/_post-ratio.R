
dir_wd <- "C:/Users/aelhabr/Documents/projects/"
# dir_wd <- "O:/_other/projects/"
prj <- "twitterbot-ratio"
wd <- file.path(dir_wd, prj)
setwd(wd)
invisible(source(".Rprofile"))
# token <- rtweet::get_token()

pre_auto()
do_post_ratio_all()
post_auto()
