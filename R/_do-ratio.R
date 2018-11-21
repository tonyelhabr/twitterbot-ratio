
# Note: command is /c C:/Users/aelhabr/Documents/R/R-3.4.0/bin/Rscript.exe "O:/_other/projects/twitterbot-ratio/R/_do-ratio.R" scrape
# rate_limit <- rtweet::rate_limit(token = rtweet::get_token())

dir_proj <- "O:/_other/projects/"
if(!dir.exists(dir_proj)) {
  dir_proj <- "C:/Users/aelhabr/Documents/projects/"
}
if(!dir.exists(dir_proj)) {
  stop("Project directory does not exist!", call. = FALSE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  proj <- "twitterbot-ratio"
  wd <- file.path(dir_proj, proj)
  setwd(wd)

  if(grepl("^O:", dir_proj)) {
    message("This script won't work on your system!")
  } else {
    invisible(source(".Rprofile"))
  }
  # token <- rtweet::get_token()

} else {
  message("Not interactive!")
  args <- character()
  # args <- "scrape"
}

# message("args: ", args)

if (length(args) == 0) {
  if(grepl("^O:", dir_proj)) {
    args <- "default"
  } else {
    args <- "scrape"
    # args <- "post"
  }
} else {
  if (!(any(c("post", "scrape") %in% args))) {
    message(sprintf('You must specify either "scrape" or "post" (not "%s"). Defaulting to "default".', args))
    args <- "default"
  }
}

# message("args: ", args)

f <-
  switch(
    args,
    post = do_post_ratio_all,
    scrape = do_scrape_ratio_all,
    default = function(x) {
      if (interactive()) {
        # return(do_scrape_ratio_all)
        return(message("You're in interactive mode. This script is not intended to be run like this"))
      } else {
        return(message("Doing nothing..."))
      }
    }
  )

# Note: Using `try()` instead of `purrr::possibly()` to avoid loading any packages
# (in the case that the system won't be able to run this script anyways.\).
try(pre_auto(), silent = TRUE)
f()
try(post_auto(), silent = TRUE)
