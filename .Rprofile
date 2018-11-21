
library("base")
library("methods")
library("datasets")
library("utils")
library("grDevices")
library("graphics")
library("stats")

path_r_profile <- "~/.Rprofile"
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm("path_r_profile")

suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library("rlang")))
suppressWarnings(suppressPackageStartupMessages(library("teplot")))
suppressWarnings(suppressPackageStartupMessages(library("rtweet")))

config <- config::get()

# Reference: https://github.com/mkearney/rtweet/issues/156.
# .TOKEN <- rtweet::get_token()
# .TOKEN <- readr::read_rds(as.character(Sys.getenv()["TWITTER_PAT"]))
.TOKEN <-
  rtweet::create_token(
    app = "pundit_ratio",
    consumer_key = "NDtgKE4sJAP6HQIzA1SV5IiwN",
    consumer_secret = "XH7q9Pvogd0e3knUd5Vry6Dc2EFDPutLFKqzOVnlZtaAqxrs3x",
    access_token = "1061748524168216576-IvpeGEwq9hgfxyg8lJetArNzd6rmMW",
    access_secret = "NzMug5p1xhtkcX9JGMjMD6Oc6nPFxDupp77uaxRCWJUfD"
  )
# print(.TOKEN)


paths_funcs <-
  list.files(
    path = file.path("R", "functions"),
    pattern = "func",
    recursive = FALSE,
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))
rm("paths_funcs")

# NOTE: Not sure why this isn't working from the .Renviron file.
options(readr.num_columns = 0)
options(width = 80)

# user <- import_user()
# regenerate_tone()
