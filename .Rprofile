
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
