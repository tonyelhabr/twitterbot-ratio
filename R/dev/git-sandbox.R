
# system('git config --global user.email "anthonyelhabr@gmail.com"')
# system('git config --global user.name "tonyelhabr"')
e <- Sys.getenv()
path_gitconfig <-
  list.files(
    # path = unname(file.path("C:", "users", Sys.info()["user"])),
    path = file.path(e["USERPROFILE"]),
    pattern = "gitconfig",
    all.files = TRUE,
    full.names = TRUE
  )

if(length(path_gitconfig) == 1L) {
  invisible(
    file.copy(
      from = path_gitconfig,
      to = file.path(e["HOME"], basename(path_gitconfig)),
      # recursive = TRUE,
      overwrite = TRUE
      )
    )
}

system('git status')
system('git add figs/')
system('git commit -m "updating figs"')
system('git push -u origin master')
