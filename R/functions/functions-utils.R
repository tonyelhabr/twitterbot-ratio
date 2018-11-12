

# NOTE: This is a simplified verison of `tetidy::pull_distinctly()`.
.pull_distinctly <-
  function(data, col) {
    col <- enquo(col)
    data %>%
      distinct(!!col) %>% 
      arrange(!!col) %>% 
      pull(!!col)
  }


# NOTE: This function is inspired by the `.create_backup()` function (functions-db) in the sports-predict project.
.create_backup <-
  function(path, suffix_backup = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")) {
    stopifnot(file.exists(path))
    file <- tools::file_path_sans_ext(path)
    ext <- tools::file_ext(path)
    path_backup <-
      sprintf("%s-%s.%s", file, suffix_backup, ext)
    if(file.exists(path_backup)) {
      msg <- sprintf("Backup file %s already exists! Are you sure you want to overwrite it?", path_backup)
      stop(msg, call. = FALSE)
      return(FALSE)
    }
    invisible(file.copy(from = path, to = path_backup))
    path_backup
  }
