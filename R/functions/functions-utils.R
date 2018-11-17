

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
  function(path,
           ...,
           file = tools::file_path_sans_ext(path),
           ext = tools::file_ext(path),
           suffix_backup = format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
           path_backup = sprintf("%s-%s.%s", file, suffix_backup, ext),
           verbose = config$verbose_file) {

    if(!file.exists(path)) {
      msg <-
        sprintf(
          "Backup file %s cannot be created because %s cannot be found!",
          path_backup,
          path
        )
      message(msg, call. = FALSE)
      return(path_backup)
    }

    if(file.exists(path_backup)) {
      msg <-
        sprintf(
          "Backup file %s already exists! Are you sure you want to overwrite it?",
          path_backup
        )
      stop(msg, call. = FALSE)
    }
    invisible(file.copy(from = path, to = path_backup))
    if(verbose) {
      msg <- sprintf("Backed up %s before exporting to %s.", path_backup, path)
      message(msg)
    }
    path_backup
  }

.clean_backup <-
  function(path,
           n_keep = 1L,
           decreasing = TRUE,
           ...,
           dir_backup = dirname(path),
           rgx_backup = paste0(tools::file_path_sans_ext(basename(path)), "-.*", tools::file_ext(path)),
           verbose = config$verbose_file) {
    paths_like_backup <-
      list.files(
        path = dir_backup,
        pattern = rgx_backup,
        recursive = FALSE,
        full.names = TRUE
      )
    n <- length(paths_like_backup)
    if(n < n_keep) {
      if(n == 0L) {
        msg <- sprintf("No backup files to delete.")
        message(msg, call. = FALSE)
        return(path)
      }
      msg <-
        sprintf(
          paste0(
            "Number of backup files (%.0f) is less than `keep` (%.0f), ",
                 "so not deleting any backup files."
                 ),
          n,
          n_keep
        )
      message(msg)
      return(path)
    }
    paths_to_keep <- sort(paths_like_backup, decreasing = decreasing)[1L:n_keep]
    paths_to_delete <- setdiff(paths_like_backup, paths_to_keep)
    invisible(sapply(paths_to_delete, unlink, recursive = TRUE, force = TRUE))
    if(verbose) {
      msg <- sprintf("Deleted %.0f backup files at %s.", length(paths_to_delete), Sys.time())
      message(msg)
    }
    invisible(path)
  }

.validate_onerow_df <-
  function(data, ...) {
    stopifnot(is.data.frame(data))
    n_row <- nrow(data)
    if (ifelse(n_row > 1L, TRUE, FALSE)) {
      msg <- sprintf("Expected `data` to have only 1 row. Instead, found `%s`.", n_row)
      stop(msg, call. = FALSE)
    }
    invisible(data)
  }
