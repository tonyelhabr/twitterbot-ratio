


# Note: This is a simplified verison of `tetidy::pull_distinctly()`.
.pull_distinctly <-
  function(data, col) {
    col <- enquo(col)
    data %>%
      distinct(!!col) %>%
      arrange(!!col) %>%
      pull(!!col)
  }


# Note: This function is inspired by the `.create_backup()` function
# in the sports-predict project.(See the "functions-db" file.)
.create_backup <-
  function(path,
           ...,
           file = tools::file_path_sans_ext(path),
           ext = tools::file_ext(path),
           suffix_backup = format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
           path_backup = sprintf("%s-%s.%s", file, suffix_backup, ext),
           clean = FALSE,
           verbose = config$verbose_file) {
    if (!file.exists(path)) {
      if(verbose) {
        msg <-
          sprintf("Backup file %s cannot be created because %s cannot be found!",
                  path_backup,
                  path)
        message(msg)
      }
      return(path_backup)
    }

    if (file.exists(path_backup)) {
      msg <-
        sprintf("Backup file %s already exists! Are you sure you want to overwrite it?",
                path_backup)
      stop(msg, call. = FALSE)
    }
    invisible(file.copy(from = path, to = path_backup))
    if (verbose) {
      msg <-
        sprintf("Backed up %s before exporting to %s.", path_backup, path)
      message(msg)
    }
    if(clean) {
      .clean_backup(path = path, ...)
    }
    invisible(path_backup)
  }

.clean_backup <-
  function(path,
           n_keep = 1L,
           decreasing = TRUE,
           ...,
           dir_backup = dirname(path),
           rgx_backup = paste0(tools::file_path_sans_ext(basename(path)),
                               "-.*",
                               tools::file_ext(path)),
           verbose = config$verbose_file) {
    paths_like_backup <-
      list.files(
        path = dir_backup,
        pattern = rgx_backup,
        recursive = FALSE,
        full.names = TRUE
      )
    n <- length(paths_like_backup)
    if (n < n_keep) {
      if (n == 0L) {
        if(verbose) {
          msg <- sprintf("No backup files to delete.")
          message(msg)
        }
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
    paths_to_keep <-
      sort(paths_like_backup, decreasing = decreasing)[1L:n_keep]
    paths_to_delete <- setdiff(paths_like_backup, paths_to_keep)
    invisible(sapply(
      paths_to_delete,
      unlink,
      recursive = TRUE,
      force = TRUE
    ))
    if (verbose) {
      msg <-
        sprintf("Deleted %.0f backup files at %s.",
                length(paths_to_delete),
                Sys.time())
      message(msg)
    }
    invisible(path)
  }

.validate_onerow_df <-
  function(data, ...) {
    stopifnot(is.data.frame(data))
    n_row <- nrow(data)
    if (ifelse(n_row > 1L, TRUE, FALSE)) {
      msg <-
        sprintf("Expected `data` to have only 1 row. Instead, found `%s`.",
                n_row)
      stop(msg, call. = FALSE)
    }
    invisible(data)
  }

.preprocess_compare_n_rows <-
  function(message, warn, stop, ...) {

    n_msg <- sum(c(message, warn, stop), na.rm = TRUE)
    if(n_msg < 1) {
      return(NULL)
    }

    if(n_msg > 1) {
      msg <- paste0("Only one of `message`, `warn`, and `stop` can be set to `TRUE`.")
      stop(msg, call. = FALSE)
    }

    invisible(TRUE)
  }

.compare_n_row_eq <-
  function(data1,
           data2,
           n1 = nrow(data1),
           n2 = nrow(data2),
           nm1 = deparse(substitute(data1)),
           nm2 = deparse(substitute(data2)),
           message = FALSE,
           warn = FALSE,
           stop = !message) {

    .preprocess_compare_n_rows(message, warn, stop)

    if (n1 != n2) {
      msg <-
        sprintf(
          paste0(
            "`%s` and `%s` do not have the same number of rows (%d != %d). ",
            "Something unexpected happened."
          ),
          nm1,
          nm2,
          n1,
          n2
        )
      stop(msg, call. = FALSE)
    }
    invisible(TRUE)
  }

.compare_n_row_le <-
  function(data1,
           data2,
           n1 = nrow(data1),
           n2 = nrow(data2),
           nm1 = deparse(substitute(data1)),
           nm2 = deparse(substitute(nm2)),
           message = FALSE,
           warn = FALSE,
           stop = !message) {

    .preprocess_compare_n_rows(message, warn, stop)

    if(n1 > n2) {
      if(message | warn) {
        msg <-
          sprintf("`%s` has less rows than `%s`  (%d < %d).",
            nm1,
            nm2,
            n1,
            n2
          )
        if(message) {
          message(msg)
        } else if (warn) {
          warning(msg, call. = FALSE)
        }
      } else if (stop) {
        msg <-
          sprintf(
            paste0(
              "`%s` should have more rows than `%s`  (%d < %d). ",
              "Something unexpected happened."),
            nm1,
            nm2,
            n1,
            n2
          )
        stop(msg, call. = FALSE)
      }

    }
    invisible(TRUE)
  }

# TODO: Implement something closer to `print.data.table()`?
# (See https://github.com/Rdatatable/data.table/blob/88439d973f54cb650db0ee2f9d964a363470cd16/R/print.data.table.R.)
headfoot_at <-
  function(.data, col = NULL, n = NULL, ..., na.rm = TRUE) {
    stopifnot(is.data.frame(.data))
    if(!is.null(col)) {
      stopifnot(is.character(col))
    }

    is_grouped <- ifelse(is.null(dplyr::groups(.data)), FALSE, TRUE)
    if (is_grouped) {
      # browser()
      cols_grp_chr <- as.character(dplyr::groups(.data))
      cols_grp_syms <- rlang::syms(cols_grp_chr)
      .data <- dplyr::group_by(.data, !!!cols_grp_syms)
      if(is.null(n)) {
        n <- 3L
      }
    } else {
      cols_grp <- NULL
      if(is.null(n)) {
        n <- 5L
      }
    }

    .n <- n
    if(!is.null(col)) {
      col_sym <- sym(col)
      if(is_grouped) {
        msg <- "Not implemented currently."
        message(msg)
      }
      res <-
        dplyr::filter(.data, (!!col_sym <= .n) | !!col_sym > (max(!!col_sym, na.rm = na.rm) - .n))
    } else {

      if(is_grouped) {
        res <- dplyr::do(.data, dplyr::slice(.data, c((1L:.n), (n() - .n + 1L):n())))
        res <- dplyr::ungroup(res)
      } else {
        .n_row <- nrow(.data)
        res <-
          dplyr::slice(.data, c((1L:.n), ((.n_row - .n + 1L):.n_row)))
      }
    }
    res
  }

# mtcars %>%
#   as_tibble() %>%
#   headfoot_at()
# mtcars %>%
#   as_tibble() %>%
#   group_by(cyl) %>%
#   headfoot_at()
