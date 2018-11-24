
# # TODO: Use these after all functions are "stable"/"finalized" because
# # these are very abstract.
.do_action_ratio_all <-
  function(f_do,
           f_get_user,
           user = NULL,
           method = "since",
           ...,
           path = NULL,
           backup = TRUE,
           clean = TRUE,
           sleep = 0,
           progress = TRUE) {
    if (is.null(user)) {
      user <- f_get_user()
      # user <- user[5:length(user)]
      if(interactive()) {
        # user <- user[1]
        # user <- sample(user, size = 5)
        # user <- "bykevinclark"
        # message(Filtering to a small subset of users. Remove this action!)
      }
    }
    .validate_user_vector(user)
    if (backup & !is.null(path)) {
      .create_backup(path = path, clean = clean)
    }
    if (progress) {
      pb <- .create_pb(total = length(user))
    } else {
      pb <- NULL
    }
    .f <- function(.user, ..., .pb = NULL) {
      f_do(user = .user, method = method, ...)
      # f_do(...)
      Sys.sleep(sleep)
      if (!is.null(.pb)) {
        .pb$tick()
      }
    }
    # browser()
    # purrr::walk(user, ~ .f(.user = .x, ..., .pb = pb))
    purrr::pwalk(list(.user = user), .f, ..., .pb = pb)
  }

# Note: Not using `purrr::partial()` here so that `user` can still be specified.
do_scrape_ratio_all <-
  function(f_do = .do_scrape_ratio,
           f_get_user = get_user_toscrape,
           user = NULL,
           path = config$path_ratio_log_scrape,
           method = "since",
           sleep = 0,
           ...) {
    .do_action_ratio_all(
      f_do = f_do,
      f_get_user = f_get_user,
      user = user,
      path = path,
      method = method,
      ...
    )
  }

do_post_ratio_all <-
  function(f_do = .do_post_ratio,
           f_get_user = get_user_topost,
           user = NULL,
           path = config$path_ratio_log_scrape,
           method = "since",
           sleep = 3,
           ...) {
    .do_action_ratio_all(
      f_do = f_do,
      f_get_user = f_get_user,
      user = user,
      path = path,
      method = method,
      ...
    )
  }
