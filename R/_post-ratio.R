
pre_auto()

f_possibly <-
  purrr::possibly(do_post_ratio_all, otherwise = message("Encountered an error..."))
# do_post_ratio_all()
f_possibly()

post_auto()

