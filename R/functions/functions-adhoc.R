
.add_frac_cols <-
  function(data, ..., na.rm = TRUE) {
    cols_enquos <- enquos(...)
    data %>%
      mutate_at(vars(!!!cols_enquos), funs(. / sum(., na.rm = na.rm)))
  }
# .count_frac <-
#   function(data, ..., sort = TRUE, na.rm = TRUE) {
#     cols_enquo <- enquos(...)
#     data %>%
#       count(!!!cols_enquo, sort = sort) %>%
#       mutate_at(vars(n), funs(. / sum(., na.rm = na.rm)))
#   }
