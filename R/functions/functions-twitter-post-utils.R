

# Reference: rtweet::emojis %>% filter(description %>% str_detect("^fire$"))
.get_chr_emoji_fire <-
  function() {
    # "\U0001f525"
    ""
  }

# Reference: "^fearful face$"
.get_chr_emoji_fear <-
  function() {
    # "\U0001f628"
    ""
  }

# Reference: "^grinning face$"
.get_chr_emoji_smile <-
  function() {
    # "\U0001f600"
    ""
  }

# Reference: "^thumbs up$"
.get_chr_emoji_thumbsup <-
  function() {
    # "\U0001f44d"
    ""
  }

.prettify_timestamp_post <-
  function(x) {
    # stopifnot(lubridate::is.timepoint(x))
    strftime(x, "%I:%M %p, %m/%d/%Y")
  }

.slice_ratio_max <-
  function(data, ...) {
    res <-
      data %>%
      filter(ratio == max(ratio, na.rm = TRUE)) %>%
      slice(1)

    .validate_onerow_df(res)
    res
  }

.filter_ratio_log_scrape_posted <-
  function(data, .user, .status_id, ...) {
    data %>%
      filter(user == .user) %>%
      filter(status_id != .status_id) %>%
      filter(posted == 1L)
  }

# Note: This verbose function is only useful to avoid conflicts/renaming of `user`
# in the `.do_post_ratio()` function.
.filter_ratio_log_scrape_byuser <-
  function(data, .user, ...) {
    data %>%
      filter(user == .user)
  }

.filter_ratio_log_scrape_byconfig <-
  function(data, ...) {
    data_filt0 <-
      data %>%
      filter(considered == 0L) %>%
      filter(posted == 0L)

    data_alt <- head(data, 0)

    data_filt1 <-
      data_filt0 %>%
      arrange(desc(created_at))

    if(n_row_filt < config$post_hour_since_min) {

    }

    n_row_filt <- nrow(data_filt0)
    if(n_row_filt < config$post_n_since_min) {
      return(data_alt )
    }
