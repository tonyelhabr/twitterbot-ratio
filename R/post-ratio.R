

ratio_log <- import_ratio_log()
?rtweet::post_tweet
ratio_log
rtweet::post_tweet("dlrow olleh")
ratio_top <-
  ratio_log %>%
  arrange(desc(ratio)) %>%
  slice(1)
img_ts <- rtweet::tweet_shot(ratio_top$status_id)
# path_ts <- file.path("data", "ts.png")
# magick::image_write(img_ts, path_ts)
# msg <-
#   glue::glue(
#     "I've been following some pundits the past couple of days and keeping track of their ",
#     "ratios (i.e. replies / (retweets + favorites). ",
#     "I found that {ratio_top$screen_name} scored the highest with a ratio of {sprintf('%.02f', ratio_top$ratio)}. ",
#     "(reply # = {ratio_top$reply_count}, ",
#     "retweet # = {ratio_top$retweet_count}, ",
#     "favorite # = {ratio_top$favorite_count})."
#   )
msg <-
  glue::glue(
    "Congratulations @{ratio_top$screen_name} on your ratio of {sprintf('%.02f', ratio_top$ratio)}!"
  )
# msg
# nchar(msg)
# rtweet::post_tweet(msg, media = path_ts)
rtweet::post_tweet(msg, retweet_id = ratio_top$status_id)
rtweet::post_tweet(msg, in_reply_to_status_id = ratio_top$status_id)
rtweet::post_favorite(ratio_top$status_id)
