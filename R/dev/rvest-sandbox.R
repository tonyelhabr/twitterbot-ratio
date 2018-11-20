


user <- "Foxworth24"
status_id <- "1063469402870419456"
url <- sprintf("https://twitter.com/%s/status/%s", user, status_id)
selector <- sprintf("#profile-tweet-action-reply-count-aria-%s", status_id)
reply_count <-
  url %>%
  xml2::read_html() %>%
  rvest::html_nodes(selector) %>%
  rvest::html_text() %>%
  str_replace_all("[^[0-9]]", "") %>%
  as.integer()
reply_count %>% str_replace_all("[^([0-9]|[,]|[.])]", "")
" abc  3,600 abc" %>% str_replace_all("[^[0-9]]", "") %>% as.integer()


url <- "https://twitter.com/drob/status/1063549587925991424"

# req <- httr::GET(url)
# req
# cont <- req %>% httr::content()
# cont
selector <- "#profile-tweet-action-reply-count-aria-1063549587925991424"
html %>%
  rvest::html_nodes(selector) %>%
  rvest::html_text()

download.file(url = url, destfile = "R/rvest-sandbox.html")
html <-
  url %>%
  xml2::read_html()
html


reply_count <-
  html %>%
  # pluck(1) %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  magrittr::extract(62) %>% # div class="permalink-inner permalink-tweet-container"
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  magrittr::extract(7) %>% # div class="ProfileTweet-actionList js-actions" role="group" aria-label="Tweet actions"
  rvest::html_children() %>%
  magrittr::extract(1) %>% # div class="ProfileTweet-action ProfileTweet-action--reply"
  rvest::html_children() %>%
  rvest::html_children() %>%
  rvest::html_children() %>%
  magrittr::extract(3) %>% # span class="ProfileTweet-actionCountForPresentation" aria-hidden="true"
  rvest::html_text()

html %>%
  # pluck(1) %>%
  rvest::html_children() %>%
  rvest::html_nodes(xpath = '//*[@id="permalink-overlay-dialog"]/div[3]/div/div/div[1]/div[1]/div/div[4]/div[2]/div[1]/button/span/span')
