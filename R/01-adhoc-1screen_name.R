

ratio_log_import <- import_ratio_log()
.SCREEN_NAME <- "stephenasmith"
.TIME
.N_HOUR_LAG
ratio_log_filt <-
  ratio_log_import %>%
  filter(created_at <= (.TIME - lubridate::hours(.N_HOUR_LAG))) %>% 
  filter(screen_name %in% .SCREEN_NAME) %>% 
  .slice_ratio_df_at()
ratio_log_filt

..validate_tl <-
  function(data, ...) {
    stopifnot(is.data.frame(data))
    stopifnot(all(c("screen_name", "status_id") %in% names(data)))
  }

.do_get_timeline <-
  function(data, ...) {
    data_cnt <-
      data %>%
      # count(screen_name, status_id) %>% 
      group_by(screen_name, status_id) %>% 
      summarise(.n = n()) %>% 
      ungroup() %>% 
      filter(n > 1L)
    cnd <- ifelse(nrow(data_cnt) > 0L, TRUE, FALSE)
    if(cnd) {
      
    }
  }

tl_raw <-
  # ratio_last_import %>%
  ratio_log_filt %>% 
  select(screen_name, status_id) %>% 
  mutate(
    data = 
      purrr::pmap(
        list(screen_name, status_id), 
        ~.get_timeline_verbosely_possibly(user = ..1, since_id = ..2)
      )
  )

tl <-
  tl_raw %>% 
  select(data) %>% 
  unnest(data)
