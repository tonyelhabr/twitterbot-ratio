

ratio_log_scrape <- import_ratio_log_scrape()
ratio_log_scrape <- ratio_log_scrape %>% .reorder_ratio_cols_at()
ratio_log_scrape %>%
  filter(created_at <= (.TIME - lubridate::hours(24))) %>% 
  .slice_ratio_df_at() %>% 
  export_ratio_last_scrape()

ratio_log_scrape %>%
  distinctify_data_at()
