

ratio_log <- import_ratio_log()
ratio_log <- ratio_log %>% .reorder_ratio_cols_at()
ratio_log %>%
  filter(created_at <= (.TIME - lubridate::hours(24))) %>% 
  .slice_ratio_df_at() %>% 
  export_ratio_last_scrape()

ratio_log %>%
  distinctify_data_at()
