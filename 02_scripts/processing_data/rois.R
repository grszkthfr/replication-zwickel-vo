# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")

df_roi <-
  read_delim(
    path(path_prep_save, "rois.csv"),
    delim = ";",
    locale = locale(
      decimal_mark = ",",
      grouping_mark = "."),
    col_types = cols(
      .default = col_double(),
      stimulus = col_character())) %>%
    gather(roi_id, values, background.size:nongaze.max_y, factor_key=TRUE) %>% 
    separate(roi_id, c("roi_id", "statistic"), sep = "\\.") %>% 
    spread(statistic, values) %>%
    select(
        stim_id = stimulus,
        roi_id, roi_size = size,
        roi_m_x = m_x, roi_m_y = m_y, 
        roi_max_x = max_x, roi_min_x = min_x,
        roi_max_y = max_y, roi_min_y = min_y) %>%
    mutate(roi_id =
               case_when(
                   roi_id == "nongaze" ~ "object_uncued",
                   roi_id == "gaze" ~ "object_cued",
                   roi_id != "gaze" ~ roi_id)) %>% 
    arrange(stim_id, roi_id)


