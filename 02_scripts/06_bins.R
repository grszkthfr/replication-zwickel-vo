
source("02_scripts/01-libraries.R")
source("02_scripts/02-paths.R")
source(path(path_scripts_data, "eyetracking.R"))
source(path(path_scripts_data, "memory.R"))

source(path(path_scripts_fun, "find_bins.R"))

df_bin <- find_bins(df_eyetracking, bin_size = 1000)

df_et_bins <- left_join(
  df_eyetracking %>% select(subject_id, trial_id, fix_n, fix_id) %>% 
    filter(fix_id != "background"),
  df_bin,
  by = c("subject_id", "trial_id", "fix_n")) %>%
  left_join(
    df_memory %>% select(subject_id, condition) %>% unique(),
    by = "subject_id"
  ) %>% 
  group_by(subject_id, trial_id, bin_id) %>% 
  mutate(fix_dur_bin = sum(bin_dur)) %>% 
  group_by(subject_id, trial_id, fix_n, bin_id, fix_id) %>% 
  mutate(
      # fix_reldur_obj = sum(bin_dur)/fix_dur_bin*100,
      fix_reldur_obj = sum(bin_dur)/1000*100) %>% 
  group_by(subject_id, trial_id, bin_id) %>% 
  mutate(
      bin_reldur = sum(fix_reldur_obj)
  )
  

df_et_bins %>%
  group_by(bin_n, fix_id, condition) %>%
  summarise(m_fix_dur = mean(fix_reldur_obj)) %>%
  ggplot(aes(x=bin_n, y=m_fix_dur, color=fix_id)) +
  geom_line()+
  geom_ribbon(aes(
    ymin = m_fix_dur-sd_fix_dur,
    ymax = m_fix_dur+sd_fix_dur,
    fill= fix_id), alpha = 0.05) +
  geom_point()+
  
  facet_wrap(~condition)

3df_et_mem <-
  
  # join condition with et data
  left_join(
    df_bin,
    df_memory %>%
      select(subject_id, trial_id, stim_id, stim_orientation, iti, condition),
    by= c("subject_id", "trial_id") ) %>%
  # join for objects, whether remembered or not
  left_join(df_memory %>%
              select(subject_id, trial_id, cueing, object_id, recalled),
            by= c("subject_id", "trial_id", "fix_id" = "cueing"))


# percentage: innerhalb eines bins / summe aller objekte die fixiert wurden
# bins eher eine oder 2 sekunden
# * bin, HE kein sinn, da summe=100%
# abgleichen abb paper und bins
df_et_mem %>%
  group_by(bin_id, fix_id, condition) %>%
  filter( fix_id != "background", bin_id <= 4) %>%
  summarise(m_fix_dur = mean(bin_dur, na.rm = T)) %>%
  ggplot(aes(x=bin_id, y=m_fix_dur, color=fix_id)) +
  geom_line()+
  geom_point() +
  facet_wrap(~condition)

#################################################################
# # nest each fixation to add rows for each bin the fixation touchs
# df_nested <-
# 
#     df_eyetracking %>%
# 
#     group_by(subject_id, trial_id, fix_n) %>%
# 
#     group_nest(.key = "fixations", keep = TRUE)
# 
# # https://dplyr.tidyverse.org/reference/progress_estimated.html
# pb <- progress_estimated(nrow(df_nested), 0)
# 
# # too slow!
# df_nested$bins <-
# 
#     map(
#         df_nested$fixations,
#         ~{
#             pb$tick()$print()
#           
#             fixation <- .
# 
#             # add rows for each bin (n_bins), starting with second (first is
#             # already present)
#             fixation <- fixation %>%
# 
#                 mutate(
#                     bin_fix_start = findInterval(fix_start, bins),
#                     bin_fix_end = findInterval(fix_end, bins),
#                     # normally: 9999 = bin 20, 10.000 = bin 21, but pretend to 
#                     # end in bin 20 (adds a extra ms to last bin)
#                     bin_fix_end = case_when(
#                         bin_fix_end == length(bins) ~ bin_fix_end - 1L,
#                         bin_fix_end != length(bins) ~ bin_fix_end),
#                     # if 0, fixation is within one bin, if 1 fixation is across
#                     # 2 bins, ...
#                     max_bin_n = bin_fix_end - bin_fix_start + 1,
#                     bin_id = NA)
# 
#             # add row for each bin
#             fixation <- add_row(
#                 fixation,
#                 bin_id = fixation$bin_fix_start:fixation$bin_fix_end) %>%
# 
#                 # fill rows with trial information
#                 fill(subject_id:max_bin_n, .direction = "down") %>%
# 
#                 # remove placeholder bin_id row
#                 drop_na(bin_id)
# 
#             fixation
#             
#         }
#     )
# 
# 
# df_bin <-
# 
#     map_df(df_nested$bins, unnest) %>%
# 
#     mutate_if(is.integer, as.numeric) %>%
# 
#     mutate(
# 
#         # bin_end in ms
#         bin_start = bin_id * bin_size - bin_size,
# 
#         # bin_start in ms
#         bin_end = bin_id * bin_size,
# 
#         bin_id_start = findInterval(bin_start, bins),
# 
#         bin_id_end = findInterval(bin_end, bins),
# 
#         bin_dur = case_when(
# 
#             # fixation duration, when fix ends before or on bin ends and fix
#             # starts after bin starts
#             fix_end < bin_end & fix_start >= bin_start ~ fix_dur,
# 
#             # bin size, when fix ends after or on bin ends and fix starts before
#             # or on bin starts
#             fix_end >= bin_end & fix_start <= bin_start ~ bin_size,
# 
#             # difference, when fix ends before bin ends and fix starts before bin
#             # starts
#             fix_end < bin_end & fix_start <= bin_start ~ fix_end - bin_start,
# 
#             # difference, when fix ends after bin ends and fix starts after bin
#             # starts
#             fix_end >= bin_end & fix_start > bin_start ~ bin_end - fix_start)) %>%
#   
#     # count bins per subject and trial
#     group_by(subject_id, trial_id) %>% 
#     mutate(bin_n = 1:n()) %>%  ungroup() %>% 
#   
#     # when fixation ends on bin start
#     filter( bin_dur != 0)
#################################################################
