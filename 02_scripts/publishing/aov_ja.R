# source(path(path_scripts_data, "eyetracking.R"))

# 1. prepare data ----------------------------------------------------------
## a) duration ####
## a) duration
df_ja_dur <-
    # filter ROIs: only object_cued and object_uncued
    filter(df_et_dur_bin, fix_id == "object_cued"|fix_id == "object_uncued")

## b) count
df_ja_num <-
  # filter ROIs: only head and body
  filter(df_et_num_bin, fix_id == "object_cued"|fix_id == "object_uncued")

## c) latency ####
df_ja_lat <-
  # filter ROIs: only head and body
  filter(df_et_lat, fix_id == "object_cued"|fix_id == "object_uncued") %>%
  filter(subject_id<94)


# 2. perform anova ----------------------------------------------------------
## a) duration, between factor: group_id, with-in factors fix_id & bin_id
aov_ja_dur <- 
  afex::aov_4(
    prop_dur ~
      group_id * fix_id * bin_id + ( fix_id * bin_id | subject_id ),
    type = "III",
    data = df_ja_dur)

emm_ja_dur <- 
  emmeans(aov_ja_dur, pairwise~fix_id|group_id|bin_id)


## b) count, between factor: group_id, with-in factors fix_id & bin_id
aov_ja_num <- 
  afex::aov_4(
    prop_num ~
      group_id * fix_id * bin_id + ( fix_id * bin_id | subject_id ),
    type = "III", 
    data = df_ja_num)

emm_ja_num <- 
  emmeans(aov_ja_num, pairwise~fix_id|group_id|bin_id)


## c) latency, between factor: group_id, with-in factors fix_id & bin_id
aov_ja_lat <- 
  afex::aov_4(
    m_lat ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III", 
    data = df_ja_lat)

emm_ja_lat <- 
  emmeans(aov_ja_dur, pairwise~fix_id|group_id)



### 3. plots ####
# ## b) count
#     
# pl_ja_num <- 
#     ggplot(
#         df_ja_num %>%
#             group_by(bin_id, fix_id, group_id) %>%
#             summarise(
#                 m_count = mean(prop_num),
#                 sd_count = sd(prop_num)),
#         aes(x=bin_id, y=m_count, color=fix_id)) +
#     geom_line(aes(linetype=group_id)) +
#   # geom_ribbon(aes(
#   #   ymin = m_duration-sd_duration,
#   #   ymax = m_duration+sd_duration,
#   #   fill= fix_id), alpha = 0.05) +
#     geom_point() #+
# # facet_wrap(~group_id)
# 
# pl_ja_dur <- 
#     ggplot(
#         df_ja_dur %>%
#             group_by(bin_id, fix_id, group_id) %>%
#             summarise(
#               m_duration = mean(prop_dur),
#               sd_duration = sd(prop_dur)),
#         aes(x=bin_id, y=m_duration, color=fix_id)) +
#     geom_line(aes(linetype=group_id)) +
#   # geom_ribbon(aes(
#   #   ymin = m_duration-sd_duration,
#   #   ymax = m_duration+sd_duration,
#   #   fill= fix_id), alpha = 0.05) +
#   geom_point() #+
#   # facet_wrap(~group_id)
# 
# # grid.arrange(pl_ja_num, pl_ja_dur)
