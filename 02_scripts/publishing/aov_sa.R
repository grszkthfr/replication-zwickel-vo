# source(path(path_scripts_data, "eyetracking.R"))

# 1. prepare data ---------------------------------------------------------
## a) duration
df_sa_dur <-
    # filter ROIs: only head and body
    filter(df_et_dur_bin, fix_id == "head"|fix_id == "body")

## b) count
df_sa_num <-
    # filter ROIs: only head and body
    filter(df_et_num_bin, fix_id == "head"|fix_id == "body")

## c) latency
df_sa_lat <-
    # filter ROIs: only head and body
    filter(df_et_lat, fix_id == "head"|fix_id == "body")


### 2. perform anova ----------------------------------------------------------
## a) duration, between factor: group_id, with-in factors fix_id & bin_id
aov_sa_dur <-
    afex::aov_4(
        prop_dur ~
            group_id * fix_id * bin_id + ( fix_id*bin_id | subject_id ),
        type = "III", 
        data = df_sa_dur)

emm_sa_dur <- 
    emmeans(aov_sa_dur, pairwise~group_id|fix_id|bin_id)


## b) count, between factor: group_id, with-in factors fix_id & bin_id
aov_sa_num <- 
    afex::aov_4(
        prop_num ~
            group_id * fix_id * bin_id + ( fix_id*bin_id | subject_id ),
        type = "III", 
        data = df_sa_num)

emm_sa_num <- 
    emmeans(aov_sa_num, pairwise~group_id|fix_id|bin_id)


## c) latency, between factor: group_id, with-in factors fix_id & bin_id
aov_sa_lat <- 
    afex::aov_4(
        m_lat ~
            group_id * fix_id + ( fix_id | subject_id ),
        type = "III", 
        data = df_sa_lat)

emm_sa_lat <- 
    emmeans(aov_sa_dur, pairwise~fix_id|group_id)



# ### plots ####
# pl_sa_num <- 
#     ggplot(
#         df_sa_num_bin %>%
#             group_by(bin_id, fix_id, group_id) %>%
#             summarise(
#                 m_count = mean(prop_num),
#                 sd_count = sd(prop_num)),
#         aes(x=bin_id, y=m_count, color=fix_id)) +
#     geom_line(aes(linetype=group_id)) +
#     # geom_ribbon(aes(
#     #   ymin = m_duration-sd_duration,
#     #   ymax = m_duration+sd_duration,
#     #   fill= fix_id), alpha = 0.05) +
#     geom_point() #+
# # facet_wrap(~group_id)
# 
# pl_sa_dur <- 
#     ggplot(
#         df_sa_dur_bin %>%
#             group_by(bin_id, fix_id, group_id) %>%
#             summarise(
#                 m_duration = mean(prop_dur),
#                 sd_duration = sd(prop_dur)),
#         aes(x=bin_id, y=m_duration, color=fix_id)) +
#     geom_line(aes(linetype=group_id)) +
#     # geom_ribbon(aes(
#     #   ymin = m_duration-sd_duration,
#     #   ymax = m_duration+sd_duration,
#     #   fill= fix_id), alpha = 0.05) +
#     geom_point() #+
# # facet_wrap(~group_id)
# 
# # grid.arrange(pl_sa_num, pl_sa_dur)
