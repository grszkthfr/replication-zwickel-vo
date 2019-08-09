# source(path(path_scripts_data, "eyetracking.R"))

# # Descriptives for fixation chacracteristics
# dscr_et_dur <-
#     df_et_dur %>%
#     group_by(group_id, fix_id) %>% 
#     summarise(m_dur = mean(prop_dur),
#               sd_dur = sd(prop_dur),
#               se_dur = sd_dur/sqrt(n()),
#               l_se_dur = m_dur - se_dur,
#               u_se_dur = m_dur + se_dur) 
# 
# dscr_et_num <-
#     df_et_num %>%
#     group_by(group_id, fix_id) %>% 
#     summarise(m_num = mean(prop_num),
#               sd_num = sd(prop_num),
#               se_num = sd_num/sqrt(n()),
#               l_se_num = m_num - se_num,
#               u_se_num = m_num + se_num)
# 
# dscr_et_lat <- 
#     df_et_lat %>%
#     group_by(group_id, fix_id) %>% 
#     summarise(sd_lat = sd(m_lat),
#               se_lat = sd_lat/sqrt(n()),
#               m_lat = mean(m_lat),
#               l_se_lat = m_lat - se_lat,
#               u_se_lat = m_lat + se_lat)

# Descriptives for temporal dynamics (bins)
dscr_et_dur_bin <-
    df_et_dur_bin %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_dur = mean(prop_dur),
              sd_dur = sd(prop_dur),
              se_dur = sd_dur/sqrt(n()),
              l_se_dur = m_dur - se_dur,
              u_se_dur = m_dur + se_dur)

dscr_et_num_bin <-
    df_et_num_bin %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_num = mean(prop_num),
              sd_num = sd(prop_num),
              se_num = sd_num/sqrt(n()),
              l_se_num = m_num - se_num,
              u_se_num = m_num + se_num)


# dscr.fixsoc <- df.w.et %>% summarise(mean(fix.face), sd(fix.face),
#                                      mean(fix.body), sd(fix.body),
#                                      mean(fixn.face), sd(fixn.face),
#                                      mean(fixn.body), sd(fixn.body),
#                                      mean(fixlat.face), sd(fixlat.face),
#                                      mean(fixlat.body), sd(fixlat.body))
# 
# dscr.fixsoc.group <- df.w.et %>%
#     group_by(group) %>%
#     summarise(mean(fix.face), sd(fix.face),
#               mean(fix.body), sd(fix.body),
#               mean(fixn.face), sd(fixn.face),
#               mean(fixn.body), sd(fixn.body),
#               mean(fixlat.face), sd(fixlat.face),
#               mean(fixlat.body), sd(fixlat.body))
