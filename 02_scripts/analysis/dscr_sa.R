# source(path(path_scripts_data, "eyetracking.R"))

# Descriptives for fixation chacracteristics for joint attention
dscr_sa_dur <-
    df_et_dur_t %>%
    group_by(group_id, fix_id) %>% 
    summarise(m_dur = mean(prop_dur),
              sd_dur = sd(prop_dur),
              se_dur = sd_dur/sqrt(n()),
              l_se_dur = m_dur - se_dur,
              u_se_dur = m_dur + se_dur) %>% 
    filter(fix_id == "head" | fix_id == "body") 

dscr_sa_num <-
    df_et_num_t %>%
    group_by(group_id, fix_id) %>% 
    summarise(m_num = mean(prop_num),
              sd_num = sd(prop_num),
              se_num = sd_num/sqrt(n()),
              l_se_num = m_num - se_num,
              u_se_num = m_num + se_num) %>% 
    filter(fix_id == "head" | fix_id == "body") 

dscr_sa_lat <- 
    df_et_lat %>%
    group_by(group_id, fix_id) %>% 
    summarise(sd_lat = sd(m_lat),
              se_lat = sd_lat/sqrt(n()),
              m_lat = mean(m_lat),
              l_se_lat = m_lat - se_lat,
              u_se_lat = m_lat + se_lat) %>% 
    filter(fix_id == "head" | fix_id == "body") 

# Descriptives for temporal dynamics (bins)
dscr_sa_dur_t <-
    df_et_dur_t %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_dur = mean(prop_dur),
              sd_dur = sd(prop_dur),
              se_dur = sd_dur/sqrt(n()),
              l_se_dur = m_dur - se_dur,
              u_se_dur = m_dur + se_dur) %>% 
    filter(fix_id == "head" | fix_id == "body") 

dscr_sa_num_t <-
    df_et_num_t %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_num = mean(prop_num),
              sd_num = sd(prop_num),
              se_num = sd_num/sqrt(n()),
              l_se_num = m_num - se_num,
              u_se_num = m_num + se_num) %>% 
    filter(fix_id == "head" | fix_id == "body") 


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
