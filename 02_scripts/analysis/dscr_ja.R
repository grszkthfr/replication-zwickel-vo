# source(path(path_scripts_data, "eyetracking.R"))

# Descriptives for fixation characteristics for joint attention
dscr_ja_dur <-
    df_et_dur_t %>%
    group_by(group_id, fix_id) %>% 
    summarise(m_dur = mean(prop_dur),
              sd_dur = sd(prop_dur),
              se_dur = sd(prop_dur)/sqrt(n()),
              l_se_dur = m_dur - se_dur,
              u_se_dur = m_dur + se_dur) %>% 
    filter(fix_id == "object_cued" | fix_id == "object_uncued")

dscr_ja_num <-
    df_et_num_t %>%
    group_by(group_id, fix_id) %>% 
    summarise(m_num = mean(prop_num),
              sd_num = sd(prop_num),
              se_num = sd(prop_num)/sqrt(n()),
              l_se_num = m_num - se_num,
              u_se_num = m_num + se_num) %>% 
    filter(fix_id == "object_cued" | fix_id == "object_uncued")

dscr_ja_lat <- 
    df_et_lat %>%
    group_by(group_id, fix_id) %>% 
    summarise(sd_lat = sd(m_lat),
              se_lat = sd(m_lat)/sqrt(n()),
              m_lat = mean(m_lat),
              l_se_lat = m_lat - se_lat,
              u_se_lat = m_lat + se_lat) %>% 
    filter(fix_id == "object_cued" | fix_id == "object_uncued") %>% 
    select(group_id, fix_id, m_lat, sd_lat, se_lat, l_se_lat, u_se_lat )

# Descriptives for temporal dynamics (bins)
dscr_ja_dur_t <-
    df_et_dur_t %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_dur = mean(prop_dur),
              sd_dur = sd(prop_dur),
              se_dur = sd_dur/sqrt(n()),
              l_se_dur = m_dur - se_dur,
              u_se_dur = m_dur + se_dur) %>% 
    filter(fix_id=="object_cued"| fix_id == "object_uncued")

dscr_ja_num_t <-
    df_et_num_t %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_num = mean(prop_num),
              sd_num = sd(prop_num),
              se_num = sd_num/sqrt(n()),
              l_se_num = m_num - se_num,
              u_se_num = m_num + se_num) %>% 
    filter(fix_id=="object_cued"| fix_id == "object_uncued")


# dscr.fix.rpl <-
#     df.w.et %>%
#     group_by(group) %>%
#     summarise_at(vars(fix.face:fixlat.nongaze),
#                  funs(mean,sd,se=sd(.)/sqrt(n()))) %>% # mit den funs() die variablen vars(von:bis) berechnen
#     gather(key, measure, fix.face_mean:fixlat.nongaze_se) %>% # ins longformat
#     mutate(
#         fix = as.factor(map(strsplit(key,"[[:punct:]]"), ~.x[1]) %>%
#                             unlist()),
#         gazed = map(strsplit(key,"[[:punct:]]"), ~.x[2]) %>%
#             unlist(),
#         gazed = as.factor(substring(gazed,1)),
#         stat = as.factor(
#             map(strsplit(key,"[[:punct:]]"), ~.x[3]) %>%
#                 unlist()),
#         key = as.factor("replication"),
#         measure = ifelse(as.character(fix) != "fixlat", round(measure, 3),
#                          round(measure, 0))) #%>%
# select(-measure, everything()) %>% # neusortieren der variablen
#     select(key, everything())
# 
# dscr.fix.rpl$gazed <- factor(dscr.fix.rpl$gazed, levels=c("face", "body",
#                                                           "gaze", "nongaze"))
# 
# dscr.fix.zwckl <-
#     data.frame(
#         group="free",
#         fix=c(rep("fix", 4),rep("fixn", 4),rep("fixlat", 4)),
#         gazed=c("gaze","gaze", "nongaze", "nongaze"),
#         stat=c("mean","se"),
#         measure=c(0.08,.01,.07,.01,5.89,.37,5.24,.34,3588, 133,4008,166)) # free = person
