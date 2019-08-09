source("02_scripts/01-libraries.R")
source("02_scripts/02-paths.R")
source(path(path_scripts_data, "eyetracking.R"))


# Descriptives for scanpath (plotting)
dscr_sp_dur <-
    df_et_dur_bin %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_dur = mean(prop_dur),
              sd_dur = sd(prop_dur),
              se_dur = sd_dur/sqrt(n()),
              l_se_dur = m_dur - se_dur,
              u_se_dur = m_dur + se_dur) %>% 
    filter(fix_id!="background")
    
dscr_sp_num <-
    df_et_num_bin %>%
    group_by(bin_id, group_id, fix_id) %>% 
    summarise(m_num = mean(prop_num),
              sd_num = sd(prop_num),
              se_num = sd_num/sqrt(n()),
              l_se_num = m_num - se_num,
              u_se_num = m_num + se_num) %>% 
    filter(fix_id!="background")
