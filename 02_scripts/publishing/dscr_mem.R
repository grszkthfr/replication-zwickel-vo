# source(path(path_scripts_data, "memory.R"))

# Descriptives for memory

dscr_mem <-
    df_mem %>%
    group_by(subject_id, group_id, stim_cued) %>%
    summarise(recalled = sum(as.numeric(stim_recall)-1)/n()*26) %>%
    group_by(group_id, stim_cued) %>%
    summarise(m_recall = mean(recalled),
              sd_recall = sd(recalled),
              se_recall = sd(recalled)/sqrt(n()),
              l_se_recall = m_recall - se_recall,
              u_se_recall = m_recall + se_recall)

# dscr.mem <- df.w.mem %>% rename( group = bed) %>%
#     group_by(group) %>%
#     summarise_at(vars(memgaze:memnogaze), funs(mean,sd,se=sd(.)/sqrt(n()))) %>%
#     gather(key, value, memgaze_mean: memnogaze_se, factor_key = T) %>%
#     separate(key, c("key", "stat"), sep = "\\_", extra = "merge") %>%
#     spread(stat, value) %>%
#     mutate(lowerSE = mean - se,
#            upperSE = mean + se)
