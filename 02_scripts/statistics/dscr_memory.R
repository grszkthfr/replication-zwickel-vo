# Descriptives for memory

dscr.mem <- df.w.mem %>% rename( group = bed) %>%
    group_by(group) %>%
    summarise_at(vars(memgaze:memnogaze), funs(mean,sd,se=sd(.)/sqrt(n()))) %>%
    gather(key, value, memgaze_mean: memnogaze_se, factor_key = T) %>%
    separate(key, c("key", "stat"), sep = "\\_", extra = "merge") %>%
    spread(stat, value) %>%
    mutate(lowerSE = mean - se,
           upperSE = mean + se)
