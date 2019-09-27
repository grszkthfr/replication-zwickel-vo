
# 1. prepare data ----------------------------------------------------------
## a) duration ####

df_recall_mem <- df_mem %>% group_by(subject_id, group_id, stim_cued) %>% 
    summarise(sum_recalled = sum(as.numeric(stim_recall)-1))


# 2. perform anova ----------------------------------------------------------
## a) duration, between factor: group_id, with-in factors fix_id & bin_id
aov_mem <- 
    afex::aov_4(
        sum_recalled ~
            group_id * stim_cued + ( stim_cued | subject_id ),
        type = "III",
        data = df_recall_mem)


emm_mem <- 
    emmeans(aov_mem, pairwise~group_id)
