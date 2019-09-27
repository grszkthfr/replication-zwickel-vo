# source(path(path_scripts_data, "eyetracking.R"))

# 1. prepare data -------------------------------------------------------------
## original analysis ----------------------------------------------------------
### a) duration ---------------------------------------------------------------
# df_sa_dur <-
#     # filter ROIs: only head and body
#     filter(df_et_dur, fix_id == "head"|fix_id == "body")
# 
# ### b) count ------------------------------------------------------------------
# df_sa_num <-
#     # filter ROIs: only head and body
#     filter(df_et_num, fix_id == "head"|fix_id == "body")

### c) latency ----------------------------------------------------------------
df_sa_lat <-
    # filter ROIs: only head and body
    filter(df_et_lat, fix_id == "head"|fix_id == "body")

## time course analysis -------------------------------------------------------
### a) duration ---------------------------------------------------------------
df_sa_dur_t <-
    # filter ROIs: only head and body
    filter(df_et_dur_t, fix_id == "head"|fix_id == "body")

### b) count ------------------------------------------------------------------
df_sa_num_t <-
    # filter ROIs: only head and body
    filter(df_et_num_t, fix_id == "head"|fix_id == "body")

# 2. perform ANOVA ------------------------------------------------------------
## original analysis ----------------------------------------------------------
### a) duration, between factor: group_id, with-in factors fix_id -------------
# aov_sa_dur <-
#     aov_4(
#         prop_dur ~
#             group_id * fix_id + ( fix_id | subject_id ),
#         type = "III",
#         data = df_sa_dur)
# 
# emm_sa_dur <-
#     emmeans(aov_sa_dur, pairwise~fix_id|group_id)

### b) count, between factor: group_id, with-in factors fix_id ----------------
# aov_sa_num <-
#   aov_4(
#       prop_num ~
#           group_id * fix_id + ( fix_id | subject_id ),
#       type = "III",
#       data = df_sa_num)
# 
# emm_sa_num <-
#     emmeans(aov_sa_num, pairwise~fix_id|group_id)


### c) latency, between factor: group_id, with-in factors fix_id --------------
aov_sa_lat <-
    aov_4(
        m_lat ~
            group_id * fix_id + ( fix_id | subject_id ),
        type = "III",
        data = df_sa_lat)

emm_sa_lat <-
  emmeans(aov_sa_lat, pairwise~fix_id|group_id)

## time course analysis -------------------------------------------------------
### a) duration, between factor: group_id, with-in factors fix_id & bin_id ----
aov_sa_dur_t <-
  aov_4(
    prop_dur ~
      group_id * fix_id * bin_id + ( fix_id * bin_id | subject_id ),
    type = "III",
    anova_table=list(correction = "GG", es = "ges"),
    data = df_sa_dur_t)

# GG-epsilon
gg_sa_dur_t <- summary(aov_sa_dur_t)$pval.adjustments[, "GG eps"]
names(gg_sa_dur_t) <- str_replace_all(names(gg_sa_dur_t), ":", "_")
gg_sa_dur_t <- as.list(gg_sa_dur_t)

# estimated marginal means
# 3-way interaction
emm_sa_dur_t <-
  emmeans(aov_sa_dur_t, pairwise~fix_id|group_id|bin_id)

### follow up anova ###

#### bin_id: 1 ####

aov_sa_dur_1 <-
  aov_4(
    prop_dur ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_dur_t, bin_id == 1))
aov_sa_num_1 <-
  aov_4(
    prop_num ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_num_t, bin_id == 1))

emm_sa_dur_1_group_id_fix_id <- 
  emmeans(aov_sa_dur_1, pairwise~group_id|fix_id)
emm_sa_num_1_group_id_fix_id <- 
  emmeans(aov_sa_num_1, pairwise~group_id|fix_id)

#### bin_id: 2 ####

aov_sa_dur_2 <-
  aov_4(
    prop_dur ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_dur_t, bin_id ==2))
aov_sa_num_2 <-
  aov_4(
    prop_num ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_num_t, bin_id ==2))

emm_sa_dur_2_group_id_fix_id <- 
  emmeans(aov_sa_dur_2, pairwise~group_id|fix_id)
emm_sa_num_2_group_id_fix_id <- 
  emmeans(aov_sa_num_2, pairwise~group_id|fix_id)

#### bin_id: 3 ####

aov_sa_dur_3 <-
  aov_4(
    prop_dur ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_dur_t, bin_id == 3))
aov_sa_num_3 <-
  aov_4(
    prop_num ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_num_t, bin_id == 3))

emm_sa_dur_3_group_id_fix_id <- 
  emmeans(aov_sa_dur_3, pairwise~group_id|fix_id)
emm_sa_num_3_group_id_fix_id <- 
  emmeans(aov_sa_num_3, pairwise~group_id|fix_id)

#### bin_id: 4 ####

aov_sa_dur_4 <-
  aov_4(
    prop_dur ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_dur_t, bin_id == 4))
aov_sa_num_4 <-
  aov_4(
    prop_num ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_num_t, bin_id == 4))

emm_sa_dur_4_group_id_fix_id <- 
  emmeans(aov_sa_dur_4, pairwise~group_id|fix_id)
emm_sa_num_4_group_id_fix_id <- 
  emmeans(aov_sa_num_4, pairwise~group_id|fix_id)

#### bin_id: 5 ####

aov_sa_dur_5 <-
  aov_4(
    prop_dur ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_dur_t, bin_id == 5))
aov_sa_num_5 <-
  aov_4(
    prop_num ~
      group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = filter(df_sa_num_t, bin_id == 5))

emm_sa_dur_5_group_id_fix_id <- 
  emmeans(aov_sa_dur_5, pairwise~group_id|fix_id)
emm_sa_num_5_group_id_fix_id <- 
  emmeans(aov_sa_num_5, pairwise~group_id|fix_id)

# main effects
emm_sa_dur_t_fix_id <-
  emmeans(aov_sa_dur_t, ~fix_id)
emm_sa_dur_t_group_id <-
  emmeans(aov_sa_dur_t, ~group_id)


### b) count, between factor: group_id, with-in factors fix_id & bin_id -------
aov_sa_num_t <-
  aov_4(
    prop_num ~
      group_id * fix_id * bin_id + ( fix_id * bin_id | subject_id ),
    type = "III",
    anova_table=list(correction = "GG", es = "ges"),
    data = df_sa_num_t)

# GG-epsilon
gg_sa_num_t <- summary(aov_sa_num_t)$pval.adjustments[, "GG eps"]
names(gg_sa_num_t) <- str_replace_all(names(gg_sa_num_t), ":", "_")
gg_sa_num_t <- as.list(gg_sa_num_t)

emm_sa_num_t <-
  emmeans(aov_sa_num_t, pairwise~fix_id|group_id|bin_id)

emm_sa_num_t <-
  emmeans(aov_sa_num_t, pairwise~fix_id|bin_id) 


###############################################################################
# ### plots ####
# pl_sa_num <- 
#     ggplot(
#         df_sa_num_t %>%
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
#         df_sa_dur_t %>%
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
