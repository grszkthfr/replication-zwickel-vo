# source(path(path_scripts_data, "eyetracking.R"))

# 1. prepare data -------------------------------------------------------------
## original analysis ----------------------------------------------------------
### a) duration ---------------------------------------------------------------
## df_ja_dur <-
##     # filter ROIs: only object_cued and object_uncued
##     filter(df_et_dur, fix_id == "object_cued"|fix_id == "object_uncued")

## ### b) count ------------------------------------------------------------------
## df_ja_num <-
##     # filter ROIs: only head and body
## filter(df_et_num, fix_id == "object_cued"|fix_id == "object_uncued")

### c) latency ----------------------------------------------------------------
df_ja_lat <-
    # filter ROIs: only head and body
    filter(df_et_lat, fix_id == "object_cued"|fix_id == "object_uncued")

## time course analysis -------------------------------------------------------
### a) duration ---------------------------------------------------------------
df_ja_dur_t <-
    # filter ROIs: only object_cued and object_uncued
    filter(df_et_dur_t, fix_id == "object_cued"|fix_id == "object_uncued")

### b) count ------------------------------------------------------------------
df_ja_num_t <-
    # filter ROIs: only head and body
    filter(df_et_num_t, fix_id == "object_cued"|fix_id == "object_uncued")

# 2. perform ANOVA ------------------------------------------------------------
## original analysis ----------------------------------------------------------
### a) duration, between factor: group_id, with-in factors fix_id -------------
## aov_ja_dur <-
##     aov_4(
##       # dv
##         prop_dur ~
##           # iv  between           within | error term
##             group_id * fix_id + ( fix_id | subject_id ),
##         type = "III",
##         data = df_ja_dur)

## emm_ja_dur <-
##     emmeans(aov_ja_dur, pairwise~fix_id|group_id)

### b) count, between factor: group_id, with-in factors fix_id ----------------
## aov_ja_num <-
##     aov_4(
##     prop_num ~
##         group_id * fix_id + ( fix_id | subject_id ),
##     type = "III",
##     data = df_ja_num)

## emm_ja_num <-
##     emmeans(aov_ja_num, pairwise~fix_id|group_id)


### c) latency, between factor: group_id, with-in factors fix_id --------------
aov_ja_lat <-
  aov_4(
    m_lat ~
        group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = df_ja_lat)

emm_ja_lat_fix_id <-
    emmeans(aov_ja_lat, ~fix_id)

emm_ja_lat_group_id <-
  emmeans(aov_ja_lat, ~group_id)

## time course analysis -------------------------------------------------------
### a) duration, between factor: group_id, with-in factors fix_id & bin_id ----
# ANOVA
aov_ja_dur_t <-
    aov_4(
        prop_dur ~
            group_id * fix_id * bin_id + ( fix_id * bin_id | subject_id ),
        type = "III",
        anova_table=list(correction = "GG", es = "ges"),
        data = df_ja_dur_t)

# GG correction (epsilon)
gg_ja_dur_t <- summary(aov_ja_dur_t)$pval.adjustments[, "GG eps"]
names(gg_ja_dur_t) <- str_replace_all(names(gg_ja_dur_t), ":", "_")
gg_ja_dur_t <- as.list(gg_ja_dur_t)

# estimated marginal means
# 3-way interaction
emm_ja_dur_t <-
  emmeans(aov_ja_dur_t, pairwise~fix_id|group_id|bin_id)

# 2-way interaction effects

emm_ja_dur_t_fix_id_group_id <-
  emmeans(aov_ja_dur_t, pairwise~fix_id|group_id)
emm_ja_dur_t_fix_id_bin_id <-
  emmeans(aov_ja_dur_t, pairwise~fix_id|bin_id)
emm_ja_dur_t_group_id_bin_id <-
  emmeans(aov_ja_dur_t, pairwise~group_id|bin_id)

# main effects
emm_ja_dur_t_fix_id <-
  emmeans(aov_ja_dur_t, ~fix_id)
emm_ja_dur_t_group_id <-
  emmeans(aov_ja_dur_t, ~group_id)



### b) count, between factor: group_id, with-in factors fix_id & bin_id -------
aov_ja_num_t <-
    aov_4(
      prop_num ~
        group_id * fix_id * bin_id + ( fix_id * bin_id | subject_id ),
      type = "III",
      anova_table=list(correction = "GG", es = "ges"),
      data = df_ja_num_t)

# GG-epsilon
gg_ja_num_t <- summary(aov_ja_num_t)$pval.adjustments[, "GG eps"]
names(gg_ja_num_t) <- str_replace_all(names(gg_ja_num_t), ":", "_")
gg_ja_num_t <- as.list(gg_ja_num_t)

# estimated marginal means
# 3-way interaction
emm_ja_num_t <-
  emmeans(aov_ja_dur_t, pairwise~fix_id|group_id|bin_id)

# 2-way interaction effects

emm_ja_num_t_fix_id_group_id <-
  emmeans(aov_ja_num_t, pairwise~fix_id|group_id)
emm_ja_num_t_fix_id_bin_id <-
  emmeans(aov_ja_num_t, pairwise~fix_id|bin_id)
emm_ja_num_t_group_id_bin_id <-
  emmeans(aov_ja_num_t, pairwise~group_id|bin_id)

# main effects
emm_ja_num_t_fix_id <-
  emmeans(aov_ja_num_t, ~fix_id)

emm_ja_num_t_group_id <-
  emmeans(aov_ja_num_t, ~group_id)

###############################################################################
#### 3. plots
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
