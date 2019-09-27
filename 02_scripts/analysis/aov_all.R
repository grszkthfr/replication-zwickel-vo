# for supplement material

# source(path(path_scripts_data, "eyetracking.R"))

# 1. prepare data -------------------------------------------------------------
## original analysis ----------------------------------------------------------
### a) duration ---------------------------------------------------------------
## df_all_dur <-
##     # filter ROIs: only object_cued and object_uncued
##     filter(df_et_dur, fix_id == "object_cued"|fix_id == "object_uncued")

## ### b) count ------------------------------------------------------------------
## df_all_num <-
##     # filter ROIs: only head and body
## filter(df_et_num, fix_id == "object_cued"|fix_id == "object_uncued")

### c) latency ----------------------------------------------------------------
df_all_lat <-
    # filter ROIs: only head and body
    filter(df_et_lat, fix_id != "background")

## time course analysis -------------------------------------------------------
### a) duration ---------------------------------------------------------------
df_all_dur <-
    # filter ROIs: only object_cued and object_uncued
    filter(df_et_dur, fix_id != "background")

### b) count ------------------------------------------------------------------
df_all_num <-
    # filter ROIs: only head and body
    filter(df_et_num, fix_id != "background")

# 2. perform ANOVA ------------------------------------------------------------

### c) latency, between factor: group_id, with-in factors fix_id --------------
aov_all_lat <-
  aov_4(
    m_lat ~
        group_id * fix_id + ( fix_id | subject_id ),
    type = "III",
    data = df_all_lat)

emm_all_lat_fix_id <-
  emmeans(aov_all_lat, pairwise~fix_id)

emm_all_lat_group_id <-
  emmeans(aov_all_lat, pairwise~group_id)

## time course analysis -------------------------------------------------------
### a) duration, between factor: group_id, with-in factors fix_id &----
# ANOVA
aov_all_dur <-
    aov_4(
        prop_dur ~
            group_id * fix_id + ( fix_id  | subject_id ),
        type = "III",
        anovaable=list(correction = "GG", es = "ges"),
        data = df_all_dur)

# GG correction (epsilon)
gg_all_dur <- summary(aov_all_dur)$pval.adjustments[, "GG eps"]
names(gg_all_dur) <- str_replace_all(names(gg_all_dur), ":", "_")
gg_all_dur <- as.list(gg_all_dur)

# estimated marginal means
# 3-way interaction
emm_all_dur <-
  emmeans(aov_all_dur, pairwise~fix_id|group_id)

# 2-way interaction effects

emm_all_dur_fix_id_group_id <-
  emmeans(aov_all_dur, pairwise~fix_id|group_id)
emm_all_dur_fix_id <-
  emmeans(aov_all_dur, pairwise~fix_id)
emm_all_dur_group_id_bin_id <-
  emmeans(aov_all_dur, pairwise~group_id)

# main effects
emm_all_dur_fix_id <-
  emmeans(aov_all_dur, pairwise~fix_id)
emm_all_dur_group_id <-
  emmeans(aov_all_dur, pairwise~group_id)



### b) count, between factor: group_id, with-in factors fix_id & bin_id -------
aov_all_num <-
    aov_4(
      prop_num ~
        group_id * fix_id  + ( fix_id  | subject_id ),
      type = "III",
      anovaable=list(correction = "GG", es = "ges"),
      data = df_all_num)

# GG-epsilon
gg_all_num <- summary(aov_all_num)$pval.adjustments[, "GG eps"]
names(gg_all_num) <- str_replace_all(names(gg_all_num), ":", "_")
gg_all_num <- list(gg_all_num)

# estimated marginal means
# 3-way interaction
emm_all_num <-
  emmeans(aov_all_dur, pairwise~fix_id|group_id)

# 2-way interaction effects
emm_all_num_fix_id_group_id <-
  emmeans(aov_all_num, pairwise~fix_id|group_id)
emm_all_num_group_id_bin_id <-
  emmeans(aov_all_num, pairwise~group_id)

# main effects
emm_all_num_fix_id <-
  emmeans(aov_all_num, pairwise~fix_id)

emm_all_num_group_id <-
  emmeans(aov_all_num, pairwise~group_id)

