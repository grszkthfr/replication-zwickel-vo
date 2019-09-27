# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")
# source(path(path_scripts_data, "eyetracking.R"))

# 1. prepare data -------------------------------------------------------------

df_mem_glmm <-
    df_mem_num %>%
    mutate(
        # centralised count
        prop_num_z = scale(prop_num),
        # centralised duration
        prop_dur_z = scale(df_mem_dur$prop_dur))

### 2. generalized linear mixed model -----------------------------------------

## a) NULL model ####

# model 1
glmm_mem_basic <-
    glmer(
        stim_recall ~
            group_id + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_glmm)

summary(glmm_mem_basic)

emm_mem_basic <-
    emmeans(glmm_mem_basic, pairwise~group_id)

emmip(glmm_mem_basic, ~group_id, CIs = TRUE)

## b) duration model ####

# model 2
glmm_mem_dur <-
    glmer(
        stim_recall ~
            group_id * prop_dur_z + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_glmm)

summary(glmm_mem_dur)
emm_mem_dur <-
    emmeans(glmm_mem_dur, pairwise~group_id)


# model comparison 1 & 2
aov_mem_glmm_1.2 <-
  anova(glmm_mem_basic, glmm_mem_dur)

## c) duration model ####

# model 3
glmm_mem_num <-
    glmer(
        stim_recall ~
            group_id * prop_num_z + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_glmm)

summary(glmm_mem_num)
emm_mem_num <-
    emmeans(glmm_mem_num, pairwise~group_id|prop_num_z)


# model comparison 1 & 3
aov_mem_glmm_1.3 <-
  anova(glmm_mem_basic, glmm_mem_num)

# model 4
glmm_mem_num_fix <-
    glmer(
        stim_recall ~
            group_id * prop_num_z * fix_id + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_glmm)

summary(glmm_mem_num_fix)
emm_mem_num_fix <-
    emmeans(glmm_mem_num_fix, pairwise~group_id|prop_num_z|fix_id)

# model comparison 3 & 4
aov_mem_glmm_3.4 <-
  anova(glmm_mem_num, glmm_mem_num_fix)
