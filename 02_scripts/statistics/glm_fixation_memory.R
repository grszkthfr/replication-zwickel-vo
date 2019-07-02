source("02_scripts/01-libraries.R")
source("02_scripts/02-paths.R")

source(path(path_scripts_data, "eyetracking.R"))

library(lme4)
library(lmerTest)

# 1. prepare data -------------------------------------------------------------
## a) duration ####
# data frame with a column for

# centralised for GLMM to better perform
df_mem_dur <-
    df_mem_dur %>% 
    mutate(z_prop_dur = scale(prop_dur))


## b) count ####
# data frame with a column for
df_mem_cnt <-
    df_mem_cnt %>% 
    mutate(z_prop_cnt = scale(prop_cnt))


### 2. generalized linear mixed model -----------------------------------------

## a) duration #### 
 
# model 1
m1_dur <- 
    glmer(
        stim_recall ~
            condition_id + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_dur)

summary(m1_dur)

# model 2
m2_dur <- 
    glmer(
        stim_recall ~
            condition_id * z_prop_dur + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_dur)

summary(m2_dur)

# model comparison 1 & 2
anova(m1_dur, m2_dur)

## b) count ####

# model 1
m1_cnt <- 
    glmer(
        stim_recall ~
            condition_id + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_cnt)

summary(m1_cnt)

# model 2
m2_cnt <- 
    glmer(
        stim_recall ~
            condition_id * z_prop_cnt + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_cnt)

summary(m2_cnt)

# model comparison 1 & 2
anova(m1_cnt, m2_cnt)

# model 3
m3_cnt <- 
    glmer(
        stim_recall ~
            condition_id * z_prop_cnt * fix_id + (1|subject_id) + (1|stim_id),
        family = "binomial",
        data = df_mem_cnt)

summary(m3_cnt)

# model comparison 2 & 3
anova(m2_cnt, m3_cnt)
