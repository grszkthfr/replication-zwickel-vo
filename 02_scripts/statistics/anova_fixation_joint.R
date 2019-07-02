source("02_scripts/01-libraries.R")
source("02_scripts/02-paths.R")

source(path(path_scripts_data, "eyetracking.R"))
library(afex)


# 1. prepare data ---------------------------------------------------------
## a) duration ####
# data frame with a column for 
df_ja_dur <-
    # filter ROIs: only head and body
    filter(df_et_dur, fix_id == "head"|fix_id == "body")


df_ja_dur_bin <-
    # filter ROIs: only head and body
    filter(df_et_dur_bin, fix_id == "head"|fix_id == "body")


## b) count ####
# data frame with a column for 
df_ja_cnt <-
    # filter ROIs: only head and body
    filter(df_et_cnt, fix_id == "head"|fix_id == "body")


df_ja_cnt_bin <-
    # filter ROIs: only head and body
    filter(df_et_cnt_bin, fix_id == "head"|fix_id == "body")



## c) latency ####

df_ja_lat <-
    # filter ROIs: only head and body
    filter(df_et_lat, fix_id == "head"|fix_id == "body")


### 2. perform anova ----------------------------------------------------------

## a) duration ####

aov_ja_dur <- 
    aov_4(
        formula = prop_dur ~ condition_id * fix_id + ( fix_id | subject_id ),
        type = "III", 
        data = df_ja_dur)$anova %>%
    round(3)

# aov_ez - syntax ####
# aov_ja_fix_dur <- 
#     aov_ez(
#         id = "subject_id",
#         dv = "prop_dur",
#         within = "fix_id",
#         between = "condition_id",
#         data = df_ja_fix_dur)$anova %>% round(3)

aov_ja_dur_bin <- 
    aov_4(
        prop_dur ~
            condition_id * fix_id * bin_id + ( fix_id*bin_id | subject_id ),
        type = "III", 
        data = df_ja_dur_bin)$anova %>%
    round(3)

## b) count ####

aov_ja_cnt <- 
    aov_4(
        prop_cnt ~
            condition_id * fix_id + ( fix_id | subject_id ),
        type = "III", 
        data = df_ja_cnt)$anova %>%
    round(3)

aov_ja_cnt_bin <- 
    aov_4(
        prop_cnt ~
            condition_id * fix_id * bin_id + ( fix_id*bin_id | subject_id ),
        type = "III", 
        data = df_ja_cnt_bin)$anova %>%
    round(3)

## c) latency ####

aov_ja_lat <- 
    aov_4(
        formula = mean_latency ~ condition_id * fix_id + ( fix_id | subject_id ),
        type = "III", 
        data = df_ja_lat)$anova %>%
    round(3)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# df_dur_matthias <-
#   df.w.et %>%
#   select(vp, group, fix.face,fix.body) %>%
#   gather(
#     key = "object_id",
#     value = "duration",
#     fix.face:fix.body) %>%
#   arrange(vp, object_id)
# 
# aov_4(duration ~ group*object_id + (object_id|vp), type = "III",
#                  data = df_dur_matthias)
# 
# # ANOVA Fixations characteristics for joint attention
# ## 2 (Group) x 2 (Gaze) ANOVA
# 
# icond <- gl(2,1,labels=c("cued","uncued")) # within-factor
# idata <- data.frame(icond)
# 
# for (st in seq(7,16,4)) { # Variables 7:16, every 4th: fix.gaze and fix.nogaze; fixn.gaze and ...
#     carmod <- lm(as.matrix(df.w.et[,st:(st+1)]) ~ df.w.et$group)
#     #print(colnames(df.w.et[,st:(st+1)]))
#     #print(Anova(carmod, idata=idata, idesign=~icond, type="III"))
#     assign(paste0("anova.",colnames(df.w.et)[st]),
#            Anova(carmod, idata=idata, idesign=~icond, type="III"))
#     assign(paste0("apa.anova.",colnames(df.w.et)[st]),
#            apa_print(Anova(carmod, idata=idata, idesign=~icond, type="III"),
#                      correction="GG", mse = FALSE))
# }
# 
# rm(carmod, idata, icond, st)
