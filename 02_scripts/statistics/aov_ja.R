source("02_scripts/01-libraries.R")
source("02_scripts/02-paths.R")

source(path(path_scripts_data, "eyetracking.R"))
# library(afex)


# 1. prepare data ---------------------------------------------------------
## a) duration ####
df_ja_dur_bin <-
    # filter ROIs: only head and body
    filter(df_et_dur_bin, fix_id == "object_cued"|fix_id == "object_uncued")


## b) count ####
df_ja_cnt_bin <-
  # filter ROIs: only head and body
  filter(df_et_cnt_bin, fix_id == "object_cued"|fix_id == "object_uncued")



## c) latency ####
df_ja_lat <-
  # filter ROIs: only head and body
  filter(df_et_lat, fix_id == "object_cued"|fix_id == "object_uncued")

  
### 2. perform anova ----------------------------------------------------------
  
## a) duration ####
aov_ja_dur_bin <- 
    afex::aov_4(
        formula = prop_dur ~ condition_id * fix_id * bin_id + ( fix_id*bin_id | subject_id ),
        type = "III",
        data = df_ja_dur_bin)$anova %>%
    round(3)

## b) count ####
aov_ja_cnt_bin <- 
    afex::aov_4(
        formula = prop_cnt ~ condition_id * fix_id * bin_id + ( fix_id*bin_id | subject_id ),
        type = "III", 
        data = df_ja_cnt_bin)$anova %>%
    round(3)

## c) latency ####
aov_ja_lat <- 
    afex::aov_4(
        formula = mean_latency ~ condition_id * fix_id + ( fix_id | subject_id ),
        type = "III", 
        data = df_ja_lat)$anova %>%
    round(3)

### plots ####

pl_ja_cnt <- 
    ggplot(
        df_ja_cnt_bin %>%
            group_by(bin_id, fix_id, condition_id) %>%
            summarise(
                m_count = mean(prop_cnt),
                sd_count = sd(prop_cnt)),
        aes(x=bin_id, y=m_count, color=fix_id)) +
    geom_line(aes(linetype=condition_id)) +
  # geom_ribbon(aes(
  #   ymin = m_duration-sd_duration,
  #   ymax = m_duration+sd_duration,
  #   fill= fix_id), alpha = 0.05) +
    geom_point() #+
# facet_wrap(~condition_id)

pl_ja_dur <- 
    ggplot(
        df_ja_dur_bin %>%
            group_by(bin_id, fix_id, condition_id) %>%
            summarise(
              m_duration = mean(prop_dur),
              sd_duration = sd(prop_dur)),
        aes(x=bin_id, y=m_duration, color=fix_id)) +
    geom_line(aes(linetype=condition_id)) +
  # geom_ribbon(aes(
  #   ymin = m_duration-sd_duration,
  #   ymax = m_duration+sd_duration,
  #   fill= fix_id), alpha = 0.05) +
  geom_point() #+
  # facet_wrap(~condition_id)

grid.arrange(pl_ja_cnt, pl_ja_dur)



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# df_dur_matthias <-
#   df.w.et %>%
#   select(vp, group, fix.face,fix.body) %>%
#   gather(
#       key = "object_id",
#       value = "duration",
#       fix.face:fix.body) %>%
#   arrange(vp, object_id)
# 
# aov_4(duration ~ group*object_id + (object_id|vp),
#       type = "III",
#       data = df_dur_matthias)
# 
# ANOVA Fixations characteristics
## 2 (Group) x 2 (Face/Body) ANOVA
# 
# icond <- gl(2,1,labels=c("head","body")) # within-factor
# idata <- data.frame(icond)
# 
# for (st in seq(5,16,4)) { # Variables 5:16, every 4th: fix.face and fix.noface; fixn.face and ...
#     carmod <- lm(as.matrix(df.w.et[,st:(st+1)]) ~ df.w.et$group)
#     #print(Anova(carmod, idata=idata, idesign=~icond, type="III"))
#     assign(paste0("anova.",colnames(df.w.et)[st]),
#            Anova(carmod, idata=idata, idesign=~icond, type="III"))
#     assign(paste0("apa.anova.",colnames(df.w.et)[st]),
#            apa_print(Anova(carmod, idata=idata, idesign=~icond, type="III"),
#                      correction="GG", mse = FALSE))
# }
# 
# rm(carmod, idata, icond, st)


