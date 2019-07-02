
# Descriptives for fixation chacracteristics for social attention

dscr.fixsoc <- df.w.et %>% summarise(mean(fix.face), sd(fix.face),
                                     mean(fix.body), sd(fix.body),
                                     mean(fixn.face), sd(fixn.face),
                                     mean(fixn.body), sd(fixn.body),
                                     mean(fixlat.face), sd(fixlat.face),
                                     mean(fixlat.body), sd(fixlat.body))
dscr.fixsoc.group <- df.w.et %>%
    group_by(group) %>%
    summarise(mean(fix.face), sd(fix.face),
              mean(fix.body), sd(fix.body),
              mean(fixn.face), sd(fixn.face),
              mean(fixn.body), sd(fixn.body),
              mean(fixlat.face), sd(fixlat.face),
              mean(fixlat.body), sd(fixlat.body))
