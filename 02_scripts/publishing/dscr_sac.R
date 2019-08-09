# source(path(path_scripts_data, "eyetracking.R"))

# Descriptives for saccades
dscr.sac.rpl <- df.w.et %>%
    group_by(group) %>%
    summarise_at(vars(sac.pgaze:sac.bnongaze), funs(mean,sd,se=sd(.)/sqrt(n()))) %>% # mit den funs() die variablen vars(von:bis) berechnen
    gather(key, measure, sac.pgaze_mean:sac.bnongaze_se) %>% # ins longformat
    mutate(stat = as.factor(map(strsplit(key,"[[:punct:]]"), ~.x[3]) %>% unlist()),
           area = map(strsplit(key,"[[:punct:]]"), ~.x[2]) %>% unlist(),
           area = as.factor(substring(area,1,1)),
           gazed = map(strsplit(key,"[[:punct:]]"), ~.x[2]) %>% unlist(),
           gazed = as.factor(substring(gazed,2)),
           key = as.factor("replication")) %>% #rausnehmen, zur kontrolle, ob alles passt, was oben läuft.
    select(-measure, everything()) %>% # neusortieren der variablen
    select(key, everything())

dscr_sac <-
    df.w.et %>% select(vp, group, sac.fgaze, sac.fnongaze) %>%
    gather(key= "fix_id", value = "sac", sac.fgaze:sac.fnongaze) %>% 
    mutate(fix_id = case_when(fix_id == "sac.fgaze" ~ "object_cued",
                              fix_id == "sac.fnongaze" ~ "object_uncued")) %>% 
    group_by(group_id = group, fix_id) %>%
    summarise(m_sac = mean(sac),
              sd_sac = sd(sac),
              se_sac = sd_sac/sqrt(n()),
              l_se_sac = m_sac - se_sac,
              u_se_sac = m_sac + se_sac)

# dscr.sac.zwckl <- data.frame(key=c("zwickel","zwickel"),group=c("free", "free"),
#                              gazed=c("gaze", "gaze", "nongaze", "nongaze"),
#                              stat=c("mean", "se"),measure=c(.14,0.01,.09,0.01)) # free = person
