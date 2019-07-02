# Descriptives for fixation chacracteristics for joint attention

dscr.fix.rpl <- df.w.et %>%
    group_by(group) %>%
    summarise_at(vars(fix.face:fixlat.nongaze),
                 funs(mean,sd,se=sd(.)/sqrt(n()))) %>% # mit den funs() die variablen vars(von:bis) berechnen
    gather(key, measure, fix.face_mean:fixlat.nongaze_se) %>% # ins longformat
    mutate(fix = as.factor(map(strsplit(key,"[[:punct:]]"), ~.x[1]) %>%
                               unlist()),
           gazed = map(strsplit(key,"[[:punct:]]"), ~.x[2]) %>%
               unlist(),
           gazed = as.factor(substring(gazed,1)),
           stat = as.factor(map(strsplit(key,"[[:punct:]]"), ~.x[3]) %>%
                                unlist()),
           key = as.factor("replication"),
           measure = ifelse(as.character(fix) != "fixlat", round(measure, 3),
                            round(measure, 0))) %>%
    select(-measure, everything()) %>% # neusortieren der variablen
    select(key, everything())

dscr.fix.rpl$gazed <- factor(dscr.fix.rpl$gazed, levels=c("face", "body",
                                                          "gaze", "nongaze"))

dscr.fix.zwckl <- data.frame(group="free",fix=c("fix","fix","fix","fix","fixn",
                                                "fixn","fixn","fixn","fixlat",
                                                "fixlat","fixlat","fixlat"),
                             gazed=c("gaze","gaze", "nongaze", "nongaze"),
                             stat=c("mean","se"),
                             measure=c(0.08,.01,.07,.01,5.89,.37,5.24,.34,3588,
                                       133,4008,166)) # free = person
