# table objects

t1 <- dscr.fix.rpl %>%
    filter(group == "free", gazed == "gaze") %>%
    select(fix, stat, measure) %>%
    mutate(measure = ifelse(fix != "fixlat", measure*100, measure),
           Measurements = ifelse(fix == "fixlat", "Fixation latency\n(in ms)",
                                 ifelse(fix == "fix", "Fixation duration (in %)",
                                        ifelse( fix == "fixn", "Fixation number (in %)",NA)))) %>%
    spread(key = "stat", value = "measure") %>%
    select(Measurements:se)

t2 <- dscr.fix.rpl %>%
    filter(group == "free", gazed == "nongaze") %>%
    select(fix, stat, measure) %>%
    mutate(measure = ifelse(fix != "fixlat", measure*100, measure)) %>%
    spread(key = "stat", value = "measure")

t3 <- dscr.fix.rpl %>%
    filter(group == "mem", gazed == "gaze") %>%
    select(fix, stat, measure) %>%
    mutate(measure = ifelse(fix != "fixlat", measure*100, measure)) %>%
    spread(key = "stat", value = "measure")

t4 <- dscr.fix.rpl %>%
    filter(group == "mem", gazed == "nongaze") %>%
    select(fix, stat, measure) %>%
    mutate(measure = ifelse(fix != "fixlat", measure*100, measure)) %>%
    spread(key = "stat", value = "measure")

# apa_table(t5,
#           align = c("l", rep("c", 12)),
#           caption = "Mean Fixation Latency (in Milliseconds), relative fixation number (FF), and fixation duration as a function of Group (explicit encoding, free viewing) and object (cued, not gazed at",
#           note = "here is space for a note.",
#           #added_stub_head = "Variables",
#           #col_spanners = list(`free viewing`= c(2,7), `explicit encoding`= c(8,13)),
#           landscape = TRUE
#           )
