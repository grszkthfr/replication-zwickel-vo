# table head

t6 <- dscr.fix.rpl %>%
    filter(group == "free", gazed == "face") %>%
    select(fix, stat, measure) %>%
    mutate(measure = ifelse(fix != "fixlat", measure*100, measure),
           Measurements = ifelse(fix == "fixlat", "Fixation latency\n(in ms)",
                                 ifelse(fix == "fix", "Fixation duration (in %)",
                                        ifelse( fix == "fixn", "Fixation number (in %)",NA)))) %>%
    spread(key = "stat", value = "measure") %>%
    select(Measurements:se)

t7 <- dscr.fix.rpl %>%
    filter(group == "free", gazed == "body") %>%
    select(fix, stat, measure) %>%
    mutate(measure = ifelse(fix != "fixlat", measure*100, measure)) %>%
    spread(key = "stat", value = "measure")

t8 <- dscr.fix.rpl %>%
    filter(group == "mem", gazed == "face") %>%
    select(fix, stat, measure) %>%
    mutate(measure = ifelse(fix != "fixlat", measure*100, measure)) %>%
    spread(key = "stat", value = "measure")

t9 <- dscr.fix.rpl %>%
    filter(group == "mem", gazed == "body") %>%
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
