# source(path(path_scripts_publ, "dscr_et.R"))
# source(path(path_scripts_publ, "plt_cosmetics.R"))

plt_sa_dur <- 
    dscr_sa_dur %>% 
    ggplot(
        aes(
            x=group_id,
            y = m_dur,
            fill = fix_id)) +
    geom_col(position = "dodge") +
    geom_errorbar(
        aes(
            ymin=l_se_dur,
            ymax=u_se_dur),
        width = .2,
        position=position_dodge(.9)) +
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsHeads, expand = c(0,0),
                       breaks = c(.05, .10, .15, .20, .25)) +
    scale_x_discrete(
        name = element_blank(),
        labels=cLabels) +
    scale_fill_manual(
        name = "Persons",
        labels = c("Head", "Body"),
        values=cColorsHB) +
    labs(title = "Object fixations")

cLegendHead <- get_legend(plt_sa_dur)

plt_sa_dur <- plt_sa_dur +
    cTheme

plt_sa_dur <- plt_sa_dur +
    labs(title = "Fixation duration\n(in %)") +
    cTheme


plt_sa_num <- 
    dscr_sa_num %>% 
    ggplot(
        aes(
            x=group_id,
            y = m_num,
            fill = fix_id)) +
    geom_col(position = "dodge") +
    geom_errorbar(
        aes(
            ymin=l_se_num,
            ymax=u_se_num),
        width = .2,
        position=position_dodge(.9)) +    ## Significanes from duration!!!
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsHeads, expand = c(0,0),
                       breaks = c(.05, .10, .15, .20, .25)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsHB) +
    labs(title = "Fixation number\n(in %)") +
    cTheme

plt_sa_lat <- 
    dscr_sa_lat %>% 
    ggplot(
        aes(
            x=group_id,
            y = m_lat,
            fill = fix_id)) +
    geom_col(position = "dodge") +
    geom_errorbar(
        aes(
            ymin=l_se_lat,
            ymax=u_se_lat),
        width = .2,
        position=position_dodge(.9)) + 
    scale_y_continuous(name = element_blank(), limits = c(0,3400),
                       expand = c(0,0)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsHB) +
    labs(title = "Fixation latency\n(in ms)") +
    cTheme

# plt_sa_dur_bin <- 
#     dscr_et_dur_bin %>% 
#     filter(fix_id == "head" |fix_id == "body") %>%  
#     ggplot(aes(x=bin_id, y=m_dur, color=fix_id)) +
#     geom_line()+
#     geom_point()+
#     geom_errorbar(
#         aes(
#             ymin=l_se_dur,
#             ymax=u_se_dur),
#         width = .1) + 
#     scale_x_continuous(name = "Bins á 2 seconds") +
#     scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1)) +
#     scale_color_manual(
#         values=c(
#             head = "#2b83ba",
#             body = "#abdda4")) +
#     facet_wrap(~group_id, nrow = 2,
#                labeller = labeller(
#                    m_dur = c(m_dur= "TESTI"),
#                    group_id = c(
#                        free = "free viewing",
#                        mem = "explicit encoding"))) +
#     labs(title = "B", subtitle = "Temporal relative fixation duration for social attention") +
#     cTheme
# 
# plt_sa_num_bin <-
#     dscr_et_dur %>% 
#     filter(fix_id == "head" |fix_id == "body") %>%  
#     ggplot(aes(x=bin_id, y=m_num, color=fix_id)) +
#     geom_line()+
#     geom_point()+
#     geom_errorbar(
#         aes(ymin=l_se_num,
#             ymax=u_se_num),
#         width = .1) + 
#     scale_x_continuous(name = "Bins á 2 seconds") +
#     scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1)) +
#     scale_color_manual(
#         values=c(
#             head = "#2b83ba",
#             body = "#abdda4")) +
#     facet_wrap(~group_id, nrow = 2,
#                labeller = labeller(
#                    group_id = c(
#                        free = "free viewing",
#                        mem = "explicit encoding"))) +
#     labs(title = "B", subtitle = "Temporal relative fixation number for social attention") +
#     cTheme



# cLimitsHeads = c(0,0.28)
# 
# pl.fixdur.head <- 
#     ggplot(
#         subset(dscr.fix.rpl,
#                stat == "mean" & fix == "fix" & gazed %in% c("face", "body")),
#         aes(x= group, y= measure, fill = gazed)) +
#     geom_col(position = "dodge") +
#     geom_errorbar(aes(
#         ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
#                     & gazed %in% c("face", "body"))$measure - subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fix"
#                         & gazed %in% c("face", "body"))$measure,
#         ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
#                     & gazed %in% c("face", "body"))$measure + subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fix"
#                         & gazed %in% c("face", "body"))$measure),
#         width=.2,
#         position=position_dodge(.9)) +
#     #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
#     #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
#     #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
#     #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
#     #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
#     scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
#                        limits = cLimitsHeads, expand = c(0,0),
#                        breaks = c(.05, .10, .15, .20, .25)) +
#     scale_x_discrete(name = element_blank(), labels=cLabels) +
#     scale_fill_manual(name = "Persons", labels = c("Head", "Body"),
#                       values=cColorsHB) +
#     labs(title = "B", subtitle = "Fixation duration\n")
# 
# cLegendHead <- get_legend(pl.fixdur.head)
# 
# pl.fixdur.head <- pl.fixdur.head +
#     cTheme
# 
# 
# pl.fixnum.head <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
#                                 & gazed %in% c("face", "body")),
#                          aes(x= group, y= measure, fill = gazed)) +
#     geom_col(position = "dodge") +
#     geom_errorbar(aes(
#         ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
#                     & gazed %in% c("face", "body"))$measure - subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixn"
#                         & gazed %in% c("face", "body"))$measure,
#         ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
#                     & gazed %in% c("face", "body"))$measure + subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixn"
#                         & gazed %in% c("face", "body"))$measure),
#         width=.2,
#         position=position_dodge(.9)) +
#     ## Significanes from duration!!!
#     #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
#     #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
#     #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
#     #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
#     #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
#     scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
#                        limits = cLimitsHeads, expand = c(0,0),
#                        breaks = c(.05, .10, .15, .20, .25)) +
#     scale_x_discrete(name = element_blank(), labels=cLabels) +
#     scale_fill_manual(name = element_blank(), values=cColorsHB) +
#     labs(title = "C", subtitle = "Fixation number\n") +
#     cTheme
# 
# pl.fixlat.head <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
#                                 & gazed %in% c("face", "body")),
#                          aes(x= group, y= measure, fill = gazed)) +
#     geom_col(position = "dodge") +
#     geom_errorbar(aes(
#         ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
#                     &  gazed %in% c("face", "body"))$measure - subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixlat"
#                         & gazed %in% c("face", "body"))$measure,
#         ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
#                     &  gazed %in% c("face", "body"))$measure + subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixlat"
#                         & gazed %in% c("face", "body"))$measure),
#         width=.2,
#         position=position_dodge(.9)) +
#     #geom_signif(annotation="***", y_position=3300, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
#     #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
#     #geom_signif(annotation="***", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .001
#     #geom_signif(annotation="***", y_position=3100, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p < .001
#     #geom_signif(annotation="***", y_position=3100, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # gazed mem - not gazed mem: p < .001
#     scale_y_continuous(name = element_blank(), limits = c(0,2800),
#                        expand = c(0,0)) +
#     scale_x_discrete(name = element_blank(), labels=cLabels) +
#     scale_fill_manual(name = element_blank(), values=cColorsHB) +
#     labs(title = "A", subtitle = "Fixation latency\n(in ms)") +
#     cTheme
