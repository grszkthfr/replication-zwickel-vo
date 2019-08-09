# source(path(path_scripts_publ, "dscr_et.R"))
# source(path(path_scripts_publ, "plt_cosmetics.R"))


plt_ja_dur <- 
    dscr_ja_dur %>% 
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
                       limits = cLimitsObjects, expand = c(0,0),
                       breaks = c(.05, .10, .15)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = "Objects", labels = c("cued", "uncued"),
                      values=cColorsCU) +
    labs(title = "Object fixations")

cLegendObjects <- get_legend(plt_ja_dur)

plt_ja_dur <- plt_ja_dur +
    labs(title = "Fixation duration\n(in %)") +
    cTheme


plt_ja_num <- 
    dscr_ja_num %>% 
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
                       limits = cLimitsObjects, expand = c(0,0),
                       breaks = c(.05, .10, .15)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsCU) +
    labs(title = "Fixation number\n(in %)") +
    cTheme

plt_ja_lat <- 
    dscr_ja_lat %>% 
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
    scale_fill_manual(name = element_blank(), values=cColorsCU) +
    labs(title = "Fixation latency\n(in ms)") +
    cTheme

plt_ja_sac <- "kommt!"

# TODO
pl.sac.obj <- ggplot(subset(dscr.sac.rpl, stat == "mean" & area == "f"),
                     aes(x= group, y= measure, fill = gazed)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=subset(dscr.sac.rpl, stat == "mean" & area == "f")$measure - subset(
            dscr.sac.rpl, stat == "se" & area == "f" )$measure,
        ymax=subset(dscr.sac.rpl, stat == "mean" & area == "f")$measure + subset(
            dscr.sac.rpl, stat == "se" & area == "f")$measure),
        width=.2,
        position=position_dodge(.9)) +
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsObjects, expand = c(0,0),
                       breaks = c(.05, .10, .15)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values = cColorsCU) +
    labs(title = "Saccades leaving head\n(in %)") +
    cTheme

# plt_ja_dur_bin <- 
#     dscr_et_dur_bin  %>% 
#     filter(fix_id == "object_cued" | fix_id == "object_uncued") %>% 
#     ggplot(aes(x=bin_id, y=m_dur, color=fix_id)) +
#     geom_line() +
#     geom_point() +
#     geom_errorbar(
#         aes(
#             ymin=l_se_dur,
#             ymax=u_se_dur),
#         width = .1) +
#     scale_x_continuous(name = "Bins") +
#     scale_y_continuous(name = element_blank(),
#                        labels = percent_format(accuracy = 1),
#                        limits = c(0, .25),
#                        breaks = c(.10, .20)) +
#     scale_color_manual(
#         values=c(
#             object_cued = "#fdb863",
#             object_uncued = "#b2abd2"
#         )) +
#     facet_wrap(~group_id, nrow = 1,
#                labeller = labeller(
#                    m_dur = c(m_dur= "TESTI"),
#                    group_id = c(
#                        free = "free viewing",
#                        mem = "explicit encoding"))) +
#     labs(title = "A", subtitle = "Temporal relative fixation durations for joint attention") +
#     cTheme
# 
# 
# plt_ja_num_bin <-
#     dscr_et_num_bin %>% 
#     filter(fix_id == "object_cued" | fix_id == "object_uncued") %>% 
#     ggplot(aes(x=bin_id, y=m_num, color=fix_id)) +
#     geom_line()+
#     geom_point()+
#     geom_errorbar(
#         aes(ymin=l_se_num,
#             ymax=u_se_num),
#         width = .1) + 
#     scale_x_continuous(name = "Bins") +
#     scale_y_continuous(name = element_blank(),
#                        labels = percent_format(accuracy = 1),
#                        limits = c(0, .25),
#                        breaks = c(.10, .20)) +
#     scale_color_manual(
#         values=c(
#             object_cued = "#fdb863",
#             object_uncued = "#b2abd2"
#         )) +
#     facet_wrap(~group_id, nrow = 1,
#                labeller = labeller(
#                    m_dur = c(m_dur= "TESTI"),
#                    group_id = c(
#                        free = "free viewing",
#                        mem = "explicit encoding"))) +
#     labs(title = "B", subtitle = "Temporal relative fixation number for joint attention") +
#     cTheme

# pl.fixdur.obj <- 
#     ggplot(
#         data = subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
#                                & gazed %in% c("gaze", "nongaze")),
#         aes(x= group, y= measure, fill = gazed)) +
#     geom_col(position = "dodge") +
#     geom_errorbar(
#         aes(
#             ymin=subset(
#                 dscr.fix.rpl, stat == "mean" & fix == "fix"
#                     & gazed %in% c("gaze", "nongaze"))$measure - 
#                 subset(
#                     dscr.fix.rpl, stat == "se" & fix == "fix"
#                         & gazed %in% c("gaze", "nongaze"))$measure,
#             ymax=subset(
#                 dscr.fix.rpl, stat == "mean" & fix == "fix"
#                     & gazed %in% c("gaze", "nongaze"))$measure + 
#                 subset(
#                     dscr.fix.rpl, stat == "se" & fix == "fix"
#                         & gazed %in% c("gaze", "nongaze"))$measure),
#         width=.2,
#         position=position_dodge(.9)) +
#     #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
#     #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
#     #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
#     #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
#     #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
#     scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
#                        limits = cLimitsObjects, expand = c(0,0),
#                        breaks = c(.05, .10, .15)) +
#     scale_x_discrete(name = element_blank(), labels=cLabels) +
#     scale_fill_manual(name = "Objects", labels = c("cued", "uncued"),
#                       values=cColorsCU) +
#     labs(title = "Object fixations", subtitle = "Fixation duration\n")
# 
# cLegendObjects <- get_legend(pl.fixdur.obj)
# 
# pl.fixdur.obj <- pl.fixdur.obj +
#     labs(title = "B", subtitle = "Fixation duration\n") +
#     cTheme
# 
# pl.fixnum.obj <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
#                                & gazed %in% c("gaze", "nongaze")),
#                         aes(x= group, y= measure, fill = gazed)) +
#     geom_col(position = "dodge") +
#     geom_errorbar(aes(
#         ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
#                     & gazed %in% c("gaze", "nongaze"))$measure - subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixn"
#                         & gazed %in% c("gaze", "nongaze"))$measure,
#         ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
#                     & gazed %in% c("gaze", "nongaze"))$measure + subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixn"
#                         & gazed %in% c("gaze", "nongaze"))$measure),
#         width=.2,
#         position=position_dodge(.9)) +
#     ## Significanes from duration!!!
#     #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
#     #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
#     #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
#     #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
#     #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
#     scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
#                        limits = cLimitsObjects, expand = c(0,0),
#                        breaks = c(.05, .10, .15)) +
#     scale_x_discrete(name = element_blank(), labels=cLabels) +
#     scale_fill_manual(name = element_blank(), values=cColorsCU) +
#     labs(title = "C", subtitle = "Fixation number\n") +
#     cTheme
# 
# pl.fixlat.obj <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
#                                & gazed %in% c("gaze", "nongaze")),
#                         aes(x= group, y= measure, fill = gazed)) +
#     geom_col(position = "dodge") +
#     geom_errorbar(aes(
#         ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
#                     &  gazed %in% c("gaze", "nongaze"))$measure - subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixlat"
#                         & gazed %in% c("gaze", "nongaze"))$measure,
#         ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
#                     &  gazed %in% c("gaze", "nongaze"))$measure + subset(
#                         dscr.fix.rpl, stat == "se" & fix == "fixlat"
#                         & gazed %in% c("gaze", "nongaze"))$measure),
#         width=.2,
#         position=position_dodge(.9)) +
#     #geom_signif(annotation="***", y_position=3300, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
#     #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
#     #geom_signif(annotation="***", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .001
#     #geom_signif(annotation="***", y_position=3100, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p < .001
#     #geom_signif(annotation="***", y_position=3100, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # gazed mem - not gazed mem: p < .001
#     scale_y_continuous(name = element_blank(), limits = c(0,3400),
#                        expand = c(0,0)) +
#     scale_x_discrete(name = element_blank(), labels=cLabels) +
#     scale_fill_manual(name = element_blank(), values=cColorsCU) +
#     labs(title = "A", subtitle = "Fixation latency\n(in ms)") +
#     cTheme
# 
# pl.sac.obj <- ggplot(subset(dscr.sac.rpl, stat == "mean" & area == "f"),
#                      aes(x= group, y= measure, fill = gazed)) +
#     geom_col(position = "dodge") +
#     geom_errorbar(aes(
#         ymin=subset(dscr.sac.rpl, stat == "mean" & area == "f")$measure - subset(
#             dscr.sac.rpl, stat == "se" & area == "f" )$measure,
#         ymax=subset(dscr.sac.rpl, stat == "mean" & area == "f")$measure + subset(
#             dscr.sac.rpl, stat == "se" & area == "f")$measure),
#         width=.2,
#         position=position_dodge(.9)) +
#     #geom_signif(annotation="***", y_position=.15, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
#     #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p = .002
#     #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .001
#     #geom_signif(annotation="***", y_position=.13, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .001
#     #geom_signif(annotation="***", y_position=.13, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # gazed mem - not gazed mem: p = .001
#     scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
#                        limits = cLimitsObjects, expand = c(0,0),
#                        breaks = c(.05, .10, .15)) +
#     scale_x_discrete(name = element_blank(), labels=cLabels) +
#     scale_fill_manual(name = element_blank(), values = cColorsCU) +
#     labs(title = "D", subtitle = "Saccades leaving head\n") +
#     cTheme
