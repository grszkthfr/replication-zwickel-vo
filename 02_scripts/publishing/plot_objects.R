cLimitsObjects = c(0,0.17)

pl.fixdur.obj <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
                               & gazed %in% c("gaze", "nongaze")),
                        aes(x= group, y= measure, fill = gazed)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
                    & gazed %in% c("gaze", "nongaze"))$measure - subset(
                        dscr.fix.rpl, stat == "se" & fix == "fix"
                        & gazed %in% c("gaze", "nongaze"))$measure,
        ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
                    & gazed %in% c("gaze", "nongaze"))$measure + subset(
                        dscr.fix.rpl, stat == "se" & fix == "fix"
                        & gazed %in% c("gaze", "nongaze"))$measure),
        width=.2,
        position=position_dodge(.9)) +
    #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
    #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
    #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
    #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsObjects, expand = c(0,0),
                       breaks = c(.05, .10, .15)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = "Objects", labels = c("cued", "uncued"),
                      values=cColorsCU) +
    labs(title = "Object fixations", subtitle = "Fixation duration\n(in %)")

cLegendObjects <- get_legend(pl.fixdur.obj)

pl.fixdur.obj <- pl.fixdur.obj +
    labs(title = "B", subtitle = "Fixation duration\n(in %)") +
    cTheme

pl.fixnum.obj <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
                               & gazed %in% c("gaze", "nongaze")),
                        aes(x= group, y= measure, fill = gazed)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
                    & gazed %in% c("gaze", "nongaze"))$measure - subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixn"
                        & gazed %in% c("gaze", "nongaze"))$measure,
        ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
                    & gazed %in% c("gaze", "nongaze"))$measure + subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixn"
                        & gazed %in% c("gaze", "nongaze"))$measure),
        width=.2,
        position=position_dodge(.9)) +
    ## Significanes from duration!!!
    #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
    #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
    #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
    #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsObjects, expand = c(0,0),
                       breaks = c(.05, .10, .15)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsCU) +
    labs(title = "C", subtitle = "Fixation number\n(in %)") +
    cTheme

pl.fixlat.obj <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
                               & gazed %in% c("gaze", "nongaze")),
                        aes(x= group, y= measure, fill = gazed)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
                    &  gazed %in% c("gaze", "nongaze"))$measure - subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixlat"
                        & gazed %in% c("gaze", "nongaze"))$measure,
        ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
                    &  gazed %in% c("gaze", "nongaze"))$measure + subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixlat"
                        & gazed %in% c("gaze", "nongaze"))$measure),
        width=.2,
        position=position_dodge(.9)) +
    #geom_signif(annotation="***", y_position=3300, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
    #geom_signif(annotation="***", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .001
    #geom_signif(annotation="***", y_position=3100, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p < .001
    #geom_signif(annotation="***", y_position=3100, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # gazed mem - not gazed mem: p < .001
    scale_y_continuous(name = element_blank(), limits = c(0,3400),
                       expand = c(0,0)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsCU) +
    labs(title = "A", subtitle = "Fixation latency\n(in ms)") +
    cTheme

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
    #geom_signif(annotation="***", y_position=.15, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p = .002
    #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .001
    #geom_signif(annotation="***", y_position=.13, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .001
    #geom_signif(annotation="***", y_position=.13, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # gazed mem - not gazed mem: p = .001
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsObjects, expand = c(0,0),
                       breaks = c(.05, .10, .15)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values = cColorsCU) +
    labs(title = "D", subtitle = "Saccades leaving head\n(in %)") +
    cTheme
