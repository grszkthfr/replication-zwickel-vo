cLimitsHeads = c(0,0.28)

pl.fixdur.head <- 
    ggplot(
        subset(dscr.fix.rpl,
               stat == "mean" & fix == "fix" & gazed %in% c("face", "body")),
        aes(x= group, y= measure, fill = gazed)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
                    & gazed %in% c("face", "body"))$measure - subset(
                        dscr.fix.rpl, stat == "se" & fix == "fix"
                        & gazed %in% c("face", "body"))$measure,
        ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fix"
                    & gazed %in% c("face", "body"))$measure + subset(
                        dscr.fix.rpl, stat == "se" & fix == "fix"
                        & gazed %in% c("face", "body"))$measure),
        width=.2,
        position=position_dodge(.9)) +
    #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
    #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
    #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
    #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsHeads, expand = c(0,0),
                       breaks = c(.05, .10, .15, .20, .25)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = "Persons", labels = c("Head", "Body"),
                      values=cColorsHB) +
    labs(title = "B", subtitle = "Fixation duration\n(in %)")

cLegendHead <- get_legend(pl.fixdur.head)

pl.fixdur.head <- pl.fixdur.head +
    cTheme


pl.fixnum.head <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
                                & gazed %in% c("face", "body")),
                         aes(x= group, y= measure, fill = gazed)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
                    & gazed %in% c("face", "body"))$measure - subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixn"
                        & gazed %in% c("face", "body"))$measure,
        ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixn"
                    & gazed %in% c("face", "body"))$measure + subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixn"
                        & gazed %in% c("face", "body"))$measure),
        width=.2,
        position=position_dodge(.9)) +
    ## Significanes from duration!!!
    #geom_signif(annotation="***", y_position=.19, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
    #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
    #geom_signif(annotation="*", y_position=.17, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p = .01
    #geom_signif(annotation="n.s.", y_position=.17, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
    scale_y_continuous(name = element_blank(), labels = percent_format(accuracy = 1),
                       limits = cLimitsHeads, expand = c(0,0),
                       breaks = c(.05, .10, .15, .20, .25)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsHB) +
    labs(title = "C", subtitle = "Fixation number\n(in %)") +
    cTheme

pl.fixlat.head <- ggplot(subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
                                & gazed %in% c("face", "body")),
                         aes(x= group, y= measure, fill = gazed)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
                    &  gazed %in% c("face", "body"))$measure - subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixlat"
                        & gazed %in% c("face", "body"))$measure,
        ymax=subset(dscr.fix.rpl, stat == "mean" & fix == "fixlat"
                    &  gazed %in% c("face", "body"))$measure + subset(
                        dscr.fix.rpl, stat == "se" & fix == "fixlat"
                        & gazed %in% c("face", "body"))$measure),
        width=.2,
        position=position_dodge(.9)) +
    #geom_signif(annotation="***", y_position=3300, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
    #geom_signif(annotation="***", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .001
    #geom_signif(annotation="***", y_position=3100, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0.5) + # gazed free - not gazed free: p < .001
    #geom_signif(annotation="***", y_position=3100, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # gazed mem - not gazed mem: p < .001
    scale_y_continuous(name = element_blank(), limits = c(0,2800),
                       expand = c(0,0)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsHB) +
    labs(title = "A", subtitle = "Fixation latency\n(in ms)") +
    cTheme
