pl.mem <- dscr.mem %>%
    ggplot(aes(x = group, y=mean, fill = key)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=lowerSE,
        ymax=upperSE),
        width=.2,
        position=position_dodge(.9)) +
    #geom_signif(annotation="***", y_position=10.5, xmin=1, xmax=2, tip_length = 0.01, vjust = 0.5) + # free - mem: p < .001
    #geom_signif(annotation="***", y_position=.42, xmin=1.188, xmax=2.188, tip_length = 0.01, vjust = 0.5) + # not-gazed free - not-gazed mem: p < .001,
    #geom_signif(annotation="**", y_position=.40, xmin=0.81, xmax=1.81, tip_length = 0.01, vjust = 0.5) + # gazed free - gazed mem: p < .003
    #geom_signif(annotation="n.s.", y_position=11, xmin=0.81, xmax=1.188, tip_length = 0.01, vjust = 0) + # gazed free - not gazed free: p = .01
    #geom_signif(annotation="n.s.", y_position=11, xmin=1.81, xmax=2.188, tip_length = 0.01, vjust = 0) + # gazed mem - not gazed mem: p = .679
    scale_y_continuous(name = element_blank(), breaks = seq(0,10, by = 5),
                       limits = c(0,11), expand = c(0,0)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsCU) +
    labs(title = " ", subtitle = "Memory performance\n(in total)") +
    cTheme
