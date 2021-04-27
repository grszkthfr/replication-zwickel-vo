## source(path(path_scripts_analysis, "dscr_et.R"))
## source(path(path_scripts_analysis, "plt_cosmetics.R"))

plt_et_dur_t <-
    dscr_et_dur_t  %>%
    filter(fix_id != "background") %>%
    ggplot(aes(x=bin_id, y=m_dur, color=fix_id, shape = fix_id)) +
    geom_line() +
    geom_point() +
    geom_errorbar(
        aes(
            ymin=l_se_dur,
            ymax=u_se_dur),
        width = .1) +
    scale_x_continuous(name = "time points") +
    scale_y_continuous(name = element_blank(),
                       labels = percent_format(accuracy = 1),
                       limits = c(0, .47),
                       breaks = c(.10, .20, .30, .40)) +
    scale_color_manual(
            name = "ROIs",
            labels = c("head", "body", "cued", "uncued"),
            values=c(
                object_cued = "#fdb863",
                object_uncued = "#b2abd2",
                head = "#2b83ba",
                body = "#abdda4")) +
    scale_shape_manual(
        name = "ROIs",
        labels = c("head", "body", "cued", "uncued"),
        values = c(16,17,2,1)) +
    facet_wrap(~group_id, nrow = 1,
               labeller = labeller(
                   group_id = c(
                       free = "free viewing",
                       mem = "explicit encoding"))) +
    labs(title = "Fixation duration")

cLegendTemp <- get_legend(plt_et_dur_t)

plt_et_dur_t <- plt_et_dur_t +
    cTheme

plt_et_num_t <-
    dscr_et_num_t %>%
    filter(fix_id != "background") %>%
    ggplot(aes(x=bin_id, y=m_num, color=fix_id, shape = fix_id)) +
    geom_line()+
    geom_point()+
    geom_errorbar(
        aes(ymin=l_se_num,
            ymax=u_se_num),
        width = .1) +
    scale_x_continuous(name = "time points") +
    scale_y_continuous(name = element_blank(),
                       labels = percent_format(accuracy = 1),
                       limits = c(0, .47),
                       breaks = c(.10, .20, .30, .40)) +
    scale_color_manual(
        values=c(
            object_cued = "#fdb863",
            object_uncued = "#b2abd2",
            head = "#2b83ba",
            body = "#abdda4")#,
        # background = "#000000")
    ) +
    scale_shape_manual(
        name = "ROIs",
        labels = c("head", "body", "cued", "uncued"),
        values = c(16,17,2,1)) +
    facet_wrap(~group_id, nrow = 1,
               labeller = labeller(
                   group_id = c(
                       free = "free viewing",
                       mem = "explicit encoding"))) +
    labs(title = "Number of fixation") +
    cTheme

