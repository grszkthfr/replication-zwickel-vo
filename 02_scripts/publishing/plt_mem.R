plt_mem <- dscr_mem %>%
    ggplot(aes(x = group_id, y=m_recall, fill = stim_cued)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(
        ymin=l_se_recall,
        ymax=u_se_recall),
        width=.2,
        position=position_dodge(.9)) +
    scale_y_continuous(name = element_blank(), breaks = seq(0,10, by = 5),
                       limits = c(0,11), expand = c(0,0)) +
    scale_x_discrete(name = element_blank(), labels=cLabels) +
    scale_fill_manual(name = element_blank(), values=cColorsCU) +
    labs(title = "Memory performance\n(in total)") +
    cTheme
