
source("02_scripts/01-libraries.R")
source("02_scripts/02-paths.R")
source(path(path_scripts_data, "eyetracking.R"))
source(path(path_scripts_data, "memory.R"))
source(path(path_scripts_publ, "plot_cosmetics.R"))

source(path(path_scripts_fun, "find_bins.R"))

bin_size = 1000
df_bin <- find_bins(df_eyetracking, bin_size = bin_size)

df_et_bins <- 
    left_join(
        df_eyetracking %>% select(subject_id, trial_id, fix_n, fix_id),
        df_bin,
        by = c("subject_id", "trial_id", "fix_n")) %>%
    left_join(
        df_memory %>% select(subject_id, condition_id) %>% unique(),
        by = "subject_id") %>%
    group_by(fix_id, bin_id, condition_id) %>%
    summarise(sum_bin_dur_obj = sum(bin_dur)) %>% ungroup() %>%
    group_by(bin_id, condition_id) %>%
    mutate(sum_bin_dur_obj = sum_bin_dur_obj/sum(sum_bin_dur_obj)) %>%
    filter(fix_id != "background")

df_et_bins %>%
    ggplot(aes(x=bin_id, y=sum_bin_dur_obj, color=fix_id, shape = condition_id)) +
    geom_line()+
    geom_point()+
    scale_color_manual(
        values=c(
            head = "#2b83ba",
            body = "#abdda4",
            object_cued = "#fdb863",
            object_uncued = "#b2abd2")) +
    facet_wrap(~condition_id) +
    theme(
        title = element_text(
            size = 8,
            #family = "Times New Roman", #excluded due to warnings
            color = "black"),
        text=element_text(
            size=10,
            #family = "Times New Roman", #excluded due to warnings
            color = "black"),
        axis.title=element_text(
            size=12,
            #family = "Times New Roman", #excluded due to warnings
            color = "black"),
        axis.line = element_line(
            colour = "black"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
            size = .01,
            colour = "grey"),
        panel.background = element_blank(),
        legend.text = element_text(
            size=12,
            color = "black"))
