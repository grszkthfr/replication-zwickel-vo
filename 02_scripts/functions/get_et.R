# https://stackoverflow.com/a/47994870/7285920
# https://www.tidyverse.org/articles/2019/06/rlang-0-4-0/
# get_et_duration <- function(
#     eyetracking_data, across_vars, by_vars, coi){
#     
#     
#     vars_group_sum <- quos(!!! alist(by_vars, across_vars))
#     by_vars <- enquo(by_vars)
#     coi <- enquo(coi)
# 
#     duration <-
#         eyetracking_data %>%
#         group_by(!!! vars_group_sum) %>%
#         summarise(abs_roi = sum(!!coi)) %>%
#         group_by(!!! by_vars) %>%
#         mutate(abs_all = sum(abs_roi),
#                prop_dur = abs_roi/abs_all) %>%
#         ungroup()
# 
#     duration
#     
# } 

get_et_duration <- 
    function(eyetracking_data, across_vars, by_vars, coi){
        
        vars_group_sum <- c(by_vars, across_vars)
        vars_group_relative <- by_vars
    
        if (coi=="fix_dur") {
            duration <-
                eyetracking_data %>%
                group_by(.dots=vars_group_sum) %>%
                summarise(abs_roi = sum(fix_dur)) %>%
                group_by(.dots=vars_group_relative) %>%
                mutate(abs_all = sum(abs_roi),
                       prop_dur = abs_roi/abs_all) %>%
                ungroup()
        } else if (coi=="bin_dur") {
            
            duration <-
                eyetracking_data %>%
                group_by(.dots=vars_group_sum) %>%
                summarise(abs_roi = sum(bin_dur)) %>%
                group_by(.dots=vars_group_relative) %>%
                mutate(abs_all = sum(abs_roi),
                       prop_dur = abs_roi/abs_all) %>%
                ungroup()
        }
        
        
        duration
        
    } 



get_et_count <- function(eyetracking_data, across_vars, by_vars){
    
    # eyetracking_data <- df_et
    # across_vars <- "fix_id"
    # by_vars <- c("subject_id", "trial_id", "baseline_ok")
    
    vars_group_sum <- c(by_vars, across_vars)
    vars_group_relative <- by_vars
    
    count <- 
        eyetracking_data %>%
        mutate(fix_counter = ifelse(!is.na(fix_n), 1, 0)) %>% 
        group_by(.dots=vars_group_sum) %>%
        summarise(abs_roi = sum(fix_counter)) %>%
        mutate(abs_all = sum(abs_roi),
               prop_num = abs_roi/abs_all) %>%
        ungroup()
    
    # # make sure every roi is represented in each bin.
    # complete(
    #     # vars
    #     nesting(quos(alist(across_vars))), !!by_vars,
    #     # fill bin duration with 0s, if fix_id  for roi is generated with complete
    #     fill = list(prop_num = 0))
    
    
    count
}

get_et_latency <- function(eyetracking_data){
    
    
    latency <-
        eyetracking_data %>%
        drop_na(fix_start) %>% 
        group_by(subject_id, trial_id, fix_id) %>%
        summarise(latency = min(fix_start)) %>%
        group_by(subject_id, fix_id) %>%
        summarise(
            m_lat = mean(latency))%>%
        ungroup()
    
    latency
}
