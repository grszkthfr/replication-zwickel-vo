
find_bins <- function(eyetracking_data, bin_size_ms,
                      trial_start = 0, trial_end = 10000){
    
    data = eyetracking_data
    
    # set bin size
    bin_size = bin_size_ms

    # unsure how useful:
    # set trial start
    trial_start = trial_start  # in ms
    
    # set trial end
    trial_end = trial_end
        
    #################################################################
    
    # create bins, from trial start to trial end in bins of size ...
    bins <- seq(trial_start, trial_end, by = bin_size)
    
    # build empty datafram for each subject, each trial, each fixation all posibble
    # bins
    df_bin_empty <- 
        as_tibble(
            matrix(
                data = NA,
                ncol = length(bins),
                nrow = nrow(data)),
            .name_repair = ~ str_c("bin_", 1:length(bins))) %>% 
        
        # add relevant ids
        cbind(data %>% select(subject_id, trial_id, fix_n)) %>%
        
        # gather bins across all fixations for all trials for all subjects
        unite(
            "key",
            c("subject_id", "trial_id", "fix_n"),
            sep = "_", remove = F) %>% 
        gather("bin_id", "value", bin_1:str_c("bin_",length(bins))) %>%
        
        # cosmetics
        mutate(bin_id = str_extract(bin_id, "[[0-9]]+") %>% as.numeric()) %>%
        select(-value, -key) %>% arrange(subject_id, trial_id, fix_n) 
    
    # join eyetracking data with empty bin dataframe, to identify bins for each
    # fixation and calculate it's duration for each bin it touches.
    df_bin <-
        
        full_join(
            
            data %>%
                select(subject_id, trial_id,
                       fix_n, fix_start, fix_end, fix_dur),
            df_bin_empty,
            by = c("subject_id", "trial_id", "fix_n")) %>% 
        
        mutate(
            
            # bin start in ms
            bin_start = as.numeric(bin_id) * bin_size - bin_size,
            
            # bin_end in ms
            bin_end = as.numeric(bin_id) * bin_size,
            
            # in which intervall falls fix_start/fix_end
            bin_fix_start = findInterval(fix_start, bins),
            bin_fix_end = findInterval(fix_end, bins),
            
            # number of bins per fixation
            # max_bin_n = bin_fix_end - bin_fix_start + 1,
            
            # calculate how much of fixation duration is in bin, NA if fixation does
            # not touch bin
            bin_dur = case_when(
                
                # First bin
                # fixation ends in other bin
                bin_id == bin_fix_start & bin_id < bin_fix_end ~ bin_end - fix_start,
                #fixation ends in same bin
                bin_id == bin_fix_start & bin_id == bin_fix_end ~ fix_dur,
                
                # Last bin
                bin_id == bin_fix_end & bin_id != bin_fix_start ~ fix_end - bin_start,
                
                # Bins in between
                bin_id > bin_fix_start & bin_id < bin_fix_end ~ bin_size)) %>%

        filter(
            # remove NAs, bin_duration might be 0 when fixation ends on bin start
            bin_dur != 0,
            # empty bins, results in incomplete cases (not all bins in each trial)
            !is.na(bin_dur)) %>%
        
        # count 1:... bins per subject and trial
        group_by(subject_id, trial_id) %>%
        mutate(bin_n = 1:n()) %>%
        ungroup() %>%
        
        # keep only relevant vars
        select(
            subject_id, trial_id, fix_n,
            bin_id, bin_n, bin_dur)
    
    # check if the sum of all bin durations equals the sum of all fixation durations
    # check <- df_bin  %>% summarise(sum = sum(bin_dur)) ==
    #     data  %>% summarise(fix_dur = sum(fix_dur)) %>% as.logical()
    
    # if check == TRUE:
    #     df_bin
    # 
    # else:
    #     print("fixation duration is not equal bin duration!")
    
}
