dscr_stim <-
    read_delim(
        path(path_postp, "pre_processing", "ROI", "roisizes.csv"),
        delim = ";",
    escape_double = FALSE,
    col_types = cols(
        background = col_number(),
        body = col_number(),
        gaze = col_number(),
        head = col_number(),
        nongaze = col_number()),
    trim_ws = TRUE) %>%
    gather(stimulus, size, background:nongaze) %>%
    rename(fix_id = stimulus) %>%
    mutate(
        fix_id = ifelse(
            fix_id == "nongaze",
            "object_uncued",
                 ifelse(
                     fix_id == "gaze",
                     "object_cued",
                     fix_id))) %>%
    group_by(fix_id) %>%
    summarise(
        m_size = mean(size)/(1280*960),
        sd_size = sd (size)/(1280*960),
        min_size = min(size)/(1280*960),
        max_size = max(size)/(1280*960))
