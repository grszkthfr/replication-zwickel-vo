
source("02_scripts/01-libraries.R")
source("02_scripts/02-paths.R")
source(path(path_scripts_fun, "get_et.R"))
source(path(path_scripts_data, "memory.R"))
source(path(path_scripts_fun, "find_bins.R"))


# protocol of eyetracking ####  
df_et_protocol <-
    dir_ls(path(path_postp, "prot/protocols"), glob = "*.csv") %>%
    map_df(
        read_delim,
        col_types = cols(
            vp = col_character(),
            trial = col_double(),
            pic = col_character(),
            iti = col_double(),
            blx = col_double(),
            bly = col_double(),
            blok = col_double()),
        delim = ";",
        locale = locale(
            decimal_mark = ",",
            grouping_mark = ".")) %>% 
    select(
        subject_id = vp,
        trial_id = trial,
        baseline_ok = blok,
        stim_id = pic) %>% 
    mutate(subject_id = str_remove(subject_id, "vpja"))

# all eyetracking fixation samples ####
df_et <-
    dir_ls(path_postp_etr, glob = "*.csv") %>%
    map_df(
  
        read_delim,
        col_types = cols(
            vp = col_character(),
            trial = col_double(),
            timest = col_double(),
            timeend = col_double(),
            x = col_double(),
            y = col_double(),
            duration = col_double(),
            roi = col_double()),
        delim = ";",
        locale = locale(
            decimal_mark = ",",
            grouping_mark = ".")) %>%

    select(
        subject_id = vp,
        trial_id = trial,
        fix_start = timest,
        fix_end = timeend,
        fix_roi = roi,
        fix_dur = duration) %>%
    
  # black to white
    mutate(fix_roi = ifelse(fix_roi == 255255255, 0, fix_roi)) %>% 
  
    # no fix_roi, e.g. closed eyes
    drop_na(fix_roi) %>%
  
    # complete, add NA rows for missing fix_rois
    # must be 93vp * 5roi * 26trials = 12090 unique compbination
    complete(nesting(subject_id, trial_id), fix_roi) %>% 
  
    mutate(
        # fixed colors in image file 11B, 12B, 12C, 12D
        # TODO replace rgb with color! with rgb(), but needs 3 numbers
        # fix_roi =
        #     case_when(
        #         fix_roi == 225225000 ~ 255255000,
        #         fix_roi == 250000000 ~ 255000000,
        #         fix_roi == 250       ~ 255,
        #         fix_roi == 25525500 | 255000000 | 255000 | 255 | 0 ~ fix_roi),

        fix_id =
            case_when(
                fix_roi == 255255000 ~ "object_uncued", # yellow
                fix_roi == 255000000 ~ "body", # red
                fix_roi == 255000 ~ "head", # green
                fix_roi == 255 ~ "object_cued", # blue
                fix_roi == 0 ~ "background") %>% # white
            as_factor(),

        # fix subject_id
        subject_id = str_remove(subject_id, "vpja"),
        fix_dur = ifelse(is.na(fix_dur), 0, fix_dur)) %>%
    
    mutate_if(is.double, as.numeric) %>%

    left_join(
        df_et_protocol,
        by = c("subject_id", "trial_id")) %>%
  
    # count valid fixations
    arrange(subject_id, trial_id, fix_start) %>% 
    group_by(subject_id, trial_id, baseline_ok) %>% 
    mutate(fix_n = 1:n()) %>% 
    ungroup() %>%
    mutate(fix_n = ifelse(is.na(fix_start), NA, fix_n)) %>% 
  
    # keep only valid fixations
    filter(baseline_ok == 1)

# collapsed measures ####

# duration ####
df_et_dur <-
  
  df_et %>%
  
  # get collapsed duration for fix_id for each subject
  get_et_duration(
    across_vars = "fix_id",
    by_vars = "subject_id",
    coi = "fix_dur") %>%
  
  # join the durations with the condition subjects were in
  left_join(
    df_memory %>%
      select(subject_id, condition_id) %>%
      unique(),
    by = "subject_id") %>%
  
  # select relevant columns
  select(subject_id, condition_id, fix_id, prop_dur)

# counts ####
df_et_cnt <-
  
  df_et %>%
  
  # get collapsed duration for fix_id for each subject
  get_et_count(
    across_vars = "fix_id",
    by_vars = c("subject_id")) %>%
  
  # join the durations with the condition subjects were in
  left_join(
    df_memory %>%
      select(subject_id, condition_id) %>%
      unique(),
    by = "subject_id") %>%
  
  # select relevant columns
  select(subject_id, condition_id, fix_id, prop_cnt)

# latency ####
df_et_lat <-
  
  df_et %>%
  
  # get collapsed duration for fix_id for each subject
  get_et_latency() %>%
  
  # join the durations with the condition subjects were in
  left_join(
    df_memory %>%
      select(subject_id, condition_id) %>%
      unique(),
    by = "subject_id") %>%
  
  # select relevant columns
  select(subject_id, condition_id, fix_id, mean_latency)


# bins ####
# duration ####
df_et_dur_bin <-
  
  # join bin_ids with eyetracking data, not necessary but keeps some ID vars
  left_join(
    df_et,
    find_bins(df_et, bin_size = 2000),
    by = c("subject_id", "trial_id", "fix_n")) %>%
  
  # make sure every roi is represented in each bin.
  drop_na(bin_dur) %>%
  complete(
    # vars
    nesting(subject_id, trial_id, bin_id, baseline_ok, stim_id), fix_id,
    # fill bin duration with 0s, if fix_id  for roi is generated with complete
    fill = list(bin_dur = 0)) %>% 
  
  # get collapsed duration for fix_id for each subject
  get_et_duration(
    across_vars = "fix_id",
    by_vars = c("subject_id", "bin_id"),
    coi = "bin_dur") %>%
  
  # join the durations with the condition subjects were in
  left_join(
    df_memory %>%
      select(subject_id, condition_id) %>%
      unique(),
    by = "subject_id") %>%

  # select relevant columns
  select(subject_id, condition_id, bin_id, fix_id, prop_dur)
  
# # test
# test <-
#   df_et_dur_bin %>%
#   group_by(subject_id, fix_id) %>%
#   summarise(
#     abs_roi = sum(abs_roi),
#     abs_all = sum(abs_all),
#     prop_dur = mean(prop_dur),
#     other_prop_dur = abs_roi/abs_all) %>% 
#   left_join(df_memory %>% select(subject_id, condition_id) %>% unique(),
#             by = "subject_id")

# counts ####
df_et_cnt_bin <-
  
  # join bin_ids with eyetracking data, not necessary but keeps some ID vars
  left_join(
    df_et,
    find_bins(df_et, bin_size = 2000),
    by = c("subject_id", "trial_id", "fix_n")) %>%
  
  # make sure every roi is represented in each bin.
  drop_na(bin_dur) %>%
  complete(
    # vars
    nesting(subject_id, trial_id, bin_id, baseline_ok, stim_id), fix_id,
    # fill bin duration with 0s, if fix_id  for roi is generated with complete
    fill = list(bin_dur = 0)) %>%
  
  # get collapsed duration for fix_id for each subject
  get_et_count(
    across_vars = "fix_id",
    by_vars = c("subject_id", "bin_id")) %>%
  
  # join the durations with the condition subjects were in
  left_join(
    df_memory %>%
      select(subject_id, condition_id) %>%
      unique(),
    by = "subject_id") %>%
  
  # select relevant columns
  select(subject_id, condition_id, bin_id, fix_id, prop_cnt)

# memory
# duration ####
df_mem_dur <- 
  left_join(
    df_et %>%
      get_et_duration(
        across_vars = "fix_id",
        by_vars = c("subject_id", "trial_id"),
        coi = "fix_dur") %>% 
      filter(fix_id == "object_cued" | fix_id == "object_uncued") %>% 
      mutate(fix_id = fct_drop(fix_id)), # drop unused factor levels
    df_memory,
    by = c("subject_id", "trial_id", "fix_id" = "stim_cued")) %>% 
  select(subject_id, stim_id, condition_id, fix_id,
         prop_dur, stim_recall)

# counts ####
df_mem_cnt <-
  left_join(
    df_et %>% 
      get_et_count(
        across_vars = "fix_id",
        by_vars = c("subject_id", "trial_id")) %>% 
      filter(fix_id == "object_cued" | fix_id == "object_uncued") %>%
      mutate(fix_id = fct_drop(fix_id)) %>% 
      complete(nesting(subject_id, trial_id), fix_id, fill = list(prop_cnt = 0)), # drop unused factor levels
    df_memory,
    by = c("subject_id", "trial_id", "fix_id" = "stim_cued"))%>% 
  select(subject_id, stim_id, condition_id, fix_id,
         prop_cnt, stim_recall)

# difference <-
#     df_et_duration %>%
#     spread(
#         key= fix_id,
#         value = rel_duration_id) %>%
#     select(subject_id, head, body, object_cued, object_uncued) %>%
#     left_join(
#         df.w.et %>%
#             select(vp, starts_with("fix.")),
#         by = c("subject_id" = "vp")) %>%
#     mutate(
#       diff_head = head - fix.face,
#       diff_body = body - fix.body,
#       diff_cued=  object_cued- fix.gaze,
#       diff_uncued= object_uncued - fix.nongaze,
#     ) #%>% #filter(subject_id == "05")
#   select(subject_id, starts_with("diff"))
# 
# df_et_alt <-
#   dir_ls(path_postp_etc, glob = "*.csv") %>%
#   map_df( ~{
#   
#     filename = .
#     df <- 
#       read_delim(
#         filename, col_types = cols(
#           .default = col_double(),
#           pic = col_character()), delim = ";",locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
#       mutate(
#         vp = str_extract(tools::file_path_sans_ext(basename(filename)), "[0-9]+")) %>% 
#       select(vp, trial, starts_with("fixlat."), blok)
#     
#     df
#     
#   }) %>%
#   select(subject_id = vp,
#          trial_id = trial,
#          head = fixlat.face,
#          body = fixlat.body,
#          object_cued = fixlat.gaze,
#          object_uncued = fixlat.nongaze,
#          blok)
# 
# 
# df_et_neu <- df_et %>% filter(fix_id  != "background") %>%
#   group_by(subject_id, trial_id, fix_id) %>% summarise(first_fix=fix_start[1]) %>% 
#   spread(key = fix_id, value = first_fix) %>% 
#   select(subject_id, trial_id, head, body, object_cued, object_uncued)
# 
# 
# diff_neu_alt <- setdiff(df_et_neu, df_et_alt %>% select(-blok)) %>% 
#   left_join(
#     df_et_alt %>%
#       select(
#         subject_id,
#         trial_id,
#         alt_head = head,
#         alt_body = body,
#         alt_cued = object_cued,
#         alt_uncued = object_uncued),
#     by = c("subject_id", "trial_id")) %>% 
#   left_join(df_et_protocol, by = c("subject_id", "trial_id"))
# 
# df_et %>% filter(fix_roi == 22522500|fix_roi == 25000000|fix_roi == 250) %>% View(#%>% left_join(df_et_protocol) %>% View()
# write_csv2(diff_neu_alt, "diff_neu_alt.csv") 
# 
# df_et %>% filter(subject_id == "13", trial_id == "19", fix_id == "object_cued") %>% View()
# 
# 
# View(body_vp9)
# df_et %>% filter(subject_id == "05", trial_id == "8") %>% View()
# 
# 
# df_et_latency %>% filter(subject_id == "05")
# df.w.et %>% filter(vp == "05") %>% select(vp, starts_with("fixlat"))


# 
# test_neu$sum <- rowSums(test_neu[2:5]) 
#     
# 
# test_alt <- df.w.et %>% select(vp, starts_with("fix."))
# test_alt$sum <- rowSums(test_alt[2:5]) 
# mean(1-test_alt$sum)

##########################################################################
# 
# 
# # ALL EYE TRACKING DATA
# vpn <- paste0(
#   "vpja",
#   ifelse(
#     c(1:78,81:96)<10,
#     "0",
#     ""),
#   c(1:78,81:96))
# 
# bed <- rep(c("free","mem"),47)
# 
# # ORIGINALLY ACQUIRED DATA
# #vpn <- paste0("vpja",ifelse(c(1:78)<10,"0",""),c(1:78))
# #bed <- rep(c("free","mem"),39)
# 
# bed <- bed[!(vpn %in% "vpja23")]  # missing data
# vpn <- vpn[!(vpn %in% "vpja23")]
# 
# # Loop over subjects
# erg <- numeric()
# nvalid <- numeric()
# cleantime <- numeric()
# 
# for (vp in vpn) {
#   #  print(vp)
# 
#   prot <- read.csv2(path(path_postp_etc,paste0(vp,"_Fixations.csv")))
# 
#   # Restrict to trials with valid baseline?
#   nvalid <- c(nvalid,sum(prot$blok==1))
#   prot <- prot[prot$blok==1,]
# 
#   cleantime <- c(cleantime,mean(prot$cleantime))
# 
#   erg <- rbind(erg,apply(prot[,8:ncol(prot)],2,mean,na.rm=TRUE))
# }
# 
# df.w.et <- data.frame(code=vpn,group=bed,nvalid,cleantime,erg) %>%
#   mutate(
#     code = as.factor(unlist(map(strsplit(as.character(code),"ja"), ~.x[2])))) %>%
#   rename(vp = code)



# df.l.et <- gather(df.w.et, key, value, fix.face:sac.bnongaze, factor_key=TRUE) %>%
#   mutate(
#     key = as.character(key),
#     fixations =
#       as.factor(
#         ifelse(
#           startsWith(key, "fix."),
#           "fix",
#           ifelse(
#             startsWith(key, "fixn."),
#             "fixn",
#             ifelse(
#               startsWith(key, "fixlat."),
#               "fixlat",
#               ifelse(
#                 startsWith(key, "sac."),
#                 "sac",
#                 NA))))),
#     region =
#       as.factor(
#         ifelse(
#           endsWith(key, ".face"),
#           "face",
#           ifelse(
#             endsWith(key, ".body"),
#             "body",
#             ifelse(
#               endsWith(key, ".gaze"),
#               "gaze",
#               ifelse(
#                 endsWith(key, ".nongaze"),
#                   "nongaze",
#                   ifelse(
#                     endsWith(key, ".pgaze"),
#                     "pgaze",
#                       ifelse(
#                         endsWith(key, ".fgaze"),
#                         "fgaze",
#                         ifelse(
#                           endsWith(key, ".bgaze"),
#                           "bgaze",
#                           ifelse(
#                             endsWith(key, ".pnongaze"),
#                             "pnongaze",
#                             ifelse(
#                               endsWith(key, ".fnongaze"),
#                               "fnongaze",
#                               ifelse(
#                                 endsWith(key, ".bnongaze"),
#                                 "bnongaze",
#                                 NA)))))))))))) %>%
#   arrange(vp)

# rm(prot, bed, cleantime, nvalid, vp, vpn, erg)



