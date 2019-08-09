# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")
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
        df_mem %>%
            select(subject_id, group_id) %>%
            unique(),
        by = "subject_id") %>%
  
    # select relevant columns
    select(subject_id, group_id, fix_id, prop_dur)

# counts ####
df_et_num <-

    df_et %>%
  
    # get collapsed duration for fix_id for each subject
    get_et_count(
        across_vars = "fix_id",
        by_vars = c("subject_id")) %>%
  
    # join the durations with the condition subjects were in
    left_join(
        df_mem %>%
            select(subject_id, group_id) %>%
            unique(),
        by = "subject_id") %>%
  
    # select relevant columns
    select(subject_id, group_id, fix_id, prop_num)

# latency ####
df_et_lat <-

    df_et %>%

    # get collapsed duration for fix_id for each subject
    get_et_latency() %>%
  
    # join the durations with the condition subjects were in
    left_join(
        df_mem %>%
          select(subject_id, group_id) %>%
          unique(),
        by = "subject_id") %>%

    # select relevant columns
    select(subject_id, group_id, fix_id, m_lat)


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
        df_mem %>%
            select(subject_id, group_id) %>%
            unique(),
        by = "subject_id") %>%
  
    # select relevant columns
    select(subject_id, group_id, bin_id, fix_id, prop_dur)

# counts ####
df_et_num_bin <-

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
        df_mem %>%
            select(subject_id, group_id) %>%
            unique(),
        by = "subject_id") %>%
  
    # select relevant columns
    select(subject_id, group_id, bin_id, fix_id, prop_num)

# memory ####
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
        df_mem,
        by = c("subject_id", "trial_id", "fix_id" = "stim_cued")) %>%
    
    select(
      subject_id, stim_id, group_id, fix_id,
      prop_dur, stim_recall)

# counts ####
df_mem_num <-
    left_join(
        df_et %>%
            get_et_count(
                across_vars = "fix_id",
                by_vars = c("subject_id", "trial_id")) %>%
            filter(fix_id == "object_cued" | fix_id == "object_uncued") %>%
            mutate(fix_id = fct_drop(fix_id)) %>%
            complete(nesting(subject_id, trial_id), fix_id, fill = list(prop_num = 0)), # drop unused factor levels
        df_mem,
        by = c("subject_id", "trial_id", "fix_id" = "stim_cued"))%>%
    select(subject_id, stim_id, group_id, fix_id,
         prop_num, stim_recall)

######################################################################

# ALL EYE TRACKING DATA
vpn <- paste0(
    "vpja",
    ifelse(
      c(1:78,81:96)<10,
      "0",
      ""),
  c(1:78,81:96))

bed <- rep(c("free","mem"),47)

# ORIGINALLY ACQUIRED DATA
#vpn <- paste0("vpja",ifelse(c(1:78)<10,"0",""),c(1:78))
#bed <- rep(c("free","mem"),39)

bed <- bed[!(vpn %in% "vpja23")]  # missing data
vpn <- vpn[!(vpn %in% "vpja23")]

# Loop over subjects
erg <- numeric()
nvalid <- numeric()
cleantime <- numeric()

for (vp in vpn) {
  #  print(vp)
  
  prot <- read.csv2(path(path_postp_etc,paste0(vp,"_Fixations.csv")))
  
  # Restrict to trials with valid baseline?
  nvalid <- c(nvalid,sum(prot$blok==1))
  prot <- prot[prot$blok==1,]
  
  cleantime <- c(cleantime,mean(prot$cleantime))
  
  erg <- rbind(erg,apply(prot[,8:ncol(prot)],2,mean,na.rm=TRUE))
}

df.w.et <- data.frame(code=vpn,group=bed,nvalid,cleantime,erg) %>%
  mutate(
    code = as.factor(unlist(map(strsplit(as.character(code),"ja"), ~.x[2])))) %>%
  rename(vp = code)

