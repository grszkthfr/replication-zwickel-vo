# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")

df_mem <-
    dir_ls(path_postp_mem, glob = "*.csv") %>%
    map_df(
        ~{

            df_mem_subject <- read_delim(
                .,
                col_types = cols(
                    pic = col_character(),
                    iti = col_double(),
                    gaze = col_character(),
                    gazedat = col_character(),
                    memgazedat = col_double(),
                    nongazedat = col_character(),
                    memnongazedat = col_double()),
                delim = ";",
                locale = locale(
                    decimal_mark = ",",
                    grouping_mark = "."))

            df_mem_subject$subject_id <- basename(.) %>%
                str_extract("[[:digit:]]+")

            df_mem_subject$trial_id <- 1:nrow(df_mem_subject)

            df_mem_subject

        }) %>%
    mutate(
        group_id = case_when(
            as.numeric(subject_id) %% 2 == 0 ~ "mem",
            as.numeric(subject_id) %% 2 != 0 ~ "free"),
        stim_id = str_extract(pic, "[0-9]+") %>% ifelse(str_count(.)<2, paste0(0, .), .),
        stim_orientation = str_extract(pic, "[aA-zZ]+")) %>%
    unite(object_cued, c("gazedat", "memgazedat")) %>%
    unite(object_uncued, c("nongazedat", "memnongazedat")) %>%
    gather(stim_cued, tmp, object_cued:object_uncued) %>%
    separate(tmp, c("stim_object", "stim_recall"), sep="_") %>%
    arrange(subject_id, trial_id) %>%
    select(subject_id, trial_id, stim_id, stim_orientation,
           iti, group_id, stim_cued, stim_object, stim_recall) %>%
  mutate(
      stim_object = str_trim(stim_object),
      stim_object = ifelse(
          stim_object == "Kaffemuehle", "Kaffeemuehle", stim_object)) %>%
  mutate_at(
      vars(stim_id:stim_object, -iti),
      list(as.factor)) %>%
  mutate(
      stim_cued = fct_relevel(
        stim_cued,
        c("object_cued", "object_uncued")),
      stim_recall = fct_recode(stim_recall, recalled = "1", not_recalled = "0"))


##############################################################################

## # ALL MEMORY DATA
## vpn <- paste0("vpja",ifelse(c(1:78,81:96)<10,"0",""),c(1:78,81:96))
## bed <- rep(c("free","mem"),47)

## # Loop over subjects
## erg <- numeric()
## for (vp in vpn) {
##   # print(vp)

##   prot <- read.csv2(path(path_postp_mem, paste0(vp,".csv")))

##   # Item recalled
##   gaze <- sum(prot$memgazedat)
##   nogaze <- sum(prot$memnongazedat)

##   erg <- rbind(erg,c(gaze,nogaze))
## }

## df.w.mem <- data.frame(code=vpn,bed,erg) %>%
##   mutate(
##     code = as.factor(unlist(map(strsplit(
##       as.character(code),"ja"), ~.x[2])))) %>%
##   rename(vp = code)

## names(df.w.mem) <- c("code","bed","memgaze","memnogaze")
