# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")

df_que <-
    read_delim(
        path(path_postp_fb,
             "questionnaires.csv"),
        col_types = cols(
            .default = col_double(),
            Demo_Abschluss = col_character(),
            Demo_Fach = col_character(),
            Demo_Taetigkeit = col_character(),
            Demo_Sehschwaeche = col_character(),
            ISKK_Taetigkeit = col_character(),
            EXP_1_Denken = col_character(),
            EXP_2_Aufgefallen = col_character(),
            EXP_4_Objekt = col_character(),
            EXP_5_Szene = col_character(),
            EXP_6_Anmerkung = col_character()),
        delim = "\t",
        locale = locale(grouping_mark = ".", decimal_mark = ",")) %>%

    # AQ-K
    mutate_at(
        vars(AQK_1, AQK_3, AQK_5, AQK_6, AQK_7, AQK_9, AQK_10, AQK_11, AQK_14,
             AQK_16, AQK_17, AQK_18, AQK_20, AQK_22, AQK_23, AQK_24, AQK_26, AQK_28,
             AQK_31, AQK_32, AQK_33),
        funs(5 - .)) %>% # reverse variables
    mutate_at(
        vars(AQK_1:AQK_33),
        funs(ifelse(. <= 2, 1, ifelse(. <= 4, 0, NA)))) %>% # recode variable
    mutate_at(
        vars(AQK_1:AQK_33),
        funs(ifelse(is.na(.), round(mean(., na.rm = T),0), .))) %>% # replace NAs with mean

    # ISKK
    mutate_at(
        vars(
            ISKK_09, ISKK_11, ISKK_18, ISKK_21, ISKK_23, ISKK_31,  ISKK_10,
            ISKK_32,  ISKK_03, ISKK_07, ISKK_12, ISKK_16, ISKK_25, ISKK_33,
            ISKK_17),
        funs(5- .)) %>% # reverse variables
    mutate_at(
        vars(ISKK_01:ISKK_33),
        funs(ifelse(is.na(.), round(mean(., na.rm = T),0), .))) %>% # replace NAs with mean
    transmute(
        subject_id = as.factor(
            ifelse(VP_Nr < 10,
                   paste0("0",VP_Nr),
                   VP_Nr)),
        subject_sex = as.factor(Demo_Sex),
        subject_age = Demo_Alter,
        aqk_social = (AQK_1 + AQK_7 + AQK_8 + AQK_10 + AQK_11 + AQK_13 + AQK_14
                      + AQK_20 + AQK_24 + AQK_28 + AQK_31),
        aqk_imagination = (AQK_3 + AQK_5 + AQK_6 + AQK_9 + AQK_16 + AQK_17 + AQK_18
                         + AQK_22 + AQK_23 + AQK_26 + AQK_32 + AQK_33),
        aqk_communication = (AQK_2 + AQK_4 + AQK_12 + AQK_15 + AQK_19 + AQK_21
                           + AQK_25 + AQK_27 + AQK_29 + AQK_30),
        aqk_sumscore = (aqk_social + aqk_imagination + aqk_communication),
        isk_1 = (ISKK_01 + ISKK_05 + ISKK_09 + ISKK_11 + ISKK_14 + ISKK_18 + ISKK_21
                 + ISKK_23 + ISKK_27 + ISKK_31),
        isk_2 = (ISKK_02 + ISKK_06 + ISKK_10 + ISKK_15 + ISKK_19 + ISKK_24
                 + ISKK_28 + ISKK_32),
        isk_3 = (ISKK_03 + ISKK_07 + ISKK_12 + ISKK_16 + ISKK_20 + ISKK_25
                 + ISKK_29 + ISKK_33),
        isk_4 = (ISKK_04 + ISKK_08 + ISKK_13 + ISKK_17 + ISKK_22 + ISKK_26
                 + ISKK_30))
