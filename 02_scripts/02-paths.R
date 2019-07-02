
# source("02_scripts/01-libraries.R", encoding = "utf-8")
library(fs)

# paths scripts
path_scripts <- path(here(), "02_scripts")
path_scripts_data <- path(path_scripts, "processing_data")
path_scripts_publ <- path(path_scripts, "publishing")
path_scripts_stat <- path(path_scripts, "statistics")
path_scripts_fun <- path(path_scripts, "functions")


# paths preprocessing
path_prep_log <- path(here(), "01_data/pre_processing/Presentation_log/")
path_prep_roi <- path(here(), "01_data/pre_processing/ROI/png/")
path_prep_seq <- path(here(), "01_data/pre_processing/Presentation_experiment/sequences/")
path_prep_et <- path(here(), "01_data/pre_processing/EyeLink_reports/")
path_prep_save <- path(here(), "01_data/prot/")

# paths postprocessing
path_postp_stim <- path(here(), "01_data/prot/stimuli/")
path_postp_mem <- path(here(), "01_data/pre_processing/Memory/")
path_postp_etc <- path(here(), "01_data/prot/fixations_collapsed/")
path_postp_etr <- path(here(), "01_data/prot/fixations_raw/")
path_postp_fb <- path(here(), "01_data/pre_processing/Questionnaires/")
path_postp <- path(here(), "01_data/")
