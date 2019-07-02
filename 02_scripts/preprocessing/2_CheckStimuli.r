###############################################################################
# Projekt: Joint Attention & Gaze Following
# Projektarbeit Sommersemester 2017
#
# Stimuli zwischen Protokoll und Eyelink-Datei abgleichen

source("02_scripts/01-libraries.R", encoding = "utf-8")
source("02_scripts/02-paths.R", encoding = "utf-8")

protpath <- path_postp_stim
etpath <- path_prep_et

######## Process ET Data
# Load fixation list
fixa <- read.table(path(etpath, "Fixations.txt"),skip=1,dec=",",na.strings=".")
# Cosmetics: Remove irrelevant column 2 and add column names
fixa <- fixa[,c(1,3:7)]
names(fixa) <- c("vp","trial","timest","timeend","x","y")

# Onsets laden
msg <-
    read.table(
        path(etpath,"Messages.txt"),
        skip=1,
        dec=",",
        sep="\t",
        na.strings=".",
        colClasses=c("character","character","numeric","character"))

# Cosmetics
names(msg) <- c("vp","trial","time","event")
msg$trial  <- as.numeric(sub("Trial: ","",msg$trial))

# Determine which subjects should be analyzed
vpn <- paste("vpja",ifelse(c(1:78,81:96)<10,"0",""),c(1:78,81:96),sep="")
vpn <- vpn[vpn!="vpja23"]   # vp23 gibt es nicht

# Loop over subjects
alltrials <- numeric()
for (vp in vpn) {
  code <- vp
  print(code)

  # Determine trial number
  vpfix <- fixa[tolower(fixa$vp)==code,]
  ntrial <- max(vpfix$trial)
  alltrials <- c(alltrials,ntrial)

  prot <- read.csv2(path(protpath,paste(vp,".csv",sep="")))

  # Loop over trials to determine trial-by-trial baselines
  for (trial in 1:ntrial) {
    # Select trial data
    msgblock <- msg[tolower(msg$vp)==code & msg$trial==trial,]

    # Determine onset (in ms)
    eventmrk <- msgblock$event[grep("Stimulus", msgblock$event)]

    if (eventmrk!=paste("Stimulus ",prot$pic[trial],".jpg",sep="")) {
      print(paste("ERR:",vp,trial))
    }
  }
}
