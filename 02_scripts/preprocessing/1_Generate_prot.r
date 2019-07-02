###############################################################################
# Projekt: Joint Attention & Gaze Following
# Projektarbeit Sommersemester 2017
#
# Protokolldateien erzeugen

source("02_scripts/02-paths.R", encoding = "utf-8")

path <- path_prep_log
seqpath <- path_prep_seq
savepath <- path_prep_save

vpn <- paste("vpja",ifelse(c(1:78,81:96)<10,"0",""),c(1:78,81:96),sep="")
vpn <- vpn[vpn!="vpja23"]   # vp23 gibt es nicht

#names(daten)
# [1] "Subject"       "Trial"         "Event.Type"
# [4] "Code"          "Time"          "TTime"
# [7] "Uncertainty"   "Duration"      "Uncertainty.1"
#[10] "ReqTime"       "ReqDur"

for (vp in vpn) {
  print(vp)

  ###########################
  # Stimulus presentation
  seqdat <- read.table(path(seqpath,paste(vp,".txt",sep="")),header=TRUE)
  seqdat$pic <- as.character(seqdat$pic)
  seqdat$trial <- NA

  #names(logdat)
  # [1] "Subject"       "Trial"         "Event.Type"
  # [4] "Code"          "Time"          "TTime"
  # [7] "Uncertainty"   "Duration"      "Uncertainty.1"
  #[10] "ReqTime"       "ReqDur"

  logdat <-
      read.table(
          path(path,paste(vp,"-PicPresentation.log",sep="")),
          sep="\t",
          skip=3,
          header=TRUE,
          fill=TRUE)
  logdat$Code <- as.character(logdat$Code)
  logdat$Event.Type <- as.character(logdat$Event.Type)

  # Check consistency of images between seq- and log-files
  bild <-
      (1:nrow(logdat))[(logdat$Event.Type=="Picture") & (logdat$Code!="fixation")]
  if (
      sum(substr(logdat$Code[bild],1,nchar(logdat$Code[bild])-4)==seqdat$pic)
      !=nrow(seqdat)) {
    print(paste("Wrong pictures - Stimulation:",vp))
  }

  # Trialnummer einfuegen und ITIs berechnen
  seqdat$trial <- 1:length(bild)
  seqdat$iti <- c(diff(logdat$Time[bild])/10,0)

  # Trialnummer an den Anfang stellen
  seqdat <- seqdat[,c(3,1:2)]
  print(path(savepath,paste(vp,".csv",sep="")))
  # Daten speichern
  write.csv2(
      seqdat,
      path(savepath, "stimuli", paste(vp,".csv",sep="")),
      na="",
      row.names=FALSE,
      quote=FALSE)
}
