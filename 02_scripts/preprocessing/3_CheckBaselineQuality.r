###############################################################################
# Projekt: Joint Attention & Gaze Following
# Projektarbeit Sommersemester 2017
#
# Score baselines and determine baseline quality

source("02_scripts/02-paths.R", encoding = "utf-8")


# Iterative outlier removal
outlier_remove <- function(x,sdmult=3) {
  # Remove outlier iteratively
  insample <- rep(1,length(x)); insample[is.na(x)] <- 0
  ok <- FALSE
  while (!ok) {
    xminpos <- (1:length(x))[x==min(x[insample==1])]
    xminpos <- xminpos[xminpos %in% (1:length(insample))[insample==1]][1]
    xmaxpos <- (1:length(x))[x==max(x[insample==1])]
    xmaxpos <- xmaxpos[xmaxpos %in% (1:length(insample))[insample==1]][1]
    tempinsample <- insample; tempinsample[c(xminpos,xmaxpos)] <- 0
    subx <- x[tempinsample==1]
    if (x[xminpos]<(mean(subx)-sdmult*sd(subx))) {
      insample[xminpos] <- 0
      out1 <- TRUE
    } else {
      out1 <- FALSE
    }

    if (x[xmaxpos]>(mean(subx)+sdmult*sd(subx))) {
      insample[xmaxpos] <- 0
      out2 <- TRUE
    } else {
      out2 <- FALSE
    }

    if (!out1 & !out2) { ok <- TRUE }
  }
  return(insample)
}

etpath <- path_prep_et

# Baseline between -300 and 0 ms relative to stimulus onset
blst <- -300; blen <- 0

######## Process ET Data
# Load fixation list
fixa <-
    read.table(
        path(etpath,"Fixations.txt"),
        skip=1,
        dec=",",
        na.strings=".")
# Cosmetics: Remove irrelevant column 2 and add column names
fixa <- fixa[,c(1,3:7)]
names(fixa) <- c("vp","trial","timest","timeend","x","y")

# Onsets laden
msg <- read.table(
    path(etpath,"Messages.txt"),
    skip=1,dec=",",
    sep="\t",
    na.strings=".",
    colClasses=c("character","character","numeric","character"))
# Cosmetics
names(msg) <- c("vp","trial","time","event")
msg$trial  <- as.numeric(sub("Trial: ","",msg$trial))

# Determine which subjects should be analyzed
vpn <- paste("vpja",ifelse(c(1:78,81:96)<10,"0",""),c(1:78,81:96),sep="")
vpn <- vpn[vpn!="vpja23"]   # vp23 gibt es nicht

n_blok <- numeric()  # Number of valid trials / subject
spread <- numeric()  # Breite der Verteilung der validen Baselines
                     # (fuer grobe Abweichungen)

# Loop over subjects
for (vp in vpn) {
  code <- vp
  print(code)

  ###########################################
  # 1. Determine baselines
  # Generate empty data field to store data
  baseline <- numeric()  # Position (x/y)

  # Determine trial number
  vpfix <- fixa[tolower(fixa$vp)==code,]
  ntrial <- max(vpfix$trial)

  # Loop over trials to determine trial-by-trial baselines
  for (trial in 1:ntrial) {
    # Select trial data
    fixblock <- fixa[tolower(fixa$vp)==code & fixa$trial==trial,]
    msgblock <- msg[tolower(msg$vp)==code & msg$trial==trial,]

    # Determine onset (in ms)
    onset <- msgblock$time[grep("Stimulus", msgblock$event)]

    # Subtract onset from timestamps
    fixblock$timest  <- fixblock$timest-onset
    fixblock$timeend <- fixblock$timeend-onset

    # Caluculate baseline as weighted average of fixations
    fixblockbl <- fixblock[fixblock$timeend>blst & fixblock$timest<blen,]
    if (nrow(fixblockbl)>0) {
      # Restrict fixation data to baseline
        fixblockbl$timest[1] <-
            ifelse(
                head(fixblockbl$timest,1)<blst,
                blst,
                head(fixblockbl$timest,1))
        fixblockbl$timeend[nrow(fixblockbl)] <-
            ifelse(
                tail(fixblockbl$timeend,1)>blen,
                blen,
                tail(fixblockbl$timeend,1))
        fixblockbl$dur <- fixblockbl$timeend - fixblockbl$timest

      # Calculate baseline coordinates
        xbl <- sum(fixblockbl$x*fixblockbl$dur)/sum(fixblockbl$dur)
        ybl <- sum(fixblockbl$y*fixblockbl$dur)/sum(fixblockbl$dur)

      # Store values
        baseline <- rbind(baseline,c(xbl,ybl))
    } else {
      # When no valid fixations are available store NA as baseline for
      # current trial
      baseline <- rbind(baseline,c(NA,NA))
    }
  }

  # Testplot
  # x11()
  #plot(
  #    baseline[,1],baseline[,2],
  #    pch=16,
  #    col="black",
  #    xlab="x (px)",ylab="y (px)")

  # Determine outlier
  blxok <- outlier_remove(baseline[,1])
  blyok <- outlier_remove(baseline[,2])
  # Baseline is valid when x and y coordinates are ok (i.e. no outlier)
  blok <- as.numeric((blxok==1) & (blyok==1))

  # Store number of valid baselines per subject
  n_blok <- rbind(n_blok,c(nrow(baseline),sum(blok)))

  # Store spread of baselines in x and y direction
  xrng <- max(baseline[blok==1,1])-min(baseline[blok==1,1])
  yrng <- max(baseline[blok==1,2])-min(baseline[blok==1,2])
  spread <- rbind(spread,c(xrng,yrng))
}

erg <- data.frame(vpn,n_blok,spread)
names(erg) <- c("code","alltrials","blok","xrng","yrng")

write.csv2(
    erg,
    path(path_postp,"Results_BaselineCheck.csv"),
    row.names=FALSE,
    quote=FALSE)
