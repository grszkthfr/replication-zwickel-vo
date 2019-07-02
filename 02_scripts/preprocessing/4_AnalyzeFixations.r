########################################################################
# Projekt: Joint Attention & Gaze Following
# Projektarbeit Sommersemester 2017
#
# - Fixation pro ROI analysieren
#   * Fixationsdauer auf ROIs
#   * Anzahl Fixationen auf ROIs
#   * Fixationslatenz bis zur ersten Fixation auf ROI
#
# - Sakkaden analysieren
#   * Sakkaden von Person/Gesicht/Koerper auf Objekte
#
#   0 = Background
#   1 = blau: Gazed-at object (000 000 255)
#   2 = gruen: Head (000 255 000)
#   3 = rot: Body (255 000 000)
#   4 = yellow: Non-gazed-at object (255 255 000)
#

library("png")
source("02_scripts/01-libraries.R", encoding = "utf-8")
source("02_scripts/02-paths.R", encoding = "utf-8")

# Paths from script
protpath <- path_postp_stim
roipath <- path_prep_roi
etpath <- path_prep_et

options(warn=2)  # Turn warnings into errors / 0=default

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

# load PNG and generate unique ROI matric
loadroi <- function(roifilename) {
  pic <- readPNG(roifilename)

  # Generate unique ROI codes and store them in one matrix
  allmat <- round(1000000*pic[,,1]*255)+1000*round(pic[,,2]*255)+round(pic[,,3]*255)

  return(allmat)
}


# Baseline between -300 and 0 ms relative to stimulus onset
blst <- -300 # baseline start
blen <- 0 # baseline end

# Scoring range
st <- 0 # trial start
en <- 10000 # trial end

# ROIorder: BG, Face, Body, Gazed-at object, Non gazed-at object
roidef <- c(0,255000,255000000,255,255255000)

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
fixa$vp <- tolower(fixa$vp)

# Load messages (Onsets laden)
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
msg$vp <- tolower(msg$vp)
msg$trial  <- as.numeric(sub("Trial: ","",msg$trial))

# Remove irrelevant messages
msg <- msg[((1:nrow(msg)) %in% grep("Stimulus", msg$event)),]

vpn <- names(table(fixa$vp))

n_blok <- numeric()  # Number of valid trials / subject

# Loop over subjects
for (vp in vpn) {
  print(vp)

  prot <- read.csv2(path(protpath, paste(vp,".csv",sep="")))

  # Generate variables for data storage
  prot$blx  <- NA
  prot$bly  <- NA
  prot$blok <- NA

  # Saubere Fixationszeit
  prot$cleantime    <- NA

  # Cumulative fixation time
  prot$fix.face     <- NA
  prot$fix.body     <- NA
  prot$fix.gaze     <- NA
  prot$fix.nongaze  <- NA

  # Fixation number
  prot$fixn.face     <- NA
  prot$fixn.body     <- NA
  prot$fixn.gaze     <- NA
  prot$fixn.nongaze  <- NA

  # Fixation latency
  prot$fixlat.face     <- NA
  prot$fixlat.body     <- NA
  prot$fixlat.gaze     <- NA
  prot$fixlat.nongaze  <- NA

  # Sakkadenfolge
  prot$sac.pgaze     <- NA  # Person -> cued object
  prot$sac.fgaze     <- NA  # Face
  prot$sac.bgaze     <- NA  # Body
  prot$sac.pnongaze  <- NA  # Person -> uncued object
  prot$sac.fnongaze  <- NA  # Face
  prot$sac.bnongaze  <- NA  # Body
  
  fix_jg <- data.frame()
  prot_jg <- data.frame()

  # create progress bar
  pb <- txtProgressBar(min=0, max=nrow(prot), style=3)

  ###########################################
  # 1. Determine baselines
  # Generate empty data field to store data
  baseline <- numeric()  # Position (x/y)

  # Determine trial number
  vpfix <- fixa[fixa$vp==vp,]
  ntrial <- max(vpfix$trial)

  # Loop over trials to determine trial-by-trial baselines
  for (trial in 1:ntrial) {
    # Select trial data
    fixblock <- fixa[fixa$vp==vp & fixa$trial==trial,]
    msgblock <- msg[msg$vp==vp & msg$trial==trial,]

    # Determine onset (in ms)
    onset <- msgblock$time[grep("Stimulus", msgblock$event)]

    if (length(onset)==1) {
      # Subtract onset from timestamps
      fixblock$timest  <- fixblock$timest - onset
      fixblock$timeend <- fixblock$timeend - onset

      # only fixations ending before baseline start & starting before baseline end
      fixblockbl <- fixblock[fixblock$timeend>blst & fixblock$timest<blen,]
      
      # Caluculate baseline as weighted average of fixations
      if (nrow(fixblockbl)>0) {
        # Restrict fixation data to baseline
          fixblockbl$timest[1] <-
              ifelse(
                  # if first fixation start is before baseline start
                  head(fixblockbl$timest,1)<blst,
                  # use baseline start
                  blst,
                  # else use fixation start
                  head(fixblockbl$timest,1))
          
          fixblockbl$timeend[nrow(fixblockbl)] <-
              ifelse(
                  # if last fixation end is after baseline end
                  tail(fixblockbl$timeend,1)>blen,
                  blen,
                  tail(fixblockbl$timeend,1))
          
        # Calculate fixation duration
          fixblockbl$dur <-
              fixblockbl$timeend - fixblockbl$timest

        # Calculate weighted baseline coordinates
          xbl <- sum(fixblockbl$x*fixblockbl$dur)/sum(fixblockbl$dur)
          ybl <- sum(fixblockbl$y*fixblockbl$dur)/sum(fixblockbl$dur)

        # Store values
          baseline <- rbind(baseline,c(xbl,ybl))
      } else {
        # When no valid fixations are available store NA as baseline for current trial
          baseline <- rbind(baseline,c(NA,NA))
      }
    } else {  # no valid onset
        baseline <- rbind(baseline,c(NA,NA))
    }
  }

  # Determine outlier
  blxok <- outlier_remove(baseline[,1])
  blyok <- outlier_remove(baseline[,2])
  # Baseline is valid when x and y coordinates are ok (i.e. no outlier)
  blok <- as.numeric((blxok==1) & (blyok==1))

  # Testplot
  #x11()
  #plot(
  #    baseline[,1],baseline[,2],
  #    pch=16,
  #    col=c("black","red")[blok+1],
  #    xlab="x (px)",ylab="y (px)")

  # Replace outlier with average of valid baselines
  if (sum(blok)!=length(blok)) {
      baseline[blok==0,] <-
          matrix(
              apply(
                  baseline[blok==1,],
                  2,
                  mean),
              ncol=2,
              nrow=sum(blok==0),
              byrow=TRUE)
  }

  prot$blx <- baseline[,1]
  prot$bly <- baseline[,2]
  prot$blok <- blok

  # Store number of valid baselines per subject
  n_blok <- c(n_blok,sum(blok))


  ###########################################
  # 2. Score data
  for (trial in 1:ntrial) {
    setTxtProgressBar(pb, trial)

    # Select trial data
    fixblock <- fixa[fixa$vp==vp & fixa$trial==trial,]
    msgblock <- msg[msg$vp==vp & msg$trial==trial,]

    # Determine onset (in ms)
    onset  <- msgblock$time[grep("Stimulus", msgblock$event)]

    # Only proceed when valid onset if available
    if (length(onset)==1) {
      # Subtract onset from timestamps
      fixblock$timest  <- fixblock$timest - onset
      fixblock$timeend <- fixblock$timeend - onset

      # Subtract baseline
      fixblock$x <- fixblock$x - baseline[trial,1]
      fixblock$y <- fixblock$y - baseline[trial,2]

      # Extract relevant fixations
      fixblock <- fixblock[fixblock$timeend>0,]

      # Determine final set of fixations between onset and offset and
      # calculate corresponding fixation durations
      if (nrow(fixblock)>0) {
        fixblockpic <- fixblock[fixblock$timeend>st & fixblock$timest<en,]
        # Restrict last fixation to stimulus duration
        fixblockpic$timeend[nrow(fixblockpic)] <-
            ifelse(
                fixblockpic$timeend[nrow(fixblockpic)]>en,
                en,
                fixblockpic$timeend[nrow(fixblockpic)])
        
        # Restict first fixatio to stimulus onset not necessary because first fixation will be removed

        # Reference times to stimulus onset
        fixblockpic$timest <- fixblockpic$timest-st
        fixblockpic$timeend <- fixblockpic$timeend-st

        # Fixationen den ROIs zuordnen
        fixblockpic$duration <- fixblockpic$timeend-fixblockpic$timest
        ROImat <- loadroi(path(roipath,paste(prot$pic[trial],"_ROI.png",sep="")))

        # Auf Bildmitte zentrieren
        xm <- ncol(ROImat)/2
        ym <- nrow(ROImat)/2

        fixblockpic$roi <- NA
        for (fixnr in 1:nrow(fixblockpic)) {
          fx <- round(fixblockpic$x[fixnr] + xm)
          fy <- round(fixblockpic$y[fixnr] + ym)
          if (fx>=1 & fx<=ncol(ROImat) & fy>=1 & fy<=nrow(ROImat)) {
            fixblockpic$roi[fixnr] <- ROImat[fy,fx]
          }
        }

        #########################################
        # Sakkaden: Fixationsfolgen
        #   Face -> gazed-at object
        #   Face -> non gazed-at onject
        #   relativiert an allen Sakkaden die das Gesicht verlassen
        # Erst gesamte Matrix erzeugen:

        sacmat <- matrix(0,nrow=length(roidef),ncol=length(roidef))
        # ROIorder: BG, Face, Body, Gazed-at object, Non gazed-at object

        for (i in 1:(nrow(fixblockpic)-1)) {
          if (!is.na(fixblockpic$roi[i]) & !is.na(fixblockpic$roi[i+1])) {
              sacmat[roidef==fixblockpic$roi[i],roidef==fixblockpic$roi[i+1]] <-
                  sacmat[roidef==fixblockpic$roi[i],roidef==fixblockpic$roi[i+1]] + 1
          }
        }

        # Cued object (Column 4)
        prot$sac.pgaze[prot$trial==trial]     <-
            (sacmat[2,4]+sacmat[3,4])/sum((apply(sacmat,1,sum))[2:3])
        prot$sac.fgaze[prot$trial==trial]     <-
            sacmat[2,4]/(apply(sacmat,1,sum))[2]
        prot$sac.bgaze[prot$trial==trial]     <-
            sacmat[3,4]/(apply(sacmat,1,sum))[3]
        # Uncued object (Column 5)
        prot$sac.pnongaze[prot$trial==trial]  <-
            (sacmat[2,5]+sacmat[3,5])/sum((apply(sacmat,1,sum))[2:3])
        prot$sac.fnongaze[prot$trial==trial]  <-
            sacmat[2,5]/(apply(sacmat,1,sum))[2]
        prot$sac.bnongaze[prot$trial==trial]  <-
            sacmat[3,5]/(apply(sacmat,1,sum))[3]

        # Score fixation data
        #   1 = blau: Gazed-at object (000 000 255)
        #   2 = gruen: Face (000 255 000)
        #   3 = rot: Body (255 000 000)
        #   4 = yellow: Non-gazed-at object (255 255 000)

        # Remove first fixation
        if ((nrow(fixblockpic)>1) & (fixblockpic$timest[1]<0)) {
          fixblockpic <- fixblockpic[2:nrow(fixblockpic),]
        }
        
        # before any scoring save raw fixations
        fix_jg <- rbind(fix_jg, fixblockpic)
        prot_jg <- cbind(vp, prot[1:6])

        prot$cleantime[prot$trial==trial] <- sum(fixblockpic$duration,na.rm=TRUE)

        #   rot: Body (255 000 000)
        prot$fix.body[prot$trial==trial] <-
            sum(fixblockpic$duration[fixblockpic$roi==255000000],na.rm=TRUE)/
            sum(fixblockpic$duration,na.rm=TRUE)
        prot$fixn.body[prot$trial==trial] <-
            sum(fixblockpic$roi==255000000,na.rm=TRUE)/
            nrow(fixblockpic)
        iroi <- (1:nrow(fixblockpic))[fixblockpic$roi==255000000]
        iroi <- iroi[!is.na(iroi)]
        prot$fixlat.body[prot$trial==trial] <- fixblockpic$timest[iroi[1]]

        #   gruen: Head (000 255 000)
        prot$fix.face[prot$trial==trial] <-
            sum(fixblockpic$duration[fixblockpic$roi==255000],na.rm=TRUE)/
            sum(fixblockpic$duration,na.rm=TRUE)
        prot$fixn.face[prot$trial==trial] <-
            sum(fixblockpic$roi==255000,na.rm=TRUE)/
            nrow(fixblockpic)
        iroi <- (1:nrow(fixblockpic))[fixblockpic$roi==255000]
        iroi <- iroi[!is.na(iroi)]
        prot$fixlat.face[prot$trial==trial] <- fixblockpic$timest[iroi[1]]

        #   blau: Gazed at object (000 000 255)
        prot$fix.gaze[prot$trial==trial] <-
            sum(fixblockpic$duration[fixblockpic$roi==255],na.rm=TRUE)/
            sum(fixblockpic$duration,na.rm=TRUE)
        prot$fixn.gaze[prot$trial==trial] <-
            sum(fixblockpic$roi==255,na.rm=TRUE)/
            nrow(fixblockpic)
        iroi <- (1:nrow(fixblockpic))[fixblockpic$roi==255]
        iroi <- iroi[!is.na(iroi)]
        prot$fixlat.gaze[prot$trial==trial] <- fixblockpic$timest[iroi[1]]

        #   gelb: Non-gazed at object (255 255 000)
        prot$fix.nongaze[prot$trial==trial] <-
            sum(fixblockpic$duration[fixblockpic$roi==255255000],na.rm=TRUE)/
            sum(fixblockpic$duration,na.rm=TRUE)
        prot$fixn.nongaze[prot$trial==trial] <-
            sum(fixblockpic$roi==255255000,na.rm=TRUE)/
            nrow(fixblockpic)
        iroi <- (1:nrow(fixblockpic))[fixblockpic$roi==255255000]
        iroi <- iroi[!is.na(iroi)]
        prot$fixlat.nongaze[prot$trial==trial] <- fixblockpic$timest[iroi[1]]

        # Testplot
        # Nur Fixationen
        #x11()
        #plot(
        #     fixblockpic$x+xm,ym-fixblockpic$y,
        #     cex=fixblockpic$duration/100,asp=1,
        #     xlim=c(1,ncol(ROImat)),ylim=c(1,nrow(ROImat)))
        # points(xm,ym,pch="+",col="red")

        # Fixationen als Overlay ueber ROIs
        # Recode colors
        #ROImat[ROImat==255000000] <- 1
        #ROImat[ROImat==255000] <- 2
        #ROImat[ROImat==255255000] <- 3
        #ROImat[ROImat==255] <- 4
        #x11()
        #image(
        #    1:ncol(ROImat),1:nrow(ROImat),
        #    t(ROImat)[,nrow(ROImat):1]/max(ROImat),asp=1)
        #points(
        #    fixblockpic$x+xm,ym-fixblockpic$y,
        #    pch=1,cex=fixblockpic$duration/100)
      }
    }
  
  # Save results (Jonas)
  write.csv2(
      fix_jg,
      path(path_prep_save, "fixations_raw", paste(vp,"_Fixations-raw.csv",sep="")),
      row.names=FALSE,
      quote=FALSE)
  
  # Save protocols (Jonas)
  write.csv2(
    prot_jg,
    path(path_prep_save, "protocols", paste(vp,"_protocol.csv",sep="")),
    row.names=FALSE,
    quote=FALSE)
  
  }

  close(pb)

  # Save results (Matthias)
  write.csv2(
      prot,
      path(path_prep_save, "fixations_collapsed", paste(vp,"_Fixations.csv",sep="")),
      row.names=FALSE,
      quote=FALSE)



  
}
