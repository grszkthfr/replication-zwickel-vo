###############################################################################
# Projekt: Joint Attention & Gaze Following
# Projektarbeit Sommersemester 2017
#
# - ROIs pruefen:
#   nur 5 Farben enthalten?
#   0 = Background
#   1 = blau: Gazed-at object (000 000 255)
#   2 = gruen: Head (000 255 000)
#   3 = rot: Body (255 000 000)
#   4 = yellow: Non-gazed-at object (255 255 000)

rm(list=ls())

options(warn=2)  # Turn warnings into errors / 0=default

require(png)

# load PNG and generate unique ROI matric
loadroi <- function(roifilename) {
  pic <- readPNG(roifilename)

  # Generate unique ROI codes and store them in one matrix
  allmat <-
      1000000*round(pic[,,1]*255)+1000*round(pic[,,2]*255)+round(pic[,,3]*255)
  roitab <- table(allmat)
  roinr <- length(roitab)
  roival <- as.numeric(names(roitab))

  roimat <- matrix(0,nrow=nrow(allmat),ncol=ncol(allmat))
  for (i in 1:roinr) {
    roimat[allmat==roival[i]] <- i
  }

  return(roimat)
}

roipath <- "/Users/gamer/Documents/UKE/Lehre/2017_SS/Projektarbeit/Experiment/Data/ROI/png/"
savepath <- "/Users/gamer/Documents/UKE/Lehre/2017_SS/Projektarbeit/Experiment/Data/ROI/"

stimuli <- dir(roipath,"*.png")

# Loop over stimuli
allroisize <- numeric()
for (i in 1:length(stimuli)) {
  print(stimuli[i])

  roimat <- loadroi(paste(roipath,stimuli[i],sep=""))
  roisize <- table(roimat)

  if (length(roisize)==5) {
    allroisize <- rbind(allroisize,as.numeric(roisize))
  } else {
    print(paste("ROI Error:",stimuli[i]," n(ROI)=",length(roisize)))
  }
}

out <- data.frame(stimuli,allroisize)
names(out) <- c("stimulus","background","gaze","head","body","nongaze")
write.csv2(
    out,
    paste(savepath,"roisizes.csv",sep=""),
    row.names=FALSE,
    quote=FALSE)
