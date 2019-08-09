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

# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")

options(warn=2)  # Turn warnings into errors / 0=default


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

roipath <- path_prep_roi
savepath <- path_prep_save

stimuli <- dir_ls(roipath, glob = "*.png")

# Loop over stimuli
allroisize <- numeric()
for (i in 1:length(stimuli)) {
  print(basename(stimuli[i]))

  roimat <- loadroi(stimuli[i])
  roisize <- table(roimat)

  for (i in 1:5) {
    pxls <- which(roimat == i, arr.ind = TRUE)
    
    roistuff    <- mean(pxls[,2]) # pxls returns row, col (y,x)
    roistuff[2] <- min(pxls[,2])
    roistuff[3] <- max(pxls[,2])
    roistuff[4] <- mean(pxls[,1])
    roistuff[5] <- min(pxls[,1])
    roistuff[6] <- max(pxls[,1])
    
    roisize <- c(roisize, roistuff)
    
  }
  
  if (length(roisize)==35) {
    allroisize <- rbind(allroisize,as.numeric(roisize))
  } else {
    print(paste("ROI Error:",stimuli[i]," n(ROI)=",length(roisize)))
  }
}

out <- data.frame(basename(stimuli),allroisize)
names(out) <- c("stimulus",
                "background.size","gaze.size","head.size","body.size","nongaze.size",
                "background.m_x", "background.min_x", "background.max_x", "background.m_y", "background.min_y", "background.max_y",
                "gaze.m_x", "gaze.min_x", "gaze.max_x", "gaze.m_y", "gaze.min_y", "gaze.max_y",
                "head.m_x", "head.min_x", "head.max_x", "head.m_y", "head.min_y", "head.max_y",
                "body.m_x", "body.min_x", "body.max_x", "body.m_y", "body.min_y", "body.max_y",
                "nongaze.m_x", "nongaze.min_x", "nongaze.max_x", "nongaze.m_y", "nongaze.min_y", "nongaze.max_y")


write.csv2(
  out,
    paste(savepath, "/rois.csv",sep=""),
    row.names=FALSE,
    quote=FALSE)
