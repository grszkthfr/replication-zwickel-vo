########################################################
# Projekt: Joint Attention & Gaze Following
# Projektarbeit Sommersemester 2017

# Frageboegen auswerten:
# AQ-K
# ISK-K

rm(list=ls())

source("/Users/gamer/Documents/UKE/R/funktionen.r")

path <- "/Users/gamer/Documents/UKE/Lehre/2017_SS/Projektarbeit/Experiment/Data/FB/"
savepath <- "/Users/gamer/Documents/UKE/Lehre/2017_SS/Projektarbeit/Experiment/Auswertung/"

# Missings checken und durch Mittelwerte aller uebrigen Probanden ersetzen
check_missing <- function(x) {
  nan <- apply(x,1,function(x) { sum(is.na(x)) })
  if (sum(nan!=0)!=0) {
    print(paste("Missing values in",sum(nan!=0),"case(s)"))
    print(nan[nan!=0])
  } else {
    print("All cases complete")
  }
  
  # Missings durch MW aller Probanden ersetzen
  for (i in 1:ncol(x)) {
    if (sum(is.na(x[,i]))!=0) {
      x[is.na(x[,i]),i] <- round(mean(x[,i],na.rm=TRUE))
    }
  }
  
  return(x)
}

# AQ-K
rawdat <- read.csv2(paste(path,"AQ-K.csv",sep=""),header=TRUE)
# Items 1,3,5,6,7,9,10,11,14,16,17,18,20,22,23,24,26,28,31,32,33 umpolen (4-item).
rawdat[,1+c(1,3,5,6,7,9,10,11,14,16,17,18,20,22,23,24,26,28,31,32,33)] <- 5-rawdat[,1+c(1,3,5,6,7,9,10,11,14,16,17,18,20,22,23,24,26,28,31,32,33)]
# Missings?
rawdat <- check_missing(rawdat)
# Items rekodieren (1/2 -> 1, 3/4 -> 0)
rawdat_recode <- matrix(as.numeric(rawdat[,2:ncol(rawdat)]<=2),nrow=nrow(rawdat))

aq1 <- apply(rawdat_recode[,c(1,7,8,10,11,13,14,20,24,28,31)],1,sum)    # Soziale Interaktion und Spontaneitaet
aq2 <- apply(rawdat_recode[,c(3,5,6,9,16,17,18,22,23,26,32,33)],1,sum)  # Fantasie und Vorstellungsvermoegen
aq3 <- apply(rawdat_recode[,c(2,4,12,15,19,21,25,27,29,30)],1,sum)      # Kommunikation und Reziprozitaet
aq  <- apply(rawdat_recode,1,sum)                                       # Gesamtsummenwert (AQ-K-Score) zwischen 0 und 33


# ISK-K
#Vier Subskalen:
#  1. Soziale Orientierung
#Items: 1, 5, 9*, 11*, 14, 18*, 21*, 23*, 27, 31*
#  2. Offensivität
#Items: 2, 6, 10*, 15, 19, 24, 28, 32*
#  3. Selbststeuerung
#Items: 3*, 7*, 12*, 16*, 20, 25*, 29, 33*
#  4. Reflexibilität
#Items: 4, 8, 13, 17*, 22, 26, 30

rawdat <- read.csv2(paste(path,"ISK-K.csv",sep=""),header=TRUE)
# Items 9,11,18,21,23,31, 10,32, 3,7,12,16,25,33, 17 umpolen (4-item).
rawdat[,1+c(9,11,18,21,23,31, 10,32, 3,7,12,16,25,33, 17)] <- 5-rawdat[,1+c(9,11,18,21,23,31, 10,32, 3,7,12,16,25,33, 17)]
# Missings?
rawdat <- check_missing(rawdat)

isk1 <- apply(rawdat[,1+c(1, 5, 9, 11, 14, 18, 21, 23, 27, 31)],1,sum) # Soziale Orientierung
isk2 <- apply(rawdat[,1+c(2, 6, 10, 15, 19, 24, 28, 32)],1,sum)        # Offensivitaet
isk3 <- apply(rawdat[,1+c(3, 7, 12, 16, 20, 25, 29, 33)],1,sum)        # Selbststeuerung
isk4 <- apply(rawdat[,1+c(4, 8, 13, 17, 22, 26, 30)],1,sum)            # Reflexibilitaet

erg <- data.frame(rawdat$code,aq1,aq2,aq3,aq,isk1,isk2,isk3,isk4)

write.csv2(erg,paste(savepath,"FB_Sample.csv",sep=""),row.names=FALSE,quote=FALSE,na="")


#################################################
# Basic statistics
# 
print(cor.prob(erg[,2:ncol(erg)]))
