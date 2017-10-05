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

#options(warn=0)

rm(list=ls())

options(warn=0)  # 2=Turn warnings into errors / 0=default

path <- "C:/Users/jog54yy/Documents/Projektarbeit_SS2017/Data/prot/"
savepath <- "/Users/gamer/Documents/UKE/Lehre/2017_SS/Projektarbeit/Experiment/Auswertung/"

# ALL DATA
vpn <- paste("vpja",ifelse(c(1:78,81:96)<10,"0",""),c(1:78,81:96),sep="")
bed <- rep(c("free","mem"),47)

# ORIGINALLY ACQUIRED DATA
#vpn <- paste("vpja",ifelse(c(1:78)<10,"0",""),c(1:78),sep="")
#bed <- rep(c("free","mem"),39)

bed <- bed[!(vpn %in% "vpja23")]  # Daten fehlen
vpn <- vpn[!(vpn %in% "vpja23")]  

# Loop over subjects
erg <- numeric(); nvalid <- numeric(); cleantime <- numeric()
for (vp in vpn) {
#  print(vp)
  
  prot <- read.csv2(paste(path,vp,"_Fixations.csv",sep=""))

  # Restrict to trials with valid baseline?
  nvalid <- c(nvalid,sum(prot$blok==1))
  prot <- prot[prot$blok==1,]
  
  cleantime <- c(cleantime,mean(prot$cleantime))
  
  erg <- rbind(erg,apply(prot[,8:ncol(prot)],2,mean,na.rm=TRUE))
}

out <- data.frame(code=vpn,group=bed,nvalid,cleantime,erg)               
#write.csv2(out,paste(savepath,"EyeMovements.csv",sep=""),row.names=FALSE,quote=FALSE)

####################################################
# Plots: Fixation data
x11()
par(mfrow=c(2,3))

farben <- c("green","red","blue","yellow")

for (st in seq(5,16,4)) {
  # Fixation density
  m  <- cbind(apply(out[out$group=="free",st:(st+3)],2,mean),
              apply(out[out$group=="mem",st:(st+3)],2,mean))
  se <- cbind(apply(out[out$group=="free",st:(st+3)],2,sd)/sqrt(sum(out$group=="free")),
              apply(out[out$group=="mem",st:(st+3)],2,sd)/sqrt(sum(out$group=="mem")))

  yrng <- c(0,max(m+se))
  
  x <- barplot(m,beside=TRUE,col=farben,ylim=yrng,xlab="",ylab="% of total fixation time")
  arrows(x,m-se,x,m+se,length=0.03,angle=90,code=3,col="black")
  axis(1,apply(x,2,mean),c("Free viewing","Explicit encoding"),tick=FALSE)
}

legend(max(x),max(yrng),c("Face","Body","Cued","Uncued"),fill=farben,xjust=1,yjust=1)

# Plots: Saccade data
farben <- c("blue","yellow")
ttxt <- c("Person","Face","Body")

for (i in 1:3) {
  st <- (17:19)[i]
  
  # Fixation density
  m  <- cbind(apply(out[out$group=="free",c(st,st+3)],2,mean),
              apply(out[out$group=="mem",c(st,st+3)],2,mean))
  se <- cbind(apply(out[out$group=="free",c(st,st+3)],2,sd)/sqrt(sum(out$group=="free")),
              apply(out[out$group=="mem",c(st,st+3)],2,sd)/sqrt(sum(out$group=="mem")))
  
  yrng <- c(0,max(m+se))
  
  x <- barplot(m,beside=TRUE,col=farben,ylim=yrng,xlab="",ylab="% of total fixation time")
  arrows(x,m-se,x,m+se,length=0.03,angle=90,code=3,col="black")
  axis(1,apply(x,2,mean),c("Free viewing","Explicit encoding"),tick=FALSE)
  
  title(ttxt[i])
}

legend(max(x),max(yrng),c("Cued","Uncued"),fill=farben,xjust=1,yjust=1)

####################################################
# Statistics
require("car")

# Fixations Face + Body in Free Viewing vs. Explicit Encoding
ergtab <- numeric()
for (st in seq(5,16,4)) {
  msd <- c(mean(out[out$group=="free",st]),sd(out[out$group=="free",st]),
           mean(out[out$group=="mem",st]),sd(out[out$group=="mem",st]))
  teststat <- t.test(out[,st] ~ out$group)
  
  ergtab <- rbind(ergtab,c(msd,teststat$parameter,teststat$statistic,teststat$p.value))
}

colnames(ergtab) <- c("M(free)","SD(free)","M(mem)","SD(mem)","df","t","p")
print(ergtab)

# ANOVA Fixations characteristics
# 2 (Group) x 2 (Gaze) ANOVA
icond <- gl(2,1,labels=c("cued","uncued"))
idata <- data.frame(icond)

for (st in seq(7,16,4)) {
  carmod <- lm(as.matrix(out[,st:(st+1)]) ~ out$group)
  print(Anova(carmod, idata=idata, idesign=~icond, type="III"))
}

# ANOVA Saccades
# 2 (Group) x 2 (Gaze) ANOVA
for (st in 17:19) {
  carmod <- lm(as.matrix(out[,c(st,st+3)]) ~ out$group)
  print(Anova(carmod, idata=idata, idesign=~icond, type="III"))
}

