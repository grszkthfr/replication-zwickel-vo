# Recalled items
# 2 (Condition) x 2 (Gaze) ANOVA (auf erinnerte Details)

imem <- gl(2,1,labels=c("cued","uncued"))
idata <- data.frame(imem)

# Car
carmod <- lm(as.matrix(df.w.mem[,3:4]) ~ df.w.mem$bed)
#print(colnames(df.w.mem[c(3,4)]))
#print(Anova(carmod, idata=idata, idesign=~imem, type="III"))
anova.mem <- Anova(carmod, idata=idata, idesign=~imem, type="III")
apa.anova.mem <- apa_print(Anova(carmod, idata=idata,
                                 idesign=~imem, type="III"),
                           correction="GG", mse = FALSE)


rm(imem, idata, carmod)
