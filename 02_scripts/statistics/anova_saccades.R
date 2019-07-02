# ANOVA Saccades
## 2 (Group) x 2 (Gaze) ANOVA

icond <- gl(2,1,labels=c("cued","uncued")) # within-factor
idata <- data.frame(icond)

for (st in 17:19) {
    carmod <- lm(as.matrix(df.w.et[,c(st,(st+3))]) ~ df.w.et$group)
    #print(colnames(df.w.et[,c(st,(st+3))]))
    #print(Anova(carmod, idata=idata, idesign=~icond, type="III"))
    assign(paste0("anova.",colnames(df.w.et)[st]),
           Anova(carmod, idata=idata, idesign=~icond, type="III"))
    # assign(paste0("postHoc.",colnames(df.w.et)[st]),
    #        lsmeans(carmod, specs = c(names(carmod$model[2]), "rep.meas")))
    assign(paste0("apa.anova.",colnames(df.w.et)[st]),
           apa_print(Anova(carmod, idata=idata, idesign=~icond, type="III"),
                     correction="GG", mse = FALSE))
}

rm(carmod, idata, icond, st)
