# Fixations Face in Free Viewing vs. Explicit Encoding

for (st in seq(5,16,4)) {
    msd <- c(mean(df.w.et[df.w.et$group=="free",st]),
             sd(df.w.et[df.w.et$group=="free",st]),
             mean(df.w.et[df.w.et$group=="mem",st]),
             sd(df.w.et[df.w.et$group=="mem",st]))
    teststat <- t.test(df.w.et[,st] ~ df.w.et$group)
    
    assign(paste0("ttest.",colnames(df.w.et)[st]),
           data.frame(M.free=msd[1],SD.free=msd[2], M.mem=msd[3], SD.mem=msd[4],
                      df=teststat$parameter, t=teststat$statistic,
                      p=teststat$p.value))
    
    assign(paste0("apa.ttest.",colnames(df.w.et)[st]),
           apa_print(t.test(df.w.et[,st] ~ df.w.et$group)))
}

rm(msd, teststat, st)
