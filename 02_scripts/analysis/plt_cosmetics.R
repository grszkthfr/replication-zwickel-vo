

cTheme = theme(
    title = element_text(
        size = 8,
        #family = "Times New Roman", #excluded due to warnings
        color = "black"),
    text=element_text(
        size=10,
        #family = "Times New Roman", #excluded due to warnings
        color = "black"),
    axis.title=element_text(
        size=12,
        #family = "Times New Roman", #excluded due to warnings
        color = "black"),
    axis.line = element_line(
        colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
        size = .01,
        colour = "grey"),
    panel.background = element_blank(),
    legend.text = element_text(
        size=12,
        color = "black"),
    legend.position = "none")

cColorsCU = c("#fdb863", "#b2abd2") #cued | uncued
cColorsHB = c("#abdda4","#2b83ba")  #head | body
cLabels = c("free\nviewing", "explicit\nencoding")
cLimitsObjects = c(0,0.17)
cLimitsHeads = c(0,0.28)
