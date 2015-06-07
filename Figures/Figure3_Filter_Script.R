#par(mar = c(4,5,2,0)+0.1)
whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
source( paste0(whereFun, "filter.R") )
Fiber <- read.delim("~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/1E-1/Drug09_Time04_Fiber.csv")
Time <- Fiber$X.1/30
Displacement <- median(Fiber$X)-Fiber$X 
Filtered <- filter(Fiber$X, range = 3)
textSize <- 10
setwd("/Users/bmcdonnell/Documents/MyFolders/DaphniaLab/Figures")
# unhash the following line to output a png of the plot
# png(filename = "Figure3_Filter_Plot.png", 
#     family = "Helvetica", 
#     width = 2400, 
#     height = 2400)

# unhash these lines to output a pdf of the plot
pdf("Figure3_Filter_Plot.pdf", 
    family = "Helvetica", 
    width = 33.35, 
    height = 33.35)

par(oma = c(bottom = 20, left = 20, 
            top = 3,    right = 1), 
    pin = c(width = 26, height = 27.75), 
    mgp = c(1,6,6),
    las = 1, ylbias = 0.8,
    col="black", lty="solid", cex.axis = 5)
plot(x = Time, y = Displacement, type = "l",
     xlab = "", ylab = "", xlim = c(0,14), ylim = c(-130,445),
     xaxs = "i", yaxs = "i", bty = "n", 
     cex = 9, lwd = 10, xpd = T, axes = F)
points(Time, Filtered, cex = 10, lwd = 5)
axis(side = 1, at = seq(0, 15, 2), padj = 1,
     line = 0, lwd = 16, tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 2, at = seq(-100, 500, 100),
     line = 0, lwd = 16, tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 1, at = seq(0, 15, 1), label = NA,
     line = 0, lwd = 8, tck = -0.01, 
     cex.axis = textSize*0.75)
axis(side = 2, at = seq(-150, 500, 25), label = NA,
     line = 0, lwd = 8, tck = -0.01, 
     cex.axis = textSize*0.75)
box(which = "plot", bty = "l", lwd = 16)
mtext(side = 1, "Displacement (pixels)", 
      cex = textSize, line = 20)
mtext(side = 2, "Force (µN)", 
      cex = textSize, line = 20, las = 3)
dev.off()