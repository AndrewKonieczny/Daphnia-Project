#Figure_1unzeroed
CalibrationData <- read.csv(
  "~/Documents/MyFolders/DaphniaLab/Raw Data/Calibration/RawFiberCalibrationData.csv")
Calibration <- data.frame(cbind(CalibrationData$Y, CalibrationData$Force..uN.))
names(Calibration) <- c("Y", "Force")
Displacement <- Calibration$Y
Force <- Calibration$Force
not_Data <- intersect(which(Calibration$Y > 200), 
                      which(Calibration$Force == 0))
Displacement <- Displacement[-not_Data]
Force <- Force[-not_Data]
lm.data <- glm(Force ~ Displacement)

# unhash the following line to output a png of the plot
# png(filename = "Figure1_Unzeroed_Cal_Plot.png", 
#     family = "Helvetica", 
#     width = 2400, 
#     height = 2400)

# unhash these lines to output a pdf of the plot
pdf(filename = "Figure1_Unzeroed_Cal_Plot.pdf", 
    family = "Helvetica", 
    width = 33.35, 
    height = 33.35)

par(oma = c(bottom = 20, left = 20, 
            top = 3,    right = 1), 
    pin = c(width = 27.25, height = 27.75), 
    mgp = c(1,6,6), 
    las = 1, ylbias = 0.8,
    col="black", lty="solid", cex.axis = 5)
plot(x = Displacement, y = Force,
     xlab = "", ylab = "", 
     xlim = c(0,max(Displacement)),
     xaxs = "i", yaxs = "i", bty = "n", 
     cex = 9, lwd = 6, xpd = T, axes = F)
axis(side = 1, at = seq(0, 500, 100), padj = 1,
     line = 0, lwd = 16, tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 2, at = seq(0, 21, 5),
     line = 0, lwd = 16, tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 1, at = seq(0, 500, 25), label = NA,
     line = 0, lwd = 8, tck = -0.01)
axis(side = 2, at = seq(0, 21, 1), label = NA,
     line = 0, lwd = 8, tck = -0.01)
abline(lm.data, lwd = 10)
box(which = "plot", bty = "l", lwd = 16)
mtext(side = 1, "Displacement (pixels)", 
      cex = textSize, line = 20)
mtext(side = 2, "Force (µN)", 
      cex = textSize, line = 17, las = 3)
dev.off()
par(mar=c(5,4,4,2)+0.1)
