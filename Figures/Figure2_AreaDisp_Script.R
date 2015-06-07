# A68930 0.1µM Drug09 Time04
Animal <- read.delim("~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/1E-1/Drug09_Time04_Animal.csv")
Fiber <- read.delim("~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/1E-1/Drug09_Time04_Fiber.csv")
Displacement <- median(Fiber$X)-Fiber$X[7:417]
Area <- 100*(Animal$Area[32:442] - 
               median(Animal$Area[32:442]))/
  median(Animal$Area[32:442])
Time <- Fiber$X.1[7:417]/30
textSize <- 10

# unhash the following line to output a png of the plot
# png(filename = "Figure2_Area_vs_Disp_Plot.png", 
#     family = "Helvetica", 
#     width = 2400, 
#     height = 2400)

# unhash these lines to output a pdf of the plot
pdf(filename = "Figure2_Area_vs_Disp_Plot.pdf", 
    family = "Helvetica", 
    width = 33.35, 
    height = 33.35)

par(oma = c(bottom = 20, left = 20, 
            top = 3,    right = 17), 
    pin = c(width = 21, height = 27.1), 
    mgp = c(1,6,6), 
    las = 1, 
    ylbias = 0.8,
    col="black", 
    lty="solid", 
    cex.axis = 5)
plot(x = Time, 
     y = Displacement, 
     xaxs = "i",
     xlab = "", 
     ylab = "", 
     type = "l", 
     lwd = 10, 
     col = "black", 
     ylim = c(-100,500),
     bty = "n", 
     axes = FALSE)
axis(side = 4, 
     at = pretty(range(Displacement)), 
     mgp = c(3,5,1),
     las = 1, 
     line = 0, 
     lwd = 16, 
     tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 4, 
     at = seq(-100,500,25), 
     label = NA,
     line = 0, 
     lwd = 8, 
     tck = -0.01)
par(new = TRUE)
plot(x = Time, 
     y = Area, 
     xaxs = "i", 
     lty = "solid",
     xlab = "", 
     ylab = "", 
     type = "l", 
     lwd = 10, 
     col = "grey60", 
     bty = "n", 
     axes = FALSE, 
     ylim = c(5,-25))
axis(side = 1, 
     at = pretty(range(Time)), 
     padj = 1,
     mgp = c(3,5,1),
     las = 1, 
     line = 0, 
     lwd = 16, 
     tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 1, 
     at = seq(0,15,1), 
     label = NA,
     line = 0, 
     lwd = 8, 
     tck = -0.01)
axis(side = 2, 
     at = seq(-25,5,5),
     label = c(paste0(seq(-25,5,5),"%")), 
     mgp = c(3,5,1),
     las = 1, 
     line = 0, 
     lwd = 16, 
     tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 2, 
     at = seq(-25,5,1),
     label = NA,
     line = 0, 
     lwd = 8, 
     tck = -0.01)
box(which = "plot", 
    lty = "solid", 
    bty = "u", 
    col = "black", 
    lwd = 16)
legend(x = 4, 
       y = -25.5, 
       cex = 5, 
       lty = c(1,1),
       legend = c("Area Deviation","Fiber Displacement"),
       col = c("grey60","black"), 
       lwd = c(16,16))
mtext("Time (s)", 
      side = 1, 
      cex = textSize, 
      line = 20)
mtext("Fiber Displacement (pixels)", 
      side = 4, 
      cex = textSize, 
      line = 26, 
      las = 3)
mtext("Animal Area Deviation (%)", 
      side = 2, 
      cex = textSize, 
      line = 24, 
      las = 3)
dev.off()
par(mar=c(5.1,4.1,4.1,2.1))