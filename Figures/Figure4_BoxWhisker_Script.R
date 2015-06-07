# Takes the input data frame and extracts all the force column vectors and 
# concatinated those vectors into a single force vector.
# Inputs:
#   where: A string specifying where the drug concentration data is
# Outputs:
#   A vector of the forces for all of the animals in the specified drug 
#   concentration
Forces_As_Vector <- function(where){
  input <- read.csv(where)
  Force_Constant <- 0.0518
  Max_Reportable <- 20.58
  out <- as.vector(input[grep("Force",names(input))])
  out <- out[!is.na(out)]*Force_Constant
  out[out < Max_Reportable]
}

textSize <- 10

directory <- paste0("~/Documents/MyFolders/DaphniaLab",
                    "/Raw Data/A68930/Dose_uM_CSV")
Control <- Forces_As_Vector(paste0(
  directory, 
  "/0E1/AllAnimals/All_Control_0E1.csv"))
Drug_0.1 <- Forces_As_Vector(paste0(
  directory,
  "/1E-1/AllAnimals/All_Drug_1E-1.csv"))
Drug_1 <- Forces_As_Vector(paste0(
  directory,
  "/1E0/AllAnimals/All_Drug_1E0.csv"))
Drug_10 <- Forces_As_Vector(paste0(
  directory,
  "/1E1/AllAnimals/All_Drug_1E1.csv"))
Drug_100 <- Forces_As_Vector(paste0(
  directory,
  "/1E2/AllAnimals/All_Drug_1E2.csv"))

# unhash the following line to output a png of the plot
# png(filename = "Figure4_BoxWhisker_Plot.png", 
#     family = "Helvetica", 
#     width = 2400, 
#     height = 2400)

# unhash these lines to output a pdf of the plot
pdf(filename = "Figure4_BoxWhisker_Plot.pdf", 
    family = "Helvetica", 
     width = 33.35, 
    height = 33.35)

par(oma = c(bottom = 20, left = 25, top = 3, right = 3), 
    pin = c(width = 27, height = 27), 
    mgp = c(1,6,6), 
    las = 1, 
    ylbias = 0.8,
    col = "black", 
    lty = "solid", 
    cex.axis = 5)
boxplot(Control, Drug_0.1, Drug_1, Drug_10, Drug_100,
        col = c("grey100", "grey80", "grey60", "grey40", "grey20"),
        ylim = c(0,21),
        yaxs = "i",
        lty = "solid",
        cex = 5, 
        lwd = 10,
        outcex = 6, 
        outlwd = 3,
        staplelwd = 12, 
        axes = FALSE)
axis(side = 1, 
     at = seq(1, 5, 1), 
     label = c(0, 0.1, 1, 10, 100),
     padj = 1,
     line = 0, 
     lwd = 16, 
     tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 2, 
     at = seq(0, 21, 5),
     line = 0, 
     lwd = 16, 
     tck = -0.02, 
     cex.axis = textSize*0.75)
axis(side = 2, 
     at = seq(0, 21, 1), 
     label = NA,
     line = 0, 
     lwd = 8, 
     tck = -0.01, 
     cex.axis = textSize*0.75)
box(which = "plot", 
    bty = "L", 
    lwd = 16)
mtext(side = 1, 
      text = "A68930 Concentration (µM)", 
      cex = textSize, 
      line = 21)
mtext(side = 2, 
      text = "Force (µN)", 
      cex = textSize, 
      line = 18, 
      las = 3)
dev.off()
