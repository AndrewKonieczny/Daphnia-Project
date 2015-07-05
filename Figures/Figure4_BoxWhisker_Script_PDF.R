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

doses <- c("0E1","1E-1","1E0","1E1","1E2")
data_directory <- paste0("~/Documents/MyFolders/DaphniaLab",
                    "/Raw Data/A68930/Dose_uM_CSV")
data <- lapply(list.files(file.path(data_directory, doses, "AllAnimals"), 
                          pattern = "All_", full.names = T),
               Forces_As_Vector)
figure_directory <- "/Users/bmcdonnell/Documents/MyFolders/DaphniaLab/Figures"


pdf(file = "Figure4_BoxWhisker_Plot.pdf", 
    family = "Times", 
    title = "Figure 4",
    width = 14, 
    height = 14,
    paper = "special",
    pointsize = 30)
setwd(figure_directory)
par( mar = c(3.25,3.25,.5,.5),
     family = "Times", 
     mgp = c(2,0.6,0))
boxplot(data,
        col = c("grey100", "grey80", "grey60", "grey40", "grey20"),
        ylim = c(0,21),
        yaxs = "i", 
        xlab = "Concentration (µM)",
        ylab = "Force (µN)",
        lty = "solid", 
        varwidth = T, notch = T,
        names = c(0, 0.1, 1, 10, 100),
        cex = 1, cex.lab = 1, cex.axis = .75, 
        border = par("fg"),
        pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5), 
        outcex = .5, 
        outlwd = .5, frame = F,
        axes = T)
box(bty="l")
dev.off()
