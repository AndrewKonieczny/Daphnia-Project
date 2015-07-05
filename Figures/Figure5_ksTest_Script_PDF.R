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

densityPlot <- function(input_data, group, conc){
  k <- suppressWarnings(
    ks.test(input_data[[1]], input_data[[group]])
    )
  control_group <- density(input_data[[1]])
  drug_group <- density(input_data[[group]])
  plot(control_group, frame = FALSE, col = "black",
       xlim = c(0,21), ylim = c(0,.25), 
       main = paste("0 µM vs.", conc, "µM"),
       xlab = "Force (µN)",
       cex.main = 0.9)
  box(bty = "l")
  lines(drug_group, col = "grey50")
  text(x = 12, y =0.25, pos = 1, cex = 0.5,
       labels = paste(k$method, 
                      "\nStatistic = ", round(k$statistic,3),
                      "\np-value = ", round(k$p.value,3)))
}

doses <- c("0E1","1E-1","1E0","1E1","1E2")

data_directory <- paste0("~/Documents/MyFolders/DaphniaLab",
                         "/Raw Data/A68930/Dose_uM_CSV")

data <- lapply(list.files(file.path(data_directory, doses, "AllAnimals"), 
                          pattern = "All_", full.names = T),
               Forces_As_Vector)

concentration <- c(0,0.1,1,10,100)

pdf(file = "Figure5_ksTest_Plot.pdf", 
    family = "Times", 
    title = "Figure 5",
    width = 14, 
    height = 14,
    paper = "special",
    pointsize = 30)

par(mfrow = c(2,2), las = 1, lwd = 2,
    mar = c(4,4,2,1), xaxs = "i", yaxs = "i",
    family = "Times", cex.axis = 0.75,
    mgp = c(2.5,1,0))

for(i in 2:5){
  densityPlot(data, i, concentration[i])
}

dev.off()
