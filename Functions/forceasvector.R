Forces_As_Vector <- function(input){
  out <- as.vector(input[grep("Force",names(input))])
  out <- out[!is.na(out)]
  out
}

C <- 0.0518

All_Control_0 <- read.csv(
  "~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/0E1/AllAnimals/All_Control_0E1.csv")
Control <- Forces_As_Vector(All_Control_0)*C

All_Drug_0.1 <- read.csv(
  "~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/1E-1/AllAnimals/All_Drug_1E-1.csv")
Drug_0.1 <- Forces_As_Vector(All_Drug_0.1)*C

All_Drug_1 <- read.csv(
  "~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/1E0/AllAnimals/All_Drug_1E0.csv")
Drug_1 <- Forces_As_Vector(All_Drug_1)*C

All_Drug_10 <- read.csv(
  "~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/1E1/AllAnimals/All_Drug_1E1.csv")
Drug_10 <- Forces_As_Vector(All_Drug_10)*C

All_Drug_100 <- read.csv(
  "~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/1E2/AllAnimals/All_Drug_1E2.csv")
Drug_100 <- Forces_As_Vector(All_Drug_100)*C

Control <- (Control)[(Control) < 20.58]
Drug_0.1 <- (Drug_0.1)[(Drug_0.1) < 20.58]
Drug_1 <- (Drug_1)[(Drug_1) < 20.58]
Drug_10 <- (Drug_10)[(Drug_10) < 20.58]
Drug_100 <- (Drug_100)[(Drug_100) < 20.58]

# png("Figure4_BoxWhisker_Plot.png", 
#     width = 2400, height = 2400)
# par(oma = c(bottom = 20, left = 25, 
#             top = 3,    right = 3), 
#     pin = c(width = 27, height = 27), 
#     mgp = c(1,6,6),
#     family = "Calibri", las = 1, ylbias = 0.8,
#     col="black", lty="solid", cex.axis = 5)

# dev.off()
# par(mar=c(5,4,4,2)+0.1)
plot(density(log(Control,exp(1))))
plot(density(log(Drug_0.1,exp(1))))
plot(density(log(Drug_1,exp(1))))
plot(density(log(Drug_10,exp(1))))
plot(density(log(Drug_100,exp(1))))

plot(density(Control))
plot(density(Drug_0.1))
plot(density(Drug_1))
plot(density(Drug_10))
plot(density(Drug_100))

plot(cdfGenerator(Control,-3),type="l")
plot(cdfGenerator(Drug_0.1,-3),type="l")
plot(cdfGenerator(Drug_1,-3),type="l")
plot(cdfGenerator(Drug_10,-3),type="l")
plot(cdfGenerator(Drug_100,-3),type="l")
