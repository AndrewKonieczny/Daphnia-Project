boxWhiskerFunction <- function(){
  ppi <- 1000
  png("Box_Plot.png", width=6*ppi, height=6*ppi, res=ppi, pointsize = 12, type = "cairo")
  boxplot(control, drug_0.1, drug_1, drug_10, drug_100,
          xlab = "",
          ylab = "",
          las = 1,
          pch = 1,
          lty = 1,
          range = 1.5,
          width = NULL,
          varwidth = FALSE,
          notch = FALSE,
          outline = TRUE,
          plot = TRUE,
          frame = FALSE,
          border = par("fg"),
          col = NULL,
          log = "",
          pars = list(boxwex = 0.8, 
                      staplewex = 0.5, 
                      outwex = 0.5),
          horizontal = FALSE,
          add = FALSE,
          at = NULL, 
          type = 10,
          axes = FALSE)
  title(main = NULL,
        xlab = "A68930 Concentration",
        ylab = expression(paste("Force (",mu,N,")")),
        cex.lab = 1.5,
        line = 2,
        family = "Times")
  axis(side = 1, 
       at = c(0,1,2,3,4,5,6), 
       labels = c("",expression(paste("0",mu,M)),
                  expression(paste("0.1",mu,M)),
                  expression(paste("1",mu,M)),
                  expression(paste("10",mu,M)),
                  expression(paste("100",mu,M)),""), 
       tick = TRUE, 
       line = NA, 
       pos = 0, 
       outer = FALSE, 
       font = NA, 
       lty = "solid",
       lwd = 2, 
       col = NULL,
       hadj = NA, 
       padj = NA,
       cex.axis = 1.25,
       family = "Times")
  axis(side = 2, 
       at = seq(0,20,5), 
       labels = TRUE, 
       tick = TRUE, 
       line = NA, 
       pos = 0.5, 
       outer = FALSE, 
       font = NA, 
       lty = "solid",
       lwd = 2, 
       col = NULL,
       hadj = NA, 
       padj = NA,
       cex.axis = 1.25,
       family = "Times")
  rectangle(control,1,0)
  rectangle(drug_0.1,2,20)
  rectangle(drug_1,3,30)
  rectangle(drug_10,4,40)
  rectangle(drug_100,5,50)
  points(mean(control))
  dev.off()
}