# Household labour function

labourPerAge <- function(){
  labourList <- c()
  
  for (i in 0:100)
  {
    labour <- 0
    if (i < 15)
    {
      labour <- (i / 15)^10
    }
    else
    {
      if (i < 45)
      {
        labour <- 1
      }
      else
      {
        labour <- (45 / i)^10
      }
    }
    labour <- max(0, labour)
    labourList <- c(labourList, labour)
  }

  return(labourList)
}

grScale = 2

png("plots/labour-equation.png", width = grScale * 450, height = grScale * 400)
par(cex = grScale * 2, mar = c(0,0,0,0))

plot(c(0, 1), c(0, 1), type = "n")

text(0, 0.85,
     expression(
       paste(
         L(x) == frac(x, 15)^10, ", ", italic("if "), x < 15
       )
     ), cex = grScale, adj = 0)
text(0, 0.5,
     expression(
       paste(
         L(x) == frac(45, x)^10, ", ", italic("if "), x > 45
       )
     ), cex = grScale, adj = 0)
text(0, 0.15,
     expression(
       paste(
         L(x) == 1, italic(" else ")
       )
     ), cex = grScale, adj = 0)

dev.off()


png("plots/labour-infograph.png", width = 640, height = 480)
par(cex = 2)

plot(c(1, 100), c(0, 1), type = "n", 
     xlab = "AGE",
     ylab = "labour adult/year",
     xlim = c(0, 100)
)

lines(0:100, labourPerAge(), col = "blue", lwd = 6)

dev.off()


