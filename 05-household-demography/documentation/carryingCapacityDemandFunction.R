# Household labour function

carryingCapacityDemandPerAge <- function(){
  carryingCapacityDemandList <- c()
  
  for (i in 0:100)
  {
    carryingCapacityDemand <- 0
    if (i < 15)
    {
      carryingCapacityDemand <- (i / 15)^1.2
    }
    else
    {
      if (i < 45)
      {
        carryingCapacityDemand <- 1
      }
      else
      {
        carryingCapacityDemand <- (45 / i)^2
      }
    }
    carryingCapacityDemand <- max(0, carryingCapacityDemand)
    carryingCapacityDemandList <- c(carryingCapacityDemandList, carryingCapacityDemand)
  }
  
  return(carryingCapacityDemandList)
}

grScale = 2

png("plots/carryingCapacityDemand-equation.png", width = grScale * 450, height = grScale * 400)
par(cex = grScale * 2, mar = c(0,0,0,0))

plot(c(0, 1), c(0, 1), type = "n")

text(0, 0.85,
     expression(
       paste(
         k(x) == frac(x, 15)^1.2, ", ", italic("if "), x < 15
       )
     ), cex = grScale, adj = 0)
text(0, 0.5,
     expression(
       paste(
         k(x) == frac(45, x)^2, ", ", italic("if "), x > 45
       )
     ), cex = grScale, adj = 0)
text(0, 0.15,
     expression(
       paste(
         k(x) == 1, italic(" else ")
       )
     ), cex = grScale, adj = 0)

dev.off()


png("plots/carryingCapacityDemand-infograph.png", width = 640, height = 480)
par(cex = 2)

plot(c(1, 100), c(0, 1), type = "n", 
     xlab = "AGE",
     ylab = "carrying capacity demand",
     xlim = c(0, 100)
)

lines(0:100, carryingCapacityDemandPerAge(), col = "blue", lwd = 6)

dev.off()


