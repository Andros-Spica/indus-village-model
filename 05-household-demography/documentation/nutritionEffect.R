### Example of nutrition effect function

# function for interpolating life tables per year old using age groups
interpolatePerYear <- function(raw, ages = c(0.5, 1.5, 4, seq(8.5, 93.5, 5))) {
  
  perYear <- data.frame(matrix(numeric(0), nrow = 151, ncol = ncol(raw)))
  names(perYear) <- 1:ncol(perYear)
  
  for (i in 1:ncol(raw)) {
    perYear[, i] <- approx(ages, raw[, i], 
                           xout = 1:151, yleft = 0, yright = 1)$y
  }
  row.names(perYear) <- 0:150
  
  return(perYear)
}

# function for getting interpolated life tables
generateCoaleDemenyLifeTable <- function(sex = "F", level = 8){
  
  curve <- interpolatePerYear(t(demogR::cdmltw(sex = sex)$nqx))[,level]
  
  return(curve)
}

# nutrition function
getNutrition <- function(consumed, numPeople)
{
  return(((consumed - numPeople) / numPeople)^3)
}

# general function
netMortCurve <- function(consumed, numPeople, mort, dimReturns = 0.1)
{
  nutrition = getNutrition(consumed, numPeople)
  
  nutEff = nutrition * (1 - mort) 
  if (nutrition > 0) { nutEff = nutEff * dimReturns }
  
  return(mort - nutEff)
}

# plot nutrition function
scaleMulti = 2
png("nutrition.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(0,5000), c(-1,1), type = "n",
     main = "Nutrition function", 
     sub = "example: needed = 2500, consumptionLevel = 1.5, required = 2500 x 1.5 = 3750, consumed = 0 to 5000",
     xlab = "consumed\n", ylab = "output")

abline(v = 2500, lty = 3, lwd = 2 * scaleMulti)
lines(c(2500, 2750), c(0.73, 0.83))
text(2500, 0.9, labels = "needed", font = 3, adj = -0.25)
abline(v = 3750, lty = 2, lwd = 2 * scaleMulti)
lines(c(3750, 4000), c(0.73, 0.83))
text(3750, 0.9, labels = "desired", font = 3, adj = -0.25)
abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

lines(0:5000, getNutrition(0:5000, 2500), col = "darkred", lwd = 2 * scaleMulti)
rect(85, 0.25, 1950, 0.75)
lines(c(1000, 1200), c(0.25, -0.15))
text(1000, 0.5,
     expression(
       "nutrition" == bgroup("(", frac("consumed" - "required", "required"), ")")^3
     )
)
dev.off()

# plot nutrition effect on mortality
png("nutritionEffect-mortality.png", width = 800, height = 480)
par(cex = 2)

plot(c(1, 151), c(0, 1), type = "n", 
     main = "Coale-Demeny model of mortality (region = west, level = 8)\nwith nutrition effect (dimReturns = 0.1)", cex.main = 1,
     xlab = "AGE", cex.lab = 1.5,
     ylab = "q(x)",
     xlim = c(0, 100)
)
# default mortality (+0)
lines(1:151, generateCoaleDemenyLifeTable(), col = "black", lwd = 3)
# very good nutrition (+10)
lines(1:151, 
      netMortCurve(consumed = 25, numPeople = 15, mort = generateCoaleDemenyLifeTable()), 
      col = "blue", lwd = 3)
# good nutrition (+5)
lines(1:151, 
      netMortCurve(consumed = 20, numPeople = 15, mort = generateCoaleDemenyLifeTable()), 
      col = "cyan", lwd = 3)
# bad nutrition (-5)
lines(1:151, 
      netMortCurve(consumed = 10, numPeople = 15, mort = generateCoaleDemenyLifeTable()), 
      col = "pink", lwd = 3)
# very bad nutrition (-10)
lines(1:151, 
      netMortCurve(consumed = 5, numPeople = 15, mort = generateCoaleDemenyLifeTable()), 
      col = "red", lwd = 3)
# starvation (none consumed)
lines(1:151, 
      netMortCurve(consumed = 0, numPeople = 15, mort = generateCoaleDemenyLifeTable()), 
      col = "darkred", lwd = 3)

text(30, 0.8,
     expression(
       "nutEffect" == bgroup("{", atop(" nutrition " %*% q(x) %*% "dimReturns, if nutrition" > 0 * " ", 
                                       " nutrition " %*% (1 - q(x)) * ", else "), "}") 
     )
     , cex = 0.5)

legend(80, 0.6, 
       c("very good (+10)", "good (+5)", "default (+0)", "bad (-5)", "very bad (-10)", "starvation"), 
       col = c("blue", "cyan", "black", "pink", "red", "darkred"),
       lwd = 3,
       cex = 0.5,
       title = "nutrition")

dev.off()
