### Example of malnutrition effect function

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

getDensityEffect <- function(carryingCapacity, numberOfPeople, densityEffectScalingFactor, densityEffectSteepness)
{
  densityEffect <- densityEffectScalingFactor * (numberOfPeople / carryingCapacity) ^ densityEffectSteepness
  
  return(densityEffect)
}


# general function
netMortCurve <- function(carryingCapacity, numberOfPeople, densityEffectScalingFactor, densityEffectSteepness)
{
  probDeath = generateCoaleDemenyLifeTable()
  
  densityEff <- getDensityEffect(carryingCapacity, numberOfPeople, densityEffectScalingFactor, densityEffectSteepness)
  
  for (i in 1:length(probDeath))
  {
    if (probDeath[i] < 1)
    {
      modProbDeath <- probDeath[i] + densityEff
      probDeath[i] <- min(1, max(0, modProbDeath))
    }
  }
  
  return(probDeath)
}


densityEffectScalingFactor = 0.05 # 0.05 -> 5% increase per person under carrying capacity
densityEffectSteepness = 3

# plot density function with carrying capacity variation
scaleMulti = 2
numberOfPeople = 200
maxCarryingCapacity = 500
png("plots/density-effect_varying-carring-capacity.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(0,maxCarryingCapacity), c(-1,1), type = "n",
     main = "Density effect under variation of carrying capacity", 
     sub = "example: number of people (N) = 200; scaling factor (d) = 0.05; steepness = 3",
     xlab = "carrying capacity (K)\n", ylab = "output")

lines(0:maxCarryingCapacity, getDensityEffect(0:maxCarryingCapacity, numberOfPeople, densityEffectScalingFactor, densityEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

abline(v = numberOfPeople, lty = 3, lwd = 2 * scaleMulti)

lines(c(numberOfPeople, numberOfPeople * 1.1), c(0.73, 0.83))
text(numberOfPeople, 0.9, labels = "N", font = 3, adj = -2)

text(maxCarryingCapacity * 0.8, 0.5,
     expression(
       "densityEff" == 0.05*bgroup("(", frac("N", "K"), ")")^3
     )
)

dev.off()

# plot density function with population variation 
scaleMulti = 2
carryingCapacity = 200
maxNumberOfPeople = 500
png("plots/density-effect_varying-population.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(0,maxNumberOfPeople), c(-1,1), type = "n",
     main = "Density effect under variation of number of people", 
     sub = "example: carrying capacity (K) = 200; scaling factor (d) = 0.05; steepness = 3",
     xlab = "number of people (N)\n", ylab = "output")

lines(0:maxNumberOfPeople, getDensityEffect(carryingCapacity, 0:maxNumberOfPeople, densityEffectScalingFactor, densityEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

abline(v = numberOfPeople, lty = 3, lwd = 2 * scaleMulti)

lines(c(numberOfPeople, numberOfPeople * 1.1), c(0.73, 0.83))
text(numberOfPeople, 0.9, labels = "K", font = 3, adj = -2)

text(maxNumberOfPeople * 0.18, -0.45,
     expression(
       "densityEff" == 0.05*bgroup("(", frac("N", "K"), ")")^3
     )
)

dev.off()

# plot density effect on mortality
scaleMulti = 2
png("plots/density-effect-on-probability-of-death.png", width = 800 * scaleMulti, height = 480 * scaleMulti)
par(cex = 1.2 * scaleMulti)

plot(c(1, 151), c(0, 1), type = "n", 
     main = "Coale-Demeny model of mortality modified by density effect", 
     cex.main = 0.8 * scaleMulti,
     sub = "example: region = west, level = 8; number of people (N) = 200; scaling factor (d) = 0.05; steepness = 3",
     
     xlab = "AGE", 
     cex.lab = 0.9 * scaleMulti,
     ylab = "q(x)",
     xlim = c(0, 100)
)
# very good density (K>>N)
lines(1:151, 
      netMortCurve(carryingCapacity = 1000, numberOfPeople = 150, densityEffectScalingFactor = densityEffectScalingFactor, densityEffectSteepness = densityEffectSteepness), 
      col = "blue", lwd = 2 * scaleMulti)
# good density (K>N)
lines(1:151, 
      netMortCurve(carryingCapacity = 300, numberOfPeople = 150, densityEffectScalingFactor = densityEffectScalingFactor, densityEffectSteepness = densityEffectSteepness), 
      col = "cyan", lwd = 2 * scaleMulti)
# bad density (K<N)
lines(1:151, 
      netMortCurve(carryingCapacity = 100, numberOfPeople = 150, densityEffectScalingFactor = densityEffectScalingFactor, densityEffectSteepness = densityEffectSteepness), 
      col = "pink", lwd = 2 * scaleMulti)
# very bad density (K<<N)
lines(1:151, 
      netMortCurve(carryingCapacity = 60, numberOfPeople = 150, densityEffectScalingFactor = densityEffectScalingFactor, densityEffectSteepness = densityEffectSteepness), 
      col = "red", lwd = 2 * scaleMulti)
# starvation (K=0)
lines(1:151, 
      netMortCurve(carryingCapacity = 0, numberOfPeople = 150, densityEffectScalingFactor = densityEffectScalingFactor, densityEffectSteepness = densityEffectSteepness), 
      col = "darkred", lwd = 2 * scaleMulti)
# default mortality (K=N)
lines(1:151, generateCoaleDemenyLifeTable(), col = "black", lwd = 3 * scaleMulti, lty = 4)

text(30, 0.5,
     expression(
       q(x) == Q(x) + "densityEffect")
     , cex = 0.5 * scaleMulti)

legend(73, 0.4, 
       c("very good (K=1000>>N)", "good (K=300>N)", "default (no effect, Q(x))", "bad (K=100<N)", "very bad (K=60<<N)", "starvation (K=0)"), 
       col = c("blue", "cyan", "black", "pink", "red", "darkred"),
       lwd = 3 * scaleMulti,
       cex = 0.45 * scaleMulti)

dev.off()

### Malnutrition effect instead of carrying capacity

### single requirement

getMalnutritionEffect <- function(consumed, required, malnutritionEffectScalingFactor, malnutritionEffectSteepness)
{
  malnutritionEffect <- malnutritionEffectScalingFactor * (1 - consumed / required) ^ malnutritionEffectSteepness
  # ignore negative values (when consumed > required)
  malnutritionEffect <- sapply(malnutritionEffect, function(x) max(c(0, x)))
  
  return(malnutritionEffect)
}

# general function
netMortCurve <- function(consumed, required, malnutritionEffectScalingFactor, malnutritionEffectSteepness)
{
  probDeath = generateCoaleDemenyLifeTable()
  
  malnutritionEff <- getMalnutritionEffect(consumed, required, malnutritionEffectScalingFactor, malnutritionEffectSteepness)
  
  for (i in 1:length(probDeath))
  {
    if (probDeath[i] < 1)
    {
      modProbDeath <- probDeath[i] + malnutritionEff
      probDeath[i] <- min(1, max(0, modProbDeath))
    }
  }
  
  return(probDeath)
}

malnutritionEffectScalingFactor = 1
malnutritionEffectSteepness = 3

# plot malnutrition effect with variation of consumption
scaleMulti = 2
required = 2000
maxConsumed = 4000
png("plots/malnutrition-effect_varying-consumed.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(0,maxConsumed), c(-0.1,1), type = "n",
     main = "Malnutrition effect under variation of consumption", 
     sub = "example: required (R) = 2000; scaling factor (n) = 1; steepness = 3",
     xlab = "consumed (C)\n", ylab = "output")

lines(0:maxConsumed, getMalnutritionEffect(0:maxConsumed, required, malnutritionEffectScalingFactor, malnutritionEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

abline(v = required, lty = 3, lwd = 2 * scaleMulti)

lines(c(required, required * 1.1), c(0.73, 0.83))
text(required, 0.9, labels = "R", font = 3, adj = -2)

text(maxConsumed * 0.8, 0.5,
     expression(
       "malnutritionEff" == 1*bgroup("(", 1 - frac("C", "R"), ")")^3
     )
)

dev.off()

# plot malnutrition effect with variation of requirements
scaleMulti = 2
consumed = 2000
maxRequired = 10000
png("plots/malnutrition-effect_varying-required.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(0,maxRequired), c(-0.1,1), type = "n",
     main = "Malnutrition effect under variation of requirement", 
     sub = "example: consumed (C) = 2000; scaling factor (n) = 1; steepness = 3",
     xlab = "required (R)\n", ylab = "output")

lines(1:maxRequired, getMalnutritionEffect(consumed, 1:maxRequired, malnutritionEffectScalingFactor, malnutritionEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

abline(v = consumed, lty = 3, lwd = 2 * scaleMulti)

lines(c(consumed, consumed * 1.1), c(0.73, 0.83))
text(consumed, 0.9, labels = "C", font = 3, adj = -2)

text(maxRequired * 0.7, 0.6,
     expression(
       "malnutritionEff" == 1*bgroup("(", 1 - frac("C", "R"), ")")^3
     )
)

dev.off()

# plot density effect on mortality
scaleMulti = 2
png("plots/malnutrition-effect-on-probability-of-death.png", width = 800 * scaleMulti, height = 480 * scaleMulti)
par(cex = 1.2 * scaleMulti)

plot(c(1, 151), c(0, 1), type = "n", 
     main = "Coale-Demeny model of mortality modified by malnutrition effect", 
     cex.main = 0.8 * scaleMulti,
     sub = "example: region = west, level = 8; requirement (R) = 200; scaling factor (n) = 1; steepness = 3",
     
     xlab = "AGE", 
     cex.lab = 0.9 * scaleMulti,
     ylab = "q(x)",
     xlim = c(0, 100)
)
# very good nutrition (K>>N)
lines(1:151, 
      netMortCurve(consumed = 1000, required = 150, malnutritionEffectScalingFactor = malnutritionEffectScalingFactor, malnutritionEffectSteepness = malnutritionEffectSteepness), 
      col = "blue", lwd = 2 * scaleMulti)
# good nutrition (K>N)
lines(1:151, 
      netMortCurve(consumed = 300, required = 150, malnutritionEffectScalingFactor = malnutritionEffectScalingFactor, malnutritionEffectSteepness = malnutritionEffectSteepness), 
      col = "cyan", lwd = 2 * scaleMulti)
# bad nutrition (K<N)
lines(1:151, 
      netMortCurve(consumed = 100, required = 150, malnutritionEffectScalingFactor = malnutritionEffectScalingFactor, malnutritionEffectSteepness = malnutritionEffectSteepness), 
      col = "pink", lwd = 2 * scaleMulti)
# very bad nutrition (K<<N)
lines(1:151, 
      netMortCurve(consumed = 60, required = 150, malnutritionEffectScalingFactor = malnutritionEffectScalingFactor, malnutritionEffectSteepness = malnutritionEffectSteepness), 
      col = "red", lwd = 2 * scaleMulti)
# starvation (K=0)
lines(1:151, 
      netMortCurve(consumed = 0, required = 150, malnutritionEffectScalingFactor = malnutritionEffectScalingFactor, malnutritionEffectSteepness = malnutritionEffectSteepness), 
      col = "darkred", lwd = 2 * scaleMulti)
# default mortality (K=N)
lines(1:151, generateCoaleDemenyLifeTable(), col = "black", lwd = 3 * scaleMulti, lty = 4)

text(30, 0.5,
     expression(
       q(x) == Q(x) + "malnutritionEffect")
     , cex = 0.5 * scaleMulti)

legend(73, 0.4, 
       c("very good (C=1000>>R)", "good (C=300>R)", "default (no effect, Q(x))", "bad (C=100<R)", "very bad (C=60<<R)", "starvation (C=0)"), 
       col = c("blue", "cyan", "black", "pink", "red", "darkred"),
       lwd = 3 * scaleMulti,
       cex = 0.45 * scaleMulti)

dev.off()

### cumulative malnutrition effect

getMalnutrition <- function(consumed, required)
{
  malnutritionEffect <- (1 - consumed / required)
  # ignore negative values (when consumed > required)
  malnutritionEffect <- sapply(malnutritionEffect, function(x) max(c(0, x)))
  
  return(malnutritionEffect)
}

getMalnutritionPerIteration <- function(consumedPerIteration, requiredPerIteration)
{
  return(mapply(function(x, y) getMalnutrition(x, y), consumedPerIteration, requiredPerIteration))
}

getCumulativeMalnutrition <- function(consumedPerIteration, requiredPerIteration)
{
  malnutritionPerIteration <- getMalnutritionPerIteration(consumedPerIteration, requiredPerIteration)
  cumMalnutrition <- cumsum(malnutritionPerIteration)
  return(cumMalnutrition)
}

getCumulativeMalnutritionEffect <- function(consumedPerIteration, required, malnutritionEffectScalingFactor, malnutritionEffectSteepness)
{
  cumMalnutrition <- getCumulativeMalnutrition(consumedPerIteration, required)
  cumMalnutritionEffect <- malnutritionEffectScalingFactor * (1 - exp(-1 * malnutritionEffectSteepness * cumMalnutrition))
  return(cumMalnutritionEffect)
}

# general function
netMortCurve <- function(consumedPerIteration, requiredPerIteration, malnutritionEffectScalingFactor, malnutritionEffectSteepness)
{
  probDeath = generateCoaleDemenyLifeTable()
  
  malnutritionEff <- getCumulativeMalnutritionEffect(consumedPerIteration, requiredPerIteration, malnutritionEffectScalingFactor, malnutritionEffectSteepness)
  
  for (i in 1:length(probDeath))
  {
    if (probDeath[i] < 1)
    {
      modProbDeath <- probDeath[i] + malnutritionEff
      probDeath[i] <- min(1, max(0, modProbDeath))
    }
  }
  
  return(probDeath)
}

numberOfIterations <- 90

# plot cumulative malnutrition effect with decreasing consumption
scaleMulti = 2
consumedPerIteration <- seq(from = 2100, to = 1800, length.out = numberOfIterations)
requiredPerIteration <- rep(2000, numberOfIterations)
png("plots/cumulative-malnutrition-effect_decreasing-consumed.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(1,numberOfIterations), c(-0.1,1), type = "n",
     main = "Malnutrition effect under decreasing consumption", 
     sub = "example: required (R) = 2000; scaling factor (n) = 1; steepness = 3",
     xlab = "iterations\n", ylab = "output")

lines(1:numberOfIterations, getMalnutritionPerIteration(consumedPerIteration, requiredPerIteration), col = "black", lty = 3, lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutrition(consumedPerIteration, requiredPerIteration), col = "blue", lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutritionEffect(consumedPerIteration, requiredPerIteration, malnutritionEffectScalingFactor, malnutritionEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

text(numberOfIterations * 0.2, 0.5,
     expression(
       "q"["S(t)"] == 1*bgroup("(", 1 - e^"3S(t)", ")")
     )
)
text(numberOfIterations * 0.2, 0.4,
     expression(
       "S(t)" == "S(t-1)" + "s(t)"
     )
)
text(numberOfIterations * 0.2, 0.28,
     expression(
       "s(t)" == "max" * bgroup("(", "0, 1" - frac("C(t)", "R(t)"), ")")
     )
)

legend(0, 1, 
       c("malnutrition (s(t))", "cumulative malnutrition (S(t))", expression(paste("cumulative malnutrition effect(", "q"["S(t)"], ")"))), 
       col = c("black", "blue", "darkred"),
       lty = c(3, 1, 1),
       lwd = 3 * scaleMulti,
       cex = 0.45 * scaleMulti)

dev.off()

# plot cumulative malnutrition effect with increasing consumption
scaleMulti = 2
consumedPerIteration <- seq(from = 1900, to = 2000, length.out = numberOfIterations)
requiredPerIteration <- rep(2000, numberOfIterations)
png("plots/cumulative-malnutrition-effect_increasing-consumed.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(1,numberOfIterations), c(-0.1,1), type = "n",
     main = "Malnutrition effect under increasing consumption", 
     sub = "example: required (R) = 2000; scaling factor (n) = 1; steepness = 3",
     xlab = "iterations\n", ylab = "output")

lines(1:numberOfIterations, getMalnutritionPerIteration(consumedPerIteration, requiredPerIteration), col = "black", lty = 3, lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutrition(consumedPerIteration, requiredPerIteration), col = "blue", lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutritionEffect(consumedPerIteration, requiredPerIteration, malnutritionEffectScalingFactor, malnutritionEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

text(numberOfIterations * 0.8, 0.5,
     expression(
       "q"["S(t)"] == 1*bgroup("(", 1 - e^"3S(t)", ")")
     )
)
text(numberOfIterations * 0.8, 0.4,
     expression(
       "S(t)" == "S(t-1)" + "s(t)"
     )
)
text(numberOfIterations * 0.8, 0.28,
     expression(
       "s(t)" == "max" * bgroup("(", "0, 1" - frac("C(t)", "R(t)"), ")")
     )
)

legend(numberOfIterations * 0.6, 0.8, 
       c("malnutrition (s(t))", "cumulative malnutrition (S(t))", expression(paste("cumulative malnutrition effect(", "q"["S(t)"], ")"))), 
       col = c("black", "blue", "darkred"),
       lty = c(3, 1, 1),
       lwd = 3 * scaleMulti,
       cex = 0.45 * scaleMulti)

dev.off()

# plot cumulative malnutrition effect with increasing requirements
scaleMulti = 2
consumedPerIteration <- rep(2000, numberOfIterations)
requiredPerIteration <- seq(from = 2000, to = 2200, length.out = numberOfIterations)
png("plots/cumulative-malnutrition-effect_increasing-required.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(1,numberOfIterations), c(-0.1,1), type = "n",
     main = "Malnutrition effect under increasing requirements", 
     sub = "example: consumed (C) = 2000; scaling factor (n) = 1; steepness = 3",
     xlab = "iterations\n", ylab = "output")

lines(1:numberOfIterations, getMalnutritionPerIteration(consumedPerIteration, requiredPerIteration), col = "black", lty = 3, lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutrition(consumedPerIteration, requiredPerIteration), col = "blue", lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutritionEffect(consumedPerIteration, requiredPerIteration, malnutritionEffectScalingFactor, malnutritionEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

text(numberOfIterations * 0.8, 0.5,
     expression(
       "q"["S(t)"] == 1*bgroup("(", 1 - e^"3S(t)", ")")
     )
)
text(numberOfIterations * 0.8, 0.4,
     expression(
       "S(t)" == "S(t-1)" + "s(t)"
     )
)
text(numberOfIterations * 0.8, 0.28,
     expression(
       "s(t)" == "max" * bgroup("(", "0, 1" - frac("C(t)", "R(t)"), ")")
     )
)

legend(numberOfIterations * 0.6, 0.8, 
       c("malnutrition (s(t))", "cumulative malnutrition (S(t))", expression(paste("cumulative malnutrition effect(", "q"["S(t)"], ")"))), 
       col = c("black", "blue", "darkred"),
       lty = c(3, 1, 1),
       lwd = 3 * scaleMulti,
       cex = 0.45 * scaleMulti)

dev.off()

# plot cumulative malnutrition effect with decreasing requirements
scaleMulti = 2
consumedPerIteration <- rep(2000, numberOfIterations)
requiredPerIteration <- seq(from = 2200, to = 2000, length.out = numberOfIterations)
png("plots/cumulative-malnutrition-effect_decreasing-required.png", width = scaleMulti * 800, height = scaleMulti * 480)
par(cex = scaleMulti * 1.2)
plot(c(1,numberOfIterations), c(-0.1,1), type = "n",
     main = "Malnutrition effect under decreasing requirements", 
     sub = "example: consumed (C) = 2000; scaling factor (n) = 1; steepness = 3",
     xlab = "iterations\n", ylab = "output")

lines(1:numberOfIterations, getMalnutritionPerIteration(consumedPerIteration, requiredPerIteration), col = "black", lty = 3, lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutrition(consumedPerIteration, requiredPerIteration), col = "blue", lwd = 2 * scaleMulti)
lines(1:numberOfIterations, getCumulativeMalnutritionEffect(consumedPerIteration, requiredPerIteration, malnutritionEffectScalingFactor, malnutritionEffectSteepness), col = "darkred", lwd = 2 * scaleMulti)

abline(h = 0, lty = 4, lwd = 2 * scaleMulti)

text(numberOfIterations * 0.8, 0.5,
     expression(
       "q"["S(t)"] == 1*bgroup("(", 1 - e^"3S(t)", ")")
     )
)
text(numberOfIterations * 0.8, 0.4,
     expression(
       "S(t)" == "S(t-1)" + "s(t)"
     )
)
text(numberOfIterations * 0.8, 0.28,
     expression(
       "s(t)" == "max" * bgroup("(", "0, 1" - frac("C(t)", "R(t)"), ")")
     )
)

legend(numberOfIterations * 0.6, 0.8, 
       c("malnutrition (s(t))", "cumulative malnutrition (S(t))", expression(paste("cumulative malnutrition effect(", "q"["S(t)"], ")"))), 
       col = c("black", "blue", "darkred"),
       lty = c(3, 1, 1),
       lwd = 3 * scaleMulti,
       cex = 0.45 * scaleMulti)

dev.off()


