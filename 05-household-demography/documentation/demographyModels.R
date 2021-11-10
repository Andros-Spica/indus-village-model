### DEMOGRAPHIC MODELS ##########################################
### by Andreas Angourakis (Andors Spica)
### 2019
### Department of Archaeology, University of Cambridge

### fertility
# The following correspond to the first parametric model
# mentioned in page 147 of:
# # Peristeva and Kostaki, 2009
# "Modeling fertility in modern populations"
# Available from: https://dx.doi.org/10.4054/DemRes.2007.16.6

### nuptiality
# The following correspond to the first parametric model
# for fitting the age-specific distributions of marriages, mentioned in page 133 of:
# # Peristeva and Kostaki, 2015
# "A parametric model for estimating nuptiality patterns in modern populations"
# Available from: https://www.researchgate.net/publication/285457704_A_parametric_model_for_estimating_nuptiality_patterns_in_modern_populations [accessed Nov 27 2018].

# the equation is the same for both fertility and nuptiality.

generateParametricModelFertility <- function(par.c1 = 0.8, par.mu = 25, par.sigma = c(5, 10) ){
  
  curve <- c()
  for (i in 1:100)
  {
    sigma = par.sigma[1]
    if (i > par.mu)
    { sigma = par.sigma[2] }
    
    curve <- c(curve, par.c1 * exp (-1 * (((i - par.mu) / sigma) ^ 2)) )
  }
  
  return(curve)
}

generateParametricModelNuptiality <- function(par.c1 = 0.85, par.mu = 20, par.sigma = c(5, 10) ){
  
  return(generateParametricModelFertility(par.c1 = par.c1, par.mu = par.mu, par.sigma = par.sigma ))
}

grScale = 2

png("fertilityModel.png", width = grScale * 800, height = grScale * 480)
par(cex = grScale * 1.2)

plot(c(1, 100), c(0, 1), type = "n", 
     main = "parametric model of fertility ",
     xlab = "AGE",
     ylab = "p(x)"
)
for (i in 1:10)
{
  lines(1:100, generateParametricModelFertility(par.c1 = i * 0.1), col = i, lwd = grScale * 3)
}
legend(2, 1, 
       1:10, 
       col = 1:10,
       lwd = grScale * 3,
       title = expression(c[1]))
text(70, 0.8, 
     expression(
       paste(
         italic("if "), x <= mu, ", ", sigma == sigma[1], italic(" else "), sigma == sigma[2]
       )
     ), cex = grScale)
text(70, 0.5,
     expression(
       p(x) == c[1]*italic(e)^-(frac(x - mu, sigma))^2
     )
     , cex = grScale)

dev.off()
#----
png("nuptialityModel.png", width = grScale * 800, height = grScale * 480)
par(cex = grScale * 1.2)

plot(c(1, 100), c(0, 1), type = "n", 
     main = "parametric model of nuptiality ",
     xlab = "AGE",
     ylab = "p(x)"
)
lines(1:100, generateParametricModelNuptiality(), col = "red", lwd = grScale * 3)
lines(1:100, generateParametricModelNuptiality(par.c1 = .8, par.mu = 23, par.sigma = c(3,12)), col = "blue", lwd = grScale * 3)
text(70, 0.8, 
     expression(
       paste(
         italic("if "), x <= mu, ", ", sigma == sigma[1], italic(" else "), sigma == sigma[2]
         )
       ), cex = grScale)
text(70, 0.5,
     expression(
       p(x) == c[1]*italic(e)^-(frac(x - mu, sigma))^2
       )
     , cex = grScale)

dev.off()

### mortality
# from Coale-Demeny Model Life Tables generated with 'demoR' package
# demoR package version 0.6.0 (2018-09-13)
# by James Holland Jones and collaborators
# Their source:
# Coale, A., P. Demeny, and B. Vaughn. 1983. 
# Regional model life tables and stable populations. 
# 2nd ed. New York: Academic Press.

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

generateCoaleDemenyLifeTable <- function(region = "north", sex = "F", level = 8){
  
  curve <- 0
  
  if (region == "north")
  {
    curve <- interpolatePerYear(t(demogR::cdmltn(sex = sex)$nqx))[,level]
  }
  if (region == "west")
  {
    curve <- interpolatePerYear(t(demogR::cdmltw(sex = sex)$nqx))[,level]
  }
  if (region == "east")
  {
    curve <- interpolatePerYear(t(demogR::cdmlte(sex = sex)$nqx))[,level]
  }
  if (region == "south")
  {
    curve <- interpolatePerYear(t(demogR::cdmlts(sex = sex)$nqx))[,level]
  }
  
  return(curve)
}

grScale = 2

png("mortalityModel-levels.png", width = grScale * 800, height = grScale * 480)
par(cex = grScale * 1.2)

plot(c(1, 151), c(0, 1), type = "n", 
     main = "Coale-Demeny model of mortality\nlevels (north)",
     xlab = "AGE", cex.lab = 1.5,
     ylab = "q(x)",
     xlim = c(0, 100)
)
lines(1:151, generateCoaleDemenyLifeTable(level = 1), col = "blue", lwd = grScale * 3)
lines(1:151, generateCoaleDemenyLifeTable(level = 25), col = "darkgreen", lwd = grScale * 3)
lines(1:151, generateCoaleDemenyLifeTable(level = 8), col = "red", lwd = grScale * 3)

legend(5, 1, 
       c("1", "8", "25"), 
       col = c("blue", "red", "darkgreen"),
       lwd = grScale * 3,
       title = "levels")

dev.off()

png("mortalityModel-regions.png", width = grScale * 800, height = grScale * 480)
par(cex = grScale * 1.2)

plot(c(1, 151), c(0, 1), type = "n", 
     main = "Coale-Demeny model of mortality - regions (level 8)",
     xlab = "AGE", cex.lab = 1.5,
     ylab = "q(x)",
     xlim = c(0, 100)
)
lines(1:151, generateCoaleDemenyLifeTable(), col = "blue", lwd = 3)
lines(1:151, generateCoaleDemenyLifeTable(region = "west"), col = "darkgreen", lwd = grScale * 3)
lines(1:151, generateCoaleDemenyLifeTable(region = "east"), col = "red", lwd = grScale * 3)
lines(1:151, generateCoaleDemenyLifeTable(region = "south"), col = "purple", lwd = grScale * 3)

legend(0, 1, 
       c("north", "west", "east", "south"), 
       col = c("blue", "darkgreen", "red", "purple"),
       lwd = grScale * 3,
       title = "region")

dev.off()
