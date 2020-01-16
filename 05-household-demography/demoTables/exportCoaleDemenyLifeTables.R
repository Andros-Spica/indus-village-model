######################################################################
# Script to export 'nqx' (probabilities of dying between age n and x)
# from Coale-Demeny Model Life Tables generated with 'demoR' package
# demoR package version 0.6.0 (2018-09-13)
# by James Holland Jones and collaborators
# Their source:
# Coale, A., P. Demeny, and B. Vaughn. 1983. 
# Regional model life tables and stable populations. 
# 2nd ed. New York: Academic Press.
######################################################################

#install.packages("demogR")
#
#this.dir <- dirname(parent.frame(2)$ofile) # r file must be "sourced" for this to work in RStudio
#setwd(this.dir)

library(demogR)

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


# West

write.table(
  interpolatePerYear(t(cdmltw(sex = "F")$nqx)), 
  file = "cdmltwF.txt")

write.table(
  interpolatePerYear(t(cdmltw(sex = "F")$nqx)), 
  file = "cdmltwM.txt")

# East

write.table(
  interpolatePerYear(t(cdmlte(sex = "F")$nqx)), 
  file = "cdmlteF.txt")

write.table(
  interpolatePerYear(t(cdmlte(sex = "F")$nqx)), 
  file = "cdmlteM.txt")

# South

write.table(
  interpolatePerYear(t(cdmlts(sex = "F")$nqx)), 
  file = "cdmltsF.txt")

write.table(
  interpolatePerYear(t(cdmlts(sex = "F")$nqx)), 
  file = "cdmltsM.txt")

# North

write.table(
  interpolatePerYear(t(cdmltn(sex = "F")$nqx)), 
  file = "cdmltnF.txt")

write.table(
  interpolatePerYear(t(cdmltn(sex = "F")$nqx)), 
  file = "cdmltnM.txt")
