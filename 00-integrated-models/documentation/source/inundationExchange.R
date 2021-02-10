################################################################################
# Script containing equivalent R code for the procedures implemented in NetLogo 
# for solving inundation exchange between patches. NetLogo code available at:
# https://github.com/TwoRains/indus-village-model/tree/master/00-Integrated%20models
# or
# https://github.com/Andros-Spica/indus-village-model/tree/master/00-Integrated%20models
################################################################################

#' @title Solve inundation exchange
#' @param elevation : nxn matrix (m, numeric) containing elevation per land unit. 
#' @param surfaceWater : nxn matrix (mm, numeric), the amounts per land unit of surface water depth
#' @param errorToleranceThreshold : amount of surface water depth (m) serving as redistribution unit and minimal difference to be accounted between land units (single value, numeric). Should be arbitrarily small.
#' @param maxIterations : maximum number of while loop iterations set to avoid infinite loops (single value, numeric)
#' @export
solveInundationExchange <- function(elevation, 
                                    surfaceWater,
                                    errorToleranceThreshold = 1,
                                    maxIterations = 100000)
{
  # identify patches that have excess water
  patchesWithExcess <- getPatchesWithExcessWater(elevation = elevation, 
                                            surfaceWater = surfaceWater,
                                            errorToleranceThreshold = errorToleranceThreshold)
  
  while (maxIterations > 0 & 
         any(patchesWithExcess)) 
  {
    # get patch coordinates of a random patch with excess water
    indexesOfPatchesWithExcess <- which(patchesWithExcess, arr.ind = TRUE)
    
    thisPatch <- indexesOfPatchesWithExcess[sample(nrow(indexesOfPatchesWithExcess), 1),]
    
    # get the coordinates of all land units within Moore's neighborhood
    neighborhood <- getMooreNeighborhood(x = thisPatch[1], 
                                         y = thisPatch[2], 
                                         xmin = 1, ymin = 1,
                                         xmax = nrow(surfaceWater), 
                                         ymax = ncol(surfaceWater))
    
    # get the sum of surface water depth (m) for the entire neighborhood 
    sumNeighborhoodWaterDepth <- sum(
      getPatchValuesFromMatrix(patchesCoordinates = neighborhood, 
                               matrixOfValues = surfaceWater)
      ) / 1000
    
    # set all water in neighborhood to zero
    #surfaceWater[neighborhood[,1], neighborhood[,2]] <- 0
    surfaceWater <- setPatchValuesFromMatrix(patchesCoordinates = neighborhood, 
                                             newValues = rep(0, nrow(neighborhood)), 
                                             matrixOfValues = surfaceWater)
    
    # iterate n times, where n is the neighborhood sum divided by errorToleranceThreshold (rounded value)
    for (i in 1:round(sumNeighborhoodWaterDepth / errorToleranceThreshold))
    {
      # get coordinates of the lowest patch in the neighborhood
      # height is elevation (m) + surfaceWater (mm) / 1000
      lowestPatchInNeighborhood <- getLowestPatchInNeighborhood(neighborhood = neighborhood,
                                                                elevation = elevation, 
                                                                surfaceWater = surfaceWater)
      
      # add errorToleranceThreshold * 1000 to the amount of surface water depth (mm) 
      surfaceWater[lowestPatchInNeighborhood[1], lowestPatchInNeighborhood[2]] <- 
        surfaceWater[lowestPatchInNeighborhood[1], lowestPatchInNeighborhood[2]] +
        errorToleranceThreshold * 1000
    }
    
    # update patches with excess water
    patchesWithExcess <- getPatchesWithExcessWater(elevation = elevation, 
                                              surfaceWater = surfaceWater,
                                              errorToleranceThreshold = errorToleranceThreshold)
    
    # ask neighbors and neighbors of neighbors (excluding this patch) to update patchesWithExcess 
    # NOTE: this piece of code is effective and much faster than asking all patches to update patchesWithExcess
    
    # exclude this patch from patchesWithExcess
    patchesWithExcess[thisPatch[1], thisPatch[2]] <- FALSE
    
    # get extended neighborhood
    extendedNeighborhood <- neighborhood
    for (i in 1:nrow(neighborhood))
    {
      extendedNeighborhood <- rbind(extendedNeighborhood,
                                    getMooreNeighborhood(x = neighborhood[i, 1], 
                                                         y = neighborhood[i, 2], 
                                                         xmin = 1, ymin = 1,
                                                         xmax = nrow(elevation), 
                                                         ymax = ncol(elevation)))
    }
    extendedNeighborhood <- unique(extendedNeighborhood)
    
    # For each patch in extended neighborhood,
    # check if it has excess water depth,
    # than add or exclude it from patchesWithExcess
    for (i in 1:nrow(extendedNeighborhood))
    {
      patchesWithExcess[extendedNeighborhood[i, 1], extendedNeighborhood[i, 2]] <- 
        hasExcessWater(patchCoordinates = extendedNeighborhood[i,], 
                       elevation = elevation, 
                       surfaceWater = surfaceWater, 
                       errorToleranceThreshold = errorToleranceThreshold) 
    }
    
    maxIterations = maxIterations - 1
  }
  
  
  return(surfaceWater)
}

#' @title Get whether each land unit has excess water
#' @param elevation : nxn matrix (m, numeric) containing elevation per land unit. 
#' @param surfaceWater : nxn matrix (mm, numeric), the amounts per land unit of surface water depth
#' @param errorToleranceThreshold : amount of surface water depth (m) serving as redistribution unit and minimal difference to be accounted between land units (single value, numeric). Should be arbitrarily small.
#' @export
getPatchesWithExcessWater <- function(elevation, surfaceWater, errorToleranceThreshold)
{
  patchesWithExcess <- matrix(rep(FALSE, length(elevation)), 
                       nrow = nrow(elevation), ncol = ncol(elevation), byrow = T)
  
  for (i in 1:nrow(patchesWithExcess))
  {
    for (j in 1:ncol(patchesWithExcess))
    {
      patchesWithExcess[i, j] <- ( 
        hasExcessWater(patchCoordinates = c(i, j), 
                       elevation = elevation, 
                       surfaceWater = surfaceWater, 
                       errorToleranceThreshold = errorToleranceThreshold)
      )
    }
  }
  return(patchesWithExcess)
}

#' @title Get whether a land unit has excess water
#' @param patchCoordinates : coordinates of the land units to report
#' @param elevation : nxn matrix (m, numeric) containing elevation per land unit. 
#' @param surfaceWater : nxn matrix (mm, numeric), the amounts per land unit of surface water depth
#' @param errorToleranceThreshold : amount of surface water depth (m) serving as redistribution unit and minimal difference to be accounted between land units (single value, numeric). Should be arbitrarily small.
#' @export
hasExcessWater <- function(patchCoordinates, elevation, surfaceWater, errorToleranceThreshold)
{
  # get the coordinates of all land units within Moore's neighborhood
  neighborhood <- getMooreNeighborhood(x = patchCoordinates[1], 
                                       y = patchCoordinates[2], 
                                       xmin = 1, ymin = 1,
                                       xmax = nrow(surfaceWater), 
                                       ymax = ncol(surfaceWater))
  
  # get the height as elevation + surfaceWater / 1000
  neighborhoodHeights <- getHeights(patchesCoordinates = neighborhood, 
                                    elevation = elevation, 
                                    surfaceWater = surfaceWater)
  
  return(
    (surfaceWater[patchCoordinates[1], patchCoordinates[2]] > 0) &&
      any(neighborhoodHeights[1] - neighborhoodHeights > errorToleranceThreshold)
  )
}

#' @title get the height (m) of given land units as elevation (m) + surfaceWater (mm) / 1000
#' @param patchesCoordinates : coordinates of the land units to report
#' @param elevation : nxn matrix (m, numeric) containing elevation per land unit. 
#' @param surfaceWater : nxn matrix (mm, numeric), the amounts per land unit of surface water depth
#' @export
getHeights <- function(patchesCoordinates, elevation, surfaceWater)
{
  heights <- c()
  
  for (i in 1:nrow(patchesCoordinates))
  {
    heights <- c(
      heights,
      elevation[patchesCoordinates[i, 1], patchesCoordinates[i, 2]] + surfaceWater[patchesCoordinates[i, 1], patchesCoordinates[i, 2]] / 1000
    )
  }
  
  return(heights)
}

#' @title Get the coordinates of land units within the Moore's neighborhood of the given central coordinates
#' @param x,y : the grid coordinates of the central land unit (integer)
#' @param xmin,xmax,ymin,ymax : the dimensions of the grid from which the coordinates are taken (integer). 
#' @export
getMooreNeighborhood <- function(x, y, xmin, xmax, ymin, ymax)
{
  xCords <- c(x)
  yCords <- c(y)
  
  for (i in -1:1)
  {
    if (x + i >= xmin && x + i <= xmax)
    {
      for (j in -1:1)
      {
        if (y + j >= ymin & y + j <= ymax & !(i == 0 & j == 0))
        {
          xCords <- c(xCords, x + i)
          yCords <- c(yCords, y + j)
        }
      }
    }
  }
  
  return(cbind(x = xCords, y = yCords))
}

#' @title Get values in given coordinates of a matrix 
#' @param patchesCoordinates : matrix coordinates to report
#' @param matrixOfValues : nxn matrix (m, numeric) containing values per pair of matrix coordinates. 
#' @export
getPatchValuesFromMatrix <- function(patchesCoordinates, matrixOfValues)
{
  values <- c()
  
  for (i in 1:nrow(patchesCoordinates))
  {
    values <- c(values, matrixOfValues[patchesCoordinates[i, 1], patchesCoordinates[i, 2]])
  }
  
  return(values)
}

#' @title Set values in given coordinates of a matrix 
#' @param patchesCoordinates : matrix coordinates to set
#' @param newValues : new values to replace each value in the given coordinates
#' @param matrixOfValues : nxn matrix (m, numeric) containing values per pair of matrix coordinates. 
#' @export
setPatchValuesFromMatrix <- function(patchesCoordinates, newValues, matrixOfValues)
{
  for (i in 1:nrow(patchesCoordinates))
  {
    matrixOfValues[patchesCoordinates[i, 1], patchesCoordinates[i, 2]] <- newValues[i]
  }
  
  return(matrixOfValues)
}

#' @title Get the (first) land unit with the least height within the neighborhood
#' @param neighborhood : the coordinates of land units within the neighborhood 
#' @param elevation : nxn matrix (m, numeric) containing elevation per land unit. 
#' @param surfaceWater : nxn matrix (mm, numeric), the amounts per land unit of surface water depth
#' @export
getLowestPatchInNeighborhood <- function(neighborhood, elevation, surfaceWater)
{
  heights <- getHeights(patchesCoordinates = neighborhood, 
                        elevation = elevation, 
                        surfaceWater = surfaceWater)
  
  lowestPatch <- 1
  
  # this algorithm will return the first patch with the minimum height, 
  # from top-left to bottom-right
  for (i in 1:nrow(neighborhood))
  {
    if (heights[i] < heights[lowestPatch])
    {
      lowestPatch <- i
    }
  }
  
  return(neighborhood[lowestPatch,])
}
