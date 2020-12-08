################################################################################
# Script containing equivalent R code for the procedures implemented in NetLogo 
# for solving runoff exchange between patches. NetLogo code available at:
# https://github.com/TwoRains/indus-village-model/tree/master/00-Integrated%20models
# or
# https://github.com/Andros-Spica/indus-village-model/tree/master/00-Integrated%20models
# Model concept and implementation is based on:
# "Working with dynamic models for agriculture"
# R script for practical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA), Sylvain Toulet (INRA, internship 2012)
# version : 2019-10-30 by Andreas Angourakis (using version from 2012-04-23)
# Model described in the book, Appendix. Models used as illustrative examples: description and R code
################################################################################

#' @title Solve runoff exchange
#' @param elevation : nxn matrix (m, numeric) containing elevation per land unit. 
#' @param flowDirection : 4xn data frame containing the flow directions of every patch in the grid (i.e. x1, y1, x2, y2) 
#' @param waterLayers : List of three nxn matrices (mm, numeric), each corresponding to the amounts per land unit of surface water, runoff and soil water. 
#' @param runOffCurveNumber : saturation (fraction of soil volume) (single value, numeric)
#' @param soilWaterSaturation : saturation (fraction of soil volume) (single value, numeric)
#' @param rootDepthZone : root zone depth (mm) (single value, numeric)
#' @param maxIterations : maximum number of while loop iterations set to avoid infinite loops (single value, numeric)
#' @export
solveRunoffExchange <- function(elevation, 
                                flowDirection, 
                                waterLayers, 
                                runoffCurveNumber = 65, 
                                soilWaterSaturation = 0.5, 
                                rootDepthZone = 400,
                                maxIterations = 100000)
{
  # identify patches that receive flow and those that do not (this makes the next step much easier)
  patchState <- setInitialState(grid = elevation, 
                                flowDirection = flowDirection)
  
  # create a safety check to assure the algorithm will not get stuck in any loop
  #patchIsFlowDirectionLoop <- isFlowDirectionLoop(elevation, flowDirection)

  while (maxIterations > 0 & 
         any(patchState == "pending") & #!patchIsFlowDirectionLoop) & 
         any(patchState == "start")) 
  {
    # get patch coordinates of a random patch with patchState == "start"
    patchesWithStart <- which(patchState == "start", arr.ind = TRUE)
    
    thisPatch <- patchesWithStart[sample(nrow(patchesWithStart), 1),]
    
    waterLayers <- setRunoff(thisPatch = thisPatch, 
                             waterLayers = waterLayers, 
                             runoffCurveNumber = runoffCurveNumber)
    
    waterLayers <- infiltrateSoilWater(thisPatch = thisPatch, 
                                       waterLayers = waterLayers, 
                                       soilWaterSaturation = soilWaterSaturation, 
                                       rootDepthZone = rootDepthZone)
    
    patchState[thisPatch[1], thisPatch[2]] <- "done"
    
    downstreamPatch <- flowDirection[flowDirection[,1] == thisPatch[1] & flowDirection[,2] == thisPatch[2], c(3, 4)]
    
    if (length(downstreamPatch) != 0)
    {
      waterLayers <- tryToSendRunoff(thisPatch = thisPatch, 
                                     downstreamPatch = downstreamPatch, 
                                     elevation = elevation, 
                                     waterLayers = waterLayers)
      
      neighborsFlowingToDowstreamPatch <- flowDirection[flowDirection[,3] == downstreamPatch[1] & flowDirection[,4] == downstreamPatch[2], c(1, 2)]
      
      ifelse(length(neighborsFlowingToDowstreamPatch) > 2,
             neighborsFlowingToDowstreamPatch_done <- all(patchState[as.matrix(neighborsFlowingToDowstreamPatch)] == "done"),
             neighborsFlowingToDowstreamPatch_done <- all(patchState[neighborsFlowingToDowstreamPatch[1],neighborsFlowingToDowstreamPatch[2]] == "done")
      )
      
      if (all(neighborsFlowingToDowstreamPatch_done))
      {
        patchState[downstreamPatch[1], downstreamPatch[2]] <- "start"
      }
    }
    
    maxIterations = maxIterations - 1
  }
  
  
  return(waterLayers)
}

#' @title Set initial state of land units depending on flows directed to it
#' @param grid : template matrix of land units
#' @param flowDirection : 4xn data frame containing the flow directions of every patch in the grid (i.e. x1, y1, x2, y2) 
#' @export
setInitialState <- function(grid, flowDirection)
{
  patchState <- matrix(rep("start", length(grid)), 
                       nrow = nrow(grid), ncol = ncol(grid), byrow = T)
  
  for (i in 1:nrow(patchState))
  {
    for (j in 1:ncol(patchState))
    {
      flowDirectionPair <- flowDirection[flowDirection[,1] == i & flowDirection[,2] == j, ]
      
      if (length(flowDirectionPair) != 0)
      {
        patchState[flowDirectionPair[3], flowDirectionPair[4]] <- "pending"
      }
    }
  }
  return(patchState)
}

#' @title Identify land units that might flow in a loop (safety check)
#' @param grid : template matrix of land units
#' @param flowDirection : 4xn data frame containing the flow directions of every patch in the grid (i.e. x1, y1, x2, y2) 
#' @export
isFlowDirectionLoop <- function(grid, flowDirection)
{
  isLoop <- grid
  
  for (i in 1:nrow(isLoop))
  {
    for (j in 1:ncol(isLoop))
    {
      flowDirectionPair <- flowDirection[flowDirection[,1] == i & flowDirection[,2] == j, ]
      
      flowDirectionPairDownstream <- flowDirection[flowDirection[,1] == flowDirectionPair[3] & 
                                                     flowDirection[,2] == flowDirectionPair[4], ]
      
      isLoop[i, j] <- length(flowDirectionPair) != 0 && 
        length(flowDirectionPairDownstream) != 0 && 
        all(flowDirectionPair[1:2] == flowDirectionPairDownstream[3:4])
    }
  }
  
  return(isLoop)
}

#' @title Calculate runoff in a land unit
#' @description equations based on the first part of the WaterBalance model
#' @param thisPatch : the grid coordinates of the land unit (vector of length 2, integer)
#' @param waterLayers : List of three nxn matrices (mm, numeric), each corresponding to the amounts per land unit of surface water, runoff and soil water. 
#' @param runoffCurveNumber : saturation (fraction of soil volume) (single value, numeric)
#' @export
setRunoff <- function(thisPatch, waterLayers, runoffCurveNumber)
{
  # Maximum abstraction (mm; for run off)
  maximumAbstraction = 25400 / runoffCurveNumber - 254
  
  # Initial Abstraction (mm; for run off)
  initialAbstraction = 0.2 * maximumAbstraction
  
  # Change in Water Before Drainage (p_water - p_runoff)
  waterLayers$runoff[thisPatch[1], thisPatch[2]] <- 0
  #print(paste(waterLayers$surfaceWater[thisPatch[1], thisPatch[2]], initialAbstraction, " = ", waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] > initialAbstraction))
  if (waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] > initialAbstraction)
  {
    waterLayers$runoff[thisPatch[1], thisPatch[2]] = 
      ((waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] - 0.2 * maximumAbstraction) ^ 2) / 
      (waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] + 0.8 * maximumAbstraction)
  }
  
  return(waterLayers)
}

#' @title Infiltrate surface water to soil water in a land unit
#' @description equations based on the second part of the WaterBalance model
#' @param thisPatch : the grid coordinates of the land unit (vector of length 2, integer)
#' @param waterLayers : List of three nxn matrices (mm, numeric), each corresponding to the amounts per land unit of surface water, runoff and soil water. 
#' @param soilWaterSaturation : saturation (fraction of soil volume) (single value, numeric)
#' @param rootDepthZone : root zone depth (mm, single value, numeric)
#' @export
infiltrateSoilWater <- function(thisPatch, waterLayers, soilWaterSaturation, rootDepthZone)
{
  # WATst : Maximum Water content at saturation (mm)
  WATst = soilWaterSaturation * rootDepthZone
  
  potentialSoilWaterChange = waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] - waterLayers$runoff[thisPatch[1], thisPatch[2]]
  soilWaterChange = 0
  
  if ( waterLayers$soilWater[thisPatch[1], thisPatch[2]] + 
             (waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] - 
                waterLayers$runoff[thisPatch[1], thisPatch[2]]) > WATst )
  {
    # soil is or becomes saturated and water accumulates on surface adding to runoff
    soilWaterChange = WATst - 
      waterLayers$soilWater[thisPatch[1], thisPatch[2]]
    waterLayers$runoff[thisPatch[1], thisPatch[2]] = 
      waterLayers$runoff[thisPatch[1], thisPatch[2]] + 
      (potentialSoilWaterChange - soilWaterChange)
  }
  else
  {
    # soil absorbes all water not running off
    soilWaterChange = potentialSoilWaterChange
  }
  
  ### add amount to soil water content
  waterLayers$soilWater[thisPatch[1], thisPatch[2]] = 
    waterLayers$soilWater[thisPatch[1], thisPatch[2]] + soilWaterChange
  waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] = 
    waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] - 
    (soilWaterChange + waterLayers$runoff[thisPatch[1], thisPatch[2]])
  
  return(waterLayers)
}

#' @title Try to send runoff from one land unit to another
#' @description algorithm based on Yang et al. 2018 (http://link.springer.com/10.1007/s10666-018-9597-3)
#' @param thisPatch : the grid coordinates of the land unit (vector of length 2, integer)
#' @param downstreamPatch : the grid coordinates of the downstream land unit (vector of length 2, integer)
#' @param elevation : matrix (m, numeric) containing elevation per land unit. 
#' @param waterLayers : List of three nxn matrices (mm, numeric), each corresponding to the amounts per land unit of surface water, runoff and soil water. 
#' @export
tryToSendRunoff <- function(thisPatch, downstreamPatch, elevation, waterLayers)
{
  ### schematics:
  
  #==== case 1: h1 of A is higher than (h2 of B) + (h2 - h1) of A
  #
  #----- h2 ----|
  #----- h1 ----|                      |----- h1 ----|
  #             |                --->  |             |----- h2 ----|
  #             |----- h2 ----|        |             |             |
  #             |----- h1 ----|        |             |----- h1 ----|
  #______A______|______B______|        |______A______|______B______|
  
  #==== case 2: h1 of A is lower than (h2 of B) + (h2 - h1) of A
  #
  #----- h2 ----|                      _ _ _ _h2_ _ _ _ _ _ h2_ _ _
  #             |----- h2 ----|        |             |             |
  #----- h1 ----|             |  --->  |----- h1 ----|             |
  #             |             |        |             |             |
  #             |----- h1 ----|        |             |----- h1 ----|
  #______A______|______B______|        |______A______|______B______|
  
  #==== case 3: next downstream patch (B) is already overflowed (h2 of B > h2 of A)
  #
  #             |----- h2 ----|
  #             |             |        |----- h2 ----|----- h2 ----|
  #----- h2 ----|             |  --->  |             |             |
  #----- h1 ----|             |        |----- h1 ----|             |
  #             |----- h1 ----|        |             |----- h1 ----|
  #______A______|______B______|        |______A______|______B______|
  
  #print(thisPatch)
  
  thisPatch_h1 = elevation[thisPatch[1], thisPatch[2]]
  thisPatch_h2 = waterLayers$runoff[thisPatch[1], thisPatch[2]] / 1000 # in m
  
  downstreamPatch_h1 = elevation[downstreamPatch[1], downstreamPatch[2]]
  downstreamPatch_h2 = waterLayers$surfaceWater[downstreamPatch[1], downstreamPatch[2]] / 1000 # in m
  
  # print(paste("[", thisPatch[1], ", ", thisPatch[2], "]", ", h1=", thisPatch_h1, ", h2=", thisPatch_h2, 
  #             " ||| ", "[", downstreamPatch[1], ", ", downstreamPatch[2], "]", ", h1=", downstreamPatch_h1, ", h2=", downstreamPatch_h2))
  
  ### case 1:
    ### if the elevation with water of the downstream patch is lower, even after transferring all runoff
  if (thisPatch_h1 > downstreamPatch_h1 + downstreamPatch_h2 + thisPatch_h2)
  {#print("case 1")
    ### all runoff is transfered downstream
    ### update water downstream (receives all runoff)
    waterLayers$surfaceWater[downstreamPatch[1], downstreamPatch[2]] <- 
      waterLayers$surfaceWater[downstreamPatch[1], downstreamPatch[2]] +
      thisPatch_h2 * 1000 # in mm
    
    # print(paste(waterLayers$surfaceWater[downstreamPatch[1], downstreamPatch[2]], 
    #             " + ", 
    #             thisPatch_h2, " * 1000 = ", waterLayers$surfaceWater[downstreamPatch[1], downstreamPatch[2]] + thisPatch_h2 * 1000))
  }
  else
  {#print("case 2 & 3") 
    ### case 2 & case 3
    ### runoff is equaly distributed
    ### ### update water downstream
    waterLayers$surfaceWater[downstreamPatch[1], downstreamPatch[2]] <- 1000 * # in mm
      (0.5 * (downstreamPatch_h1 + downstreamPatch_h2 + thisPatch_h1 + thisPatch_h2) - downstreamPatch_h1)
    
    # print(paste("downstreamPatch receives: 0.5 * (", thisPatch_h1, " + ", thisPatch_h2, " + ", downstreamPatch_h1, " + ", 
    #             downstreamPatch_h2, ") - ", downstreamPatch_h1, " = ", waterLayers$surfaceWater[thisPatch[1], thisPatch[2]]))
    
    ### update (effective) runoff (this should be the last instance this patch runoff can be modified during a time step)
    waterLayers$runoff[thisPatch[1], thisPatch[2]] <- 1000 * # in mm
      (0.5 * (downstreamPatch_h1 + downstreamPatch_h2 + thisPatch_h1 + thisPatch_h2) - downstreamPatch_h1)
    
    ### update water after runoff substraction
    waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] <- 1000 * # in mm
      (0.5 * (downstreamPatch_h1 + downstreamPatch_h2 + thisPatch_h1 + thisPatch_h2) - thisPatch_h1)
    
    # print(paste("thisPatch keeps: 0.5 * (", thisPatch_h1, " + ", thisPatch_h2, " + ", downstreamPatch_h1, " + ", 
    #             downstreamPatch_h2, ") - ", thisPatch_h1, " = ", waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] / 1000))
    
    # print(paste("total height (m): thisPatch = ", 
    #             (elevation[thisPatch[1], thisPatch[2]] + waterLayers$surfaceWater[thisPatch[1], thisPatch[2]] / 1000), 
    #             " ||| downstreamPatch = ", 
    #             elevation[downstreamPatch[1], downstreamPatch[2]] + waterLayers$surfaceWater[downstreamPatch[1], downstreamPatch[2]] / 1000))
  }
  
  return(waterLayers)
}
