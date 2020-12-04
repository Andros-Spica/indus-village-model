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
#' @param grid : template matrix of land units
#' @param flowDirection : 4xn data frame containing the flow directions of every patch in the grid (i.e. x1, y1, x2, y2) 
#' @param waterLayers : List of three matrices (numeric), each corresponding to the amounts per land unit of surface water, runoff and soil water. 
#' @param runOffCurveNumber : saturation (fraction of soil volume) (single value, numeric)
#' @param soilWaterSaturation : saturation (fraction of soil volume) (single value, numeric)
#' @param rootDepthZone : root zone depth (mm) (single value, numeric)
#' @export
solveRunoffExchange <- function(grid, flowDirection, 
                                waterLayers, 
                                runoffCurveNumber = 65, 
                                soilWaterSaturation, 
                                rootDepthZone = 400)
{
  # get template grid
  grid <- waterLayers$surfaceWater == -99
  
  # identify patches that receive flow and those that do not (this makes the next step much easier)
  patchState <- setInitialState(grid, flowDirection)
  
  # create a safety check to assure the algorithm will not get stuck in any loop
  #patchIsFlowDirectionLoop <- isFlowDirectionLoop(grid, flowDirection)

  maxIterations = 100000
  
  while (maxIterations > 0 & 
         any(patchState == "pending") & #!patchIsFlowDirectionLoop) & 
         any(patchState == "start")) 
  {
    # get patch coordinates of a random patch with patchState == "start"
    patchesWithStart <- which(patchState == "start", arr.ind = TRUE)
    
    thisPatch <- patchesWithStart[sample(nrow(patchesWithStart), 1),]
    
    
    waterLayers <- setRunoff(thisPatch, waterLayers, runoffCurveNumber)
    
    waterLayers <- infiltrateSoilWater(thisPatch, waterLayers, soilWaterSaturation, rootDepthZone)
    
    patchState[thisPatch[1], thisPatch[2]] <- "done"
    
    downstreamPatch <- flowDirection[flowDirection[,1] == thisPatch[1] & flowDirection[,2] == thisPatch[2], c(3, 4)]
    
    if (length(downstreamPatch) != 0)
    {
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
#' @param waterLayers : List of three matrices (numeric), each corresponding to the amounts per land unit of surface water, runoff and soil water. 
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
#' @param waterLayers : List of three matrices (numeric), each corresponding to the amounts per land unit of surface water, runoff and soil water. 
#' @param soilWaterSaturation : saturation (fraction of soil volume) (single value, numeric)
#' @param rootDepthZone : root zone depth (mm) (single value, numeric)
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

