;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Land model - v04 with soil depth and texture
;;  Copyright (C) Andreas Angourakis (andros.spica@gmail.com)
;;  available at https://www.github.com/Andros-Spica/indus-village-model
;;  This model includes a cleaner version of the Terrain Generator model v.2 (https://github.com/Andros-Spica/ProceduralMap-NetLogo)
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

extensions [ csv ]

breed [ transectLines transectLine ]
breed [ flowHolders flowHolder ]

globals
[
  ;;; constants
  maxDist

  ; elev_algorithm-style (GUI): style of algorithm to create land features "ranges" and "rifts".
  ; The algorithms styles available are: "NetLogo", using auxiliary agents and
  ; less parameters but with less control, and "C#", without agents and more
  ; parameters.

  ; flow_do-fill-sinks (GUI): whether to apply algorithm to fill all drainage "sinks" (land units
  ;                      not on the edge of the map where there is no flow towards a neighbour).
  ;                      Those sinks are likely to have been created by land forming algorithms.

  ;;; parameters (modified copies of interface input) ===============================================================

  ;;;; elevation
  elev_numRanges                  ; number of landforming features ("ranges", "rifts").
  elev_numRifts

  elev_rangeLength                ; maximum length, in land units, of landforming features.
  elev_riftLength

  elev_rangeHeight                ; the starting elevation of landforming features.
  elev_riftHeight                 ; These are also used as maximum and minimum elevation
                                  ; for x,y, and valley slopes.

  elev_featureAngleRange          ; the maximum change in features angle (direction every step)

  elev_smoothStep                 ; the level of smoothing applied to elevation.
                                  ; 0 = none, 1 = values are equated to the mean of neighbours.
  elev_smoothingRadius            ; maximum distance to include land units in another's neihgbourhood.
                                  ; Applied as a radius surrounding a land unit.

  elev_xSlope                     ; the level of adjustment of elevation to x (West-East) and
                                  ; y (North-South) slopes. 0 = no slope, 1 = no variation besides slopes.
  elev_ySlope

  elev_valleyAxisInclination      ; the level of inclination of the North-South valley.
                                  ; 0 = centred, 1 = top-right to bottom-left diagonal.
  elev_valleySlope                ; the level of adjustment of elevation to the North-South valley.
                                  ; 0 = no valley, 1 = no variation besides valley.

  ;;;;; used when algorithm-style = "C#"
  elev_numProtuberances           ; numDepressions: (approximated) number of distinct
  elev_numDepressions             ; protuberances/depressions. Consider those are acheived by aglutinating
                                  ; elevated and depressed land units.


  elev_rangeAggregation           ; the minimum proximity required between
  elev_riftAggregation            ; landform features, expressed as percentage of the map's maximum distance.

  elev_noise                      ; noise to be added/subtracted to elevation as the standard
                                  ; deviation of a centred normal distribution.

  ;;;;; used when algorithm-style = "NetLogo"
  elev_inversionIterations         ; the number of iterations all land units with neighbours
                                   ; with opposite elevation sign (i.e., if elevation = 10, neighbours with elevation < 0)
                                   ; have their elevation exchanged with one of those neighbours.

  ;;;; water flow accumulation
  flow_riverAccumulationAtStart ; the amount of flow units added to a land unit at the edge of the map.
                                ; These units may be transmitted following flow directions,
                                ; drawing meanders of a passing river.
                                ; To better scale this parameter, consider that the catchment area today of
                                ; the entire Indus River Basin is 116,500,000 ha or 1,165,000 km^2,
                                ; the Chenab River Basin in Pakistani Punjab is 2,615,500 ha or 26,155 km^2,
                                ; and the Ghaggar River Basin in Haryana is 4,997,800 ha or 49,978 km^2.

  ;;;; soil
  soil_formativeErosionRate              ; rate of increase in soil depth, decrease of percentage of sand,
                                         ; increase of percentage of silt, and increase of percentage of clay, depending on flow_accumulation.
  soil_minDepth                          ; minimum soil depth
  soil_maxDepth                          ; maximum soil depth
  soil_depthNoise                        ; normal random variation in soil depth (standard deviation)

  soil_min%sand                          ; minimum percentage of sand (within the represented area)
  soil_max%sand                          ; maximum percentage of sand (within the represented area)
  soil_min%silt                          ; minimum percentage of silt (within the represented area)
  soil_max%silt                          ; maximum percentage of silt (within the represented area)
  soil_min%clay                          ; minimum percentage of clay (within the represented area)
  soil_max%clay                          ; maximum percentage of clay (within the represented area)
  soil_textureNoise                      ; normal random variation in the proportion of sand/silt/clay (standard deviation of every component previous to normalisation)

  soil_textureTypes_display              ; Texture types ordered specifically for display (i.e. meaninful fixed colour pallete)

  ;;; variables ===============================================================
  seaLevel                    ; elevation considered as sea level for display purposes.

  landRatio                   ; the ratio of land units above or equal to seaLevel.
  elevationDistribution       ; the set or list containing the elevation of all land units
  minElevation                ; statistics on the elevation of land units.
  sdElevation
  maxElevation

  landWithRiver               ; count of land units with passing river
  maxFlowAccumulation

  mostCommonTextureType       ; the most common of texture type, see soil_textureTypes
]

patches-own
[
  elevation             ; average elevation above reference of the land unit (metres).
                        ; The reference is an arbitrary elevation from which all
                        ; algorithms will sculpt the terrain.

  flow_direction        ; the numeric code for the (main) direction of flow or
                        ; drainage within the land unit.
                        ; Following Jenson & Domingue (1988) convention:
                        ; NW = 64,   N = 128,        NE = 1,
                        ; W = 32,     <CENTRE>,   E = 2,
                        ; SW = 16,     S = 8,          SE = 4

  flow_receive          ; Boolean variable stating whether or not the land unit receives
                        ; the flow of a neighbour.

  flow_accumulation     ; the amount of flow units accumulated in the land unit.
                        ; A Flow unit is the volume of runoff water flowing from one land unit
                        ; to another (assumed constant and without losses).
  flow_accumulationState ; the state of the land unit regarding the calculation of flow
                        ; accumulation (auxiliary variable).

  ;;; soil conditions
  p_soil_formativeErosion   ; the intensity of previous erosion of parent materials
                            ; forming soil and finer soil elements (scale 0-1).

  p_soil_depth          ; soil depth in milimeters (mm)

  p_soil_%sand          ; percentage of sand fraction in soil
  p_soil_%silt          ; percentage of silt fraction in soil
  p_soil_%clay          ; percentage of clay fraction in soil
  p_soil_textureType          ; soil texture type according to sand-silt-clay proportions, under USDA convention.
                              ; see "03-land-model/documentation/USDA-texturalSoilClassification.png"
]

breed [ mapSetters mapSetter ] ; used when elev_algorithm-style = "NetLogo"

mapSetters-own [ numPoints ] ; the number of land units to be chained together as a "rift" or "range".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to create-terrain

  clear-all

  set-parameters

  ;;; START - core procedures ;;;;;;;;;;;;;;;;;;;;;;;

  setup-elevations

  setup-flows

  setup-soil

  ;;; END - core procedures ;;;;;;;;;;;;;;;;;;;;;;;

  set-output-stats

  paint-patches

  setup-patch-coordinates-labels "bottom" "left"

  setup-transect

  update-transects

  update-plots

end

to set-parameters

  random-seed randomSeed

  set maxDist (sqrt (( (max-pxcor - min-pxcor) ^ 2) + ((max-pycor - min-pycor) ^ 2)) / 2)

  ;;; Ordered list of soil texture types used for visualisation
  ;;; this order corresponds to an approximation to the soil texture palette (red: sand, green: silt, blue: clay)
  set soil_textureTypes_display (list
    "Sand"             "Loamy sand"        "Sandy loam"     ; red         orange  brown
    "Loam"             "Silt loam"         "Silt"           ; yellow      green   lime
    "Silty clay loam"  "Silty clay"        "Clay"           ; turquoise   cyan    sky
    "Clay loam"        "Sandy clay"        "Sandy clay loam"; blue        violet  magenta
  )

  ;parameters-check-1 ; in case you want to avoid zeros (optional)

  if (type-of-experiment = "user-defined")
  [
    ;;; load parameters from user interface
    set elev_numProtuberances par_elev_numProtuberances
    set elev_numDepressions par_elev_numDepressions

    set elev_numRanges par_elev_numRanges
    set elev_rangeLength round ( par_elev_rangeLength * maxDist)
    set elev_rangeHeight par_elev_rangeHeight
    set elev_rangeAggregation par_elev_rangeAggregation

    set elev_numRifts par_elev_numRifts
    set elev_riftLength round ( par_elev_riftLength * maxDist)
    set elev_riftHeight par_elev_riftHeight
    set elev_riftAggregation par_elev_riftAggregation

    set elev_noise par_elev_noise

    set elev_featureAngleRange par_elev_featureAngleRange

    set elev_inversionIterations par_elev_inversionIterations

    set elev_smoothStep par_elev_smoothStep
    set elev_smoothingRadius par_elev_smoothingRadius * maxDist

    set elev_xSlope par_elev_xSlope
    set elev_ySlope par_elev_ySlope

    set elev_valleyAxisInclination par_elev_valleyAxisInclination
    set elev_valleySlope par_elev_valleySlope

    set flow_riverAccumulationAtStart par_flow_riverAccumulationAtStart

    set soil_formativeErosionRate par_soil_formativeErosionRate

    set soil_minDepth par_soil_minDepth
    set soil_maxDepth par_soil_maxDepth

    set soil_depthNoise par_soil_depthNoise

    set soil_min%sand par_soil_min%sand
    set soil_max%sand par_soil_max%sand

    set soil_min%silt par_soil_min%silt
    set soil_max%silt par_soil_max%silt

    set soil_min%clay par_soil_min%clay
    set soil_max%clay par_soil_max%clay

    set soil_textureNoise par_soil_textureNoise
  ]

  if (type-of-experiment = "random") ; TODO
  [
    ;;; get random values within an arbitrary (reasonable) range of values
    ;;; this depends on what type and scale of terrain you want
    ;;; Here, our aim is to create inland/coastal, plain, small-scale terrains with a general flow running from N to S (e.g., 5km^2 Haryana, India)
    set elev_numProtuberances 1 + random 10
    set elev_numDepressions 1 + random 10

    set elev_numRanges 1 + random 100
    set elev_rangeLength round ( (random-float 100) * maxDist)
    set elev_rangeHeight random-float 50
    set elev_rangeAggregation random-float 1

    set elev_numRifts 1 + random 100
    set elev_riftLength round ( (random-float 100) * maxDist)
    set elev_riftHeight -1 * random-float 50
    set elev_riftAggregation random-float 1

    set elev_noise random-float 5

    set elev_featureAngleRange random-float 30

    set elev_inversionIterations (random-float 2)

    set elev_smoothStep 1 ; not randomised
    set elev_smoothingRadius 0.1 * maxDist ; not randomised

    set elev_xSlope random-float 0.01 ; W depression
    set elev_ySlope random-float 0.01 ; S depression

    set elev_valleyAxisInclination random-float 1
    set elev_valleySlope random-float 0.02 ; only valley (no ridges)

    set flow_riverAccumulationAtStart random 2E6

    set soil_formativeErosionRate random-float 3

    set soil_minDepth 50 + random-float 250
    set soil_maxDepth soil_minDepth + random-float 300

    set soil_depthNoise random-float 50

    set soil_min%sand random-float 100
    set soil_max%sand soil_min%sand + random-float (100 - soil_min%sand)

    set soil_min%silt random-float 100
    set soil_max%silt soil_min%silt + random-float (100 - soil_min%silt)

    set soil_min%clay random-float 100
    set soil_max%clay soil_min%clay + random-float (100 - soil_min%clay)

    set soil_textureNoise random-float 10
  ]
  if (type-of-experiment = "defined by experiment-number")
  [
    ;load-experiment
  ]

end

to parameters-check-1

  ;;; check if values were reset to 0 (comment out lines if 0 is a valid value)
  ;;; and set default values

  if (par_elev_rangeHeight = 0)                    [ set par_elev_rangeHeight                    15 ]
  if (par_elev_riftHeight = 0)                     [ set par_elev_riftHeight                      0 ]
  if (par_elev_noise = 0)                          [ set par_elev_noise                           1 ]

  if (par_elev_numProtuberances = 0)               [ set par_elev_numProtuberances                1 ]
  if (par_elev_numDepressions = 0)                 [ set par_elev_numDepressions                  1 ]

  if (par_elev_inversionIterations = 0)            [ set par_elev_inversionIterations             5 ]

  if (par_elev_numRanges = 0)                      [ set par_elev_numRanges                       1 ]
  if (par_elev_rangeLength = 0)                    [ set par_elev_rangeLength                   100 ]
  if (par_elev_rangeAggregation = 0)               [ set par_elev_rangeAggregation                0.75 ]

  if (par_elev_numRifts = 0)                       [ set par_elev_numRifts                        1 ]
  if (par_elev_riftLength = 0)                     [ set par_elev_riftLength                    100 ]
  if (par_elev_riftAggregation = 0)                [ set par_elev_riftAggregation                 0.9 ]

  if (par_elev_smoothStep = 0)                     [ set par_elev_smoothStep                      1 ]
  if (par_elev_smoothingRadius = 0)                [ set par_elev_smoothingRadius                 0.1 ]

  if (par_elev_xSlope = 0)                          [ set par_elev_xSlope                         0.01 ]
  if (par_elev_ySlope = 0)                          [ set par_elev_ySlope                         0.025 ]
  if (par_elev_valleyAxisInclination = 0)           [ set par_elev_valleyAxisInclination          0.1 ]
  if (par_elev_valleySlope = 0)                     [ set par_elev_valleySlope                    0.02 ]

  if (par_flow_riverAccumulationAtStart = 0)     [ set par_flow_riverAccumulationAtStart        1E6 ]

  if (soil_formativeErosionRate = 0)             [ set soil_formativeErosionRate                  2 ]

  if (par_soil_minDepth = 0)                    [ set par_soil_minDepth                         300 ]
  if (par_soil_maxDepth = 0)                    [ set par_soil_maxDepth                         500 ]
  if (par_soil_depthNoise = 0)                  [ set par_soil_depthNoise                        50 ]

  if (par_soil_min%sand = 0)                    [ set par_soil_min%sand                          60 ]
  if (par_soil_max%sand = 0)                    [ set par_soil_max%sand                          90 ]

  if (par_soil_min%silt = 0)                    [ set par_soil_min%silt                          40 ]
  if (par_soil_max%silt = 0)                    [ set par_soil_max%silt                          70 ]

  if (par_soil_min%clay = 0)                    [ set par_soil_min%clay                           0 ]
  if (par_soil_max%clay = 0)                    [ set par_soil_max%clay                          50 ]

  if (par_soil_textureNoise = 0)                [ set par_soil_textureNoise                       5 ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set par_elev_rangeHeight                  15
  set par_elev_riftHeight                    0
  set par_elev_noise                         1

  set par_elev_numProtuberances              1
  set par_elev_numDepressions                1

  set par_elev_inversionIterations           5

  set par_elev_numRanges                     1
  set par_elev_rangeLength                 100
  set par_elev_rangeAggregation              0.75

  set par_elev_numRifts                      1
  set par_elev_riftLength                  100
  set par_elev_riftAggregation               0.9

  set par_elev_smoothStep                    1
  set par_elev_smoothingRadius               0.1

  set par_elev_xSlope                        0.01
  set par_elev_ySlope                        0.025
  set par_elev_valleyAxisInclination         0.1
  set par_elev_valleySlope                   0.02

  set par_flow_riverAccumulationAtStart    1E6

  set soil_formativeErosionRate              2
  set par_soil_minDepth                    300
  set par_soil_maxDepth                    500
  set par_soil_depthNoise                   50

  set par_soil_min%sand                     60
  set par_soil_max%sand                     90

  set par_soil_min%silt                     40
  set par_soil_max%silt                     70

  set par_soil_min%clay                      0
  set par_soil_max%clay                     50

  set par_soil_textureNoise                  5

end

to setup-elevations

  ifelse (elev_algorithm-style = "NetLogo")
  [
    set-landform-NetLogo
  ]
  [
    set-landform-Csharp
  ]

  set-xySlope

  set-valleySlope

end

to set-landform-NetLogo ;[ elev_numRanges elev_rangeLength elev_rangeHeight elev_numRifts elev_riftLength elev_riftHeight inversionIterations smoothingNeighborhood elevationSmoothStep]

  ; Netlogo-like code
  ask n-of elev_numRanges patches [ sprout-mapSetters 1 [ set numPoints random elev_rangeLength ] ]
  ask n-of elev_numRifts patches with [any? turtles-here = false] [ sprout-mapSetters 1 [ set numPoints (random elev_riftLength) * -1 ] ]

  let steps sum [ abs numPoints ] of mapSetters
  repeat steps
  [
    ask one-of mapSetters
    [
      let sign 1
      let scale elev_rangeHeight
      if ( numPoints < 0 ) [ set sign -1 set scale elev_riftHeight ]
      ask patch-here [ set elevation scale ]
      set numPoints numPoints - sign
      if (numPoints = 0) [die]
      rt (random-exponential elev_featureAngleRange) * (1 - random-float 2)
      forward 1
    ]
  ]

  smooth-elevation-all

  let depressedPatches patches with [elevation < 0]
  let elevatedPatches patches with [elevation > 0]

  repeat elev_inversionIterations * count patches
  [
    if (any? depressedPatches AND any? elevatedPatches)
    [
      let p_depression max-one-of depressedPatches [ count neighbors with [elevation > 0] ]
      let p_protuberance  max-one-of elevatedPatches [ count neighbors with [elevation < 0] ]
      let temp [elevation] of p_depression
      ask p_depression [ set elevation [elevation] of p_protuberance ]
      ask p_protuberance [ set elevation temp ]
      set depressedPatches depressedPatches with [pxcor != [pxcor] of p_depression AND pycor != [pycor] of p_depression]
      set elevatedPatches elevatedPatches with [pxcor != [pxcor] of p_protuberance AND pycor != [pycor] of p_protuberance]
    ]
  ]

  smooth-elevation-all

end

to set-landform-Csharp ;[ elev_noise elev_numProtuberances elev_numRanges elev_rangeLength elev_rangeHeight rangeAggregation numDepressions elev_numRifts elev_riftLength elev_riftHeight riftAggregation smoothingNeighborhood elevationSmoothStep]

  ; C#-like code
  let p1 0
  let sign 0
  let len 0
  let elev 0
  let elev_numRiftsToDo elev_numRifts
  let elev_numRangesToDo elev_numRifts

  let protuberances n-of elev_numProtuberances patches
  let depressions n-of elev_numDepressions patches

  let maxDistBetweenRanges (1.1 - elev_rangeAggregation) * maxDist
  let maxDistBetweenRifts (1.1 - elev_riftAggregation) * maxDist

  repeat (elev_numRanges + elev_numRifts)
  [
    set sign -1 + 2 * (random 2)
    if (elev_numRangesToDo = 0) [ set sign -1 ]
    if (elev_numRiftsToDo = 0) [ set sign 1 ]

    ifelse (sign = -1)
    [
      set elev_numRiftsToDo elev_numRiftsToDo - 1
      set len elev_riftLength - 2
      set elev elev_riftHeight
      ;ifelse (any? patches with [elevation < 0]) [set p0 one-of patches with [elevation < 0]] [set p0 one-of patches]
      set p1 one-of patches with [ distance one-of depressions < maxDistBetweenRifts ]
    ]
    [
      set elev_numRangesToDo elev_numRangesToDo - 1
      set len elev_rangeLength - 2
      set elev elev_rangeHeight
      set p1 one-of patches with [ distance one-of protuberances < maxDistBetweenRanges ]
    ]

    draw-elevation-pattern p1 len elev
  ]

  smooth-elevation-all

  ask patches
  [
    set elevation elevation + random-normal 0 elev_noise
  ]

  smooth-elevation-all

end

to draw-elevation-pattern [ p1 len elev ]

  let p2 0
  let xDirection 0
  let yDirection 0
  let directionAngle 0

  ask p1 [ set elevation elev set p2 one-of neighbors ]
  set xDirection ([pxcor] of p2) - ([pxcor] of p1)
  set yDirection ([pycor] of p2) - ([pycor] of p1)
  ifelse (xDirection = 1 AND yDirection = 0) [ set directionAngle 0 ]
  [ ifelse (xDirection = 1 AND yDirection = 1) [ set directionAngle 45 ]
    [ ifelse (xDirection = 0 AND yDirection = 1) [ set directionAngle 90 ]
      [ ifelse (xDirection = -1 AND yDirection = 1) [ set directionAngle 135 ]
        [ ifelse (xDirection = -1 AND yDirection = 0) [ set directionAngle 180 ]
          [ ifelse (xDirection = -1 AND yDirection = -1) [ set directionAngle 225 ]
            [ ifelse (xDirection = 0 AND yDirection = -1) [ set directionAngle 270 ]
              [ if (xDirection = 1 AND yDirection = -1) [ set directionAngle 315 ]]
            ]
          ]
        ]
      ]
    ]
  ]

  repeat len
  [
    set directionAngle directionAngle + (random-exponential elev_featureAngleRange) * (1 - random 2)
    set directionAngle directionAngle mod 360

    set p1 p2
    ask p2
    [
      set elevation elev
      if (patch-at-heading-and-distance directionAngle 1 != nobody) [ set p2 patch-at-heading-and-distance directionAngle 1 ]
    ]
  ]

end

to smooth-elevation-all

  ask patches
  [
    smooth-elevation
  ]

end

to smooth-elevation

  let smoothedElevation mean [elevation] of patches in-radius elev_smoothingRadius
  set elevation elevation + (smoothedElevation - elevation) * elev_smoothStep

end

to set-xySlope

  ask patches
  [
    set elevation (1 - elev_xSlope) * elevation + (elev_xSlope * (elev_rangeHeight - elev_riftHeight) * (pxcor - min-pxcor) / world-width)

    set elevation (1 - elev_ySlope) * elevation + (elev_ySlope * (elev_rangeHeight - elev_riftHeight) * (pycor - min-pycor) / world-height)
  ]

end

to set-valleySlope

  ; bend terrain as a valley (valleySlope > 0) or a ridge (valleySlope < 0) following a North-South pattern
  ask patches
  [
    let xValley (world-width / 2) + elev_valleyAxisInclination * (pycor - (world-height / 2))
    set elevation (1 - elev_valleySlope) * elevation + (elev_valleySlope * (elev_rangeHeight - elev_riftHeight) * abs (xValley - pxcor))
  ]

end

to setup-flows

  if (flow_do-fill-sinks)
  [
    fill-sinks
  ]

  set-flow-directions

  introduce-river-flow

  set-flow-accumulations

  ; set maximum flow accumulation as a reference excluding the flow entering through the river
  set maxFlowAccumulation max [flow_accumulation] of patches with [flow_accumulation < flow_riverAccumulationAtStart]

end

;=======================================================================================================
;;; START of algorithms based on:
;;; Huang, P., Lee, K.T. A simple depression-filling method for raster and irregular elevation datasets.
;;; J Earth Syst Sci 124, 1653–1665 (2015). https://doi.org/10.1007/s12040-015-0641-2
;=======================================================================================================

to fill-sinks

  while [ count patches with [is-sink] > 0 ]
  [
    ask patches with [is-sink]
    [
      ;print (word "before: " elevation)
      set elevation [elevation] of min-one-of neighbors [elevation] + 1E-1
      ; the scale of this "small number" (1E-1) regulates how fast will be the calculation
      ; and how distorted will be the depressless DEM
      ;print (word "after: " elevation)
    ]
  ]

end

to-report is-sink ; ego = patch

  let thisPatch self

  report (not is-at-edge) and (elevation < min [elevation] of neighbors);count neighbors with [elevation < [elevation] of thisPatch] = 0)

end

;=======================================================================================================
;;; START of algorithms based on:
;;; Jenson, S. K., & Domingue, J. O. (1988).
;;; Extracting topographic structure from digital elevation data for geographic information system analysis.
;;; Photogrammetric engineering and remote sensing, 54(11), 1593-1600.
;;; ===BUT used elsewhere, such as in the algorithms based on:
;;; Huang, P., Lee, K.T. A simple depression-filling method for raster and irregular elevation datasets.
;;; J Earth Syst Sci 124, 1653–1665 (2015). https://doi.org/10.1007/s12040-015-0641-2
;=======================================================================================================

to-report get-drop-from [ aPatch ] ; ego = patch

  ; "Distance- weighted drop is calculated by subtracting the neighbor’s value from the center cell’s value
  ; and dividing by the distance from the center cell, √2 for a corner cell and one for a noncorner cell." (p. 1594)

  report ([elevation] of aPatch - elevation) / (distance aPatch)

end

to-report is-at-edge ; ego = patch

  report (pxcor = min-pxcor or pxcor = max-pxcor or pycor = min-pycor or pycor = max-pycor)

end

to-report has-flow-direction-code ; ego = patch

  if (member? flow_direction [ 1 2 4 8 16 32 64 128 ]) [ report true ]

  report false

end

to-report flow-direction-is [ centralPatch ]

  if (flow_direction = get-flow-direction-encoding ([pxcor] of centralPatch - pxcor) ([pycor] of centralPatch - pycor))
  [ report true ]

  report false

end

to-report get-flow-direction-encoding [ x y ]

  if (x = -1 and y = -1) [ report 16 ] ; Southwest
  if (x = -1 and y = 0) [ report 32 ]  ; West
  if (x = -1 and y = 1) [ report 64 ]  ; Northwest

  if (x = 0 and y = -1) [ report 8 ]   ; South
  if (x = 0 and y = 1) [ report 128 ]  ; North

  if (x = 1 and y = -1) [ report 4 ]   ; Southeast
  if (x = 1 and y = 0) [ report 2 ]    ; East
  if (x = 1 and y = 1) [ report 1 ]    ; Northeast

end

to-report get-patch-in-flow-direction [ neighborEncoding ] ; ego = patch

  ; 64 128 1
  ; 32  x  2
  ; 16  8  4

  if (neighborEncoding = 16) [ report patch (pxcor - 1) (pycor - 1) ]
  if (neighborEncoding = 32) [ report patch (pxcor - 1) (pycor) ]
  if (neighborEncoding = 64) [ report patch (pxcor - 1) (pycor + 1) ]

  if (neighborEncoding = 8) [ report patch (pxcor) (pycor - 1) ]
  if (neighborEncoding = 128) [ report patch (pxcor) (pycor + 1) ]

  if (neighborEncoding = 4) [ report patch (pxcor + 1) (pycor - 1) ]
  if (neighborEncoding = 2) [ report patch (pxcor + 1) (pycor) ]
  if (neighborEncoding = 1) [ report patch (pxcor + 1) (pycor + 1) ]

  report nobody

end

to-report flow-direction-is-loop ; ego = patch

  let thisPatch self
  let dowstreamPatch get-patch-in-flow-direction flow_direction
  ;print (word "thisPatch: " thisPatch "dowstreamPatch: " dowstreamPatch)

  if (dowstreamPatch != nobody)
  [ report [flow-direction-is thisPatch] of dowstreamPatch ]

  report false

end

to set-flow-directions

  ask patches
  [
    ifelse (is-at-edge)
    [
      ifelse ( pxcor = min-pxcor )
      [ set flow_direction 32 ] ; west
      [
        ifelse ( pxcor = max-pxcor )
        [ set flow_direction 2 ] ; east
        [
          ifelse ( pycor = min-pycor )
          [ set flow_direction 8 ] ; south
          [ set flow_direction 128 ] ; north
        ]
      ]
    ]
    [
      set-flow-direction
    ]
  ]

end

to set-flow-direction ; ego = patch

  let thisPatch self

  let downstreamPatch max-one-of neighbors [get-drop-from thisPatch]
  set flow_direction get-flow-direction-encoding ([pxcor] of downstreamPatch - pxcor) ([pycor] of downstreamPatch - pycor)

end

to introduce-river-flow

  ; get average elevation of edges
  let southEdgeAverageElevation  mean [elevation] of patches with [pycor = min-pycor]
  let northEdgeAverageElevation  mean [elevation] of patches with [pycor = max-pycor]

  ; find which edge has the highest average elevation
  let highestEdge patches with [pycor = max-pycor] ; assume north
  if (southEdgeAverageElevation > northEdgeAverageElevation)
  [ set highestEdge patches with [pycor = min-pycor] ] ; change to south

  ; give value (flow_riverAccumulationAtStart) of flow_accumulation to the lowest patch at that edge
  ; and assign it an inward flowDirection
  ask min-one-of highestEdge [elevation] ; a patch at the bottom of a valley
  [
    set flow_accumulation flow_riverAccumulationAtStart
    let downstreamPatch min-one-of neighbors with [not is-at-edge] [elevation]
    set flow_Direction get-flow-direction-encoding ([pxcor] of downstreamPatch - pxcor) ([pycor] of downstreamPatch - pycor)
  ]

end

to set-flow-accumulations

  ; From Jenson, S. K., & Domingue, J. O. (1988), p. 1594
  ; "FLOW ACCUMULATION DATA SET
  ; The third procedure of the conditioning phase makes use of the flow direction data set to create the flow accumulation data set,
  ; where each cell is assigned a value equal to the number of cells that flow to it (O’Callaghan and Mark, 1984).
  ; Cells having a flow accumulation value of zero (to which no other cells flow) generally correspond to the pattern of ridges.
  ; Because all cells in a depressionless DEM have a path to the data set edge, the pattern formed by highlighting cells
  ; with values higher than some threshold delineates a fully connected drainage network."

  ; identify patches that receive flow and those that do not (this makes the next step much easier)
  ask patches
  [
    set flow_receive false
    set flow_accumulationState "start"
    ;set pcolor red
  ]

  ask patches with [has-flow-direction-code]
  [
    let patchInFlowDirection get-patch-in-flow-direction flow_direction
    if (patchInFlowDirection != nobody)
    [
      ask patchInFlowDirection
      [
        set flow_receive true
        set flow_accumulationState "pending"
        ;set pcolor yellow
      ]
    ]
  ]

  let maxIterations 100000 ; just as a safety measure, to avoid infinite loop
  while [count patches with [flow_accumulationState = "pending" and not flow-direction-is-loop] > 0 and maxIterations > 0 and count patches with [flow_accumulationState = "start"] > 0 ]
  [
    ask one-of patches with [flow_accumulationState = "start"]
    [
      let downstreamPatch get-patch-in-flow-direction flow_direction
      let nextFlow_accumulation flow_accumulation + 1

      set flow_accumulationState "done"
      ;set pcolor orange

      if (downstreamPatch != nobody)
      [
        ask downstreamPatch
        [
          set flow_accumulation flow_accumulation + nextFlow_accumulation
          if (count neighbors with [
            get-patch-in-flow-direction flow_direction = downstreamPatch and
            (flow_accumulationState = "pending" or flow_accumulationState = "start")
            ] = 0
          )
          [
            set flow_accumulationState "start"
            ;set pcolor red
          ]
        ]
      ]
    ]

    set maxIterations maxIterations - 1
  ]

end

;=======================================================================================================
;;; END of algorithms based on:
;;; Jenson, S. K., & Domingue, J. O. (1988).
;;; Extracting topographic structure from digital elevation data for geographic information system analysis.
;;; Photogrammetric engineering and remote sensing, 54(11), 1593-1600.
;;; ===BUT used in the algorithms based on:
;;; Huang P C and Lee K T 2015
;;; A simple depression-filling method for raster and irregular elevation datasets
;;; J. Earth Syst. Sci. 124 1653–65
;=======================================================================================================

to setup-soil

  ask patches
  [
    setup-soil-formative-erosion

    setup-soil-depth

    setup-soil-texture
  ]

end

to setup-soil-formative-erosion

  set p_soil_formativeErosion get-soil-formative-erosion flow_accumulation

end

to-report get-soil-formative-erosion [ flowAccumulation ]

  report get-value-in-sigmoid (flowAccumulation / maxFlowAccumulation) soil_formativeErosionRate

end

to setup-soil-depth

  set p_soil_depth max (list 0
    (get-soil-depth p_soil_formativeErosion
    + (random-normal 0 soil_depthNoise))
    )

end

to-report get-soil-depth [ soil_formativeErosion ]

  ;;; Following the same rationale of soil texture, soil depth is positively related to p_soil_erosion.
  ;;; Soil particles are derived from the parent geological material, eroded by environmental factors, and assumingly accumulate more downhill following the flow path.
  report soil_minDepth + (soil_maxDepth - soil_minDepth) * soil_formativeErosion

end

to setup-soil-texture

  ;;; assign % of sand, silt and clay according to flowAccumulation, min/max, and noise parameters
  ;;; NOTE: %sand decrease with flowAccumulation while %silt and %clay increase
  ;;; useful reference: https://www.earthonlinemedia.com/ebooks/tpe_3e/soil_systems/soil__development_soil_forming_factors.html

  set p_soil_%sand clampMinMax (
    get-soil-%sand p_soil_formativeErosion    ; as function of p_soil_formativeErosion
    + (random-normal 0 soil_textureNoise))    ; add some random variation
    0 100                                     ; clampMinMax <value> 0 100 -- keeps value within range of 0-100, after adding normal noise

  set p_soil_%silt clampMinMax (
    get-soil-%silt p_soil_formativeErosion
    + (random-normal 0 soil_textureNoise))
    0 100

  set p_soil_%clay clampMinMax (
    get-soil-%clay p_soil_formativeErosion
    + (random-normal 0 soil_textureNoise))
    0 100

  ;;; normalise values to sum up 100 %
  let total p_soil_%sand + p_soil_%silt + p_soil_%clay

  set p_soil_%sand 100 * p_soil_%sand / total
  set p_soil_%silt 100 * p_soil_%silt / total
  set p_soil_%clay 100 * p_soil_%clay / total

  ;;; get soil texture type according to sand/silt/clay composition
  set p_soil_textureType get-soil-texture-type (p_soil_%sand) (p_soil_%silt) (p_soil_%clay)

end

;;; NOTE on soil texture formation:
;;; it might be possible to reduce the number of parameters related to soil texture by:
;;; - using a single erosion curve parameter (must consult with pedologist)

to-report get-soil-%sand [ soil_erosion ]

  ;;; %sand is negatively related to flow accumulation. Sand is the coarser fraction of particles derived from the parent geological material
  ;;; that will assumingly erode progressively into finer particles down the flow path through physical and chemical processes.
  report soil_min%sand + (soil_max%sand - soil_min%sand) * (1 - soil_erosion)

end

to-report get-soil-%silt [ soil_erosion ]

  ;;; %silt is positively related to flow accumulation. Silt is the intermediate, finer fraction of particles derived from sand and, ultimately,
  ;;; the parent geological material. Thus, silt is being accumulated through the erosion of sand by environmental factors, assumingly following the flow path.
  report soil_min%silt + (soil_max%silt - soil_min%silt) * soil_erosion

end

to-report get-soil-%clay [ soil_erosion ]

  ;;; %clay is positively related to flow accumulation. Clay is the finest fraction of particles derived from sand, silt, and, ultimately,
  ;;; the parent geological material. Thus, clay is being accumulated through the erosion of coarser particles by environmental factors, assumingly following the flow path.
  report soil_min%clay + (soil_max%clay - soil_min%clay) * soil_erosion

end

to-report get-soil-texture-type [ %sand %silt %clay ]

  ;;; based on ternary plot textural soil classification by the United States Department of Agriculture (USDA)
  ;;; See: Unknown-Author, Soil Mechanics Level 1, Module 3, USDA Textural Classification Study Guide,
  ;;; United States Department of Agriculture, 1987.

  if ((%sand > 85) and (%silt <= 15) and (%clay <= 10)) [ report "Sand" ]

  if ((%sand > 70 and %sand <= 90) and (%silt <= 30) and (%clay <= 15)) [ report "Loamy sand" ]

  if ((%sand > 42.5 and %sand <= 85) and (%silt <= 50) and (%clay <= 20)) [ report "Sandy loam" ]

  if ((%sand > 45) and (%silt <= 27.5) and (%clay > 20 and %clay <= 35)) [ report "Sandy clay loam" ]

  if ((%sand > 45) and (%silt <= 20) and (%clay > 35 and %clay <= 55)) [ report "Sandy clay" ]

  if ((%sand <= 45) and (%silt <= 40) and (%clay > 40)) [ report "Clay" ]

  if ((%sand <= 20) and (%silt > 40 and %silt <= 60) and (%clay > 40 and %clay <= 60)) [ report "Silty clay" ]

  if ((%sand <= 20) and (%silt > 40 and %silt <= 72.5) and (%clay > 27.5 and %clay <= 40)) [ report "Silty clay loam" ]

  if ((%sand <= 50) and (%silt > 50 and %silt <= 87.5) and (%clay <= 27.5)) [ report "Silt loam" ]

  if ((%sand <= 20) and (%silt > 80) and (%clay <= 12.5)) [ report "Silt" ]

  if ((%sand > 20 and %sand <= 45) and (%silt > 15 and %silt <= 52.5) and (%clay > 27.5 and %clay <= 40)) [ report "Clay loam" ]

  if ((%sand > 22.5 and %sand <= 52.5) and (%silt > 27.5 and %silt <= 50) and (%clay > 7 and %clay <= 27.5)) [ report "Loam" ]

  report ""

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT STATS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-output-stats

  set elevationDistribution [elevation] of patches

  set minElevation min [elevation] of patches

  set maxElevation max [elevation] of patches

  set sdElevation standard-deviation [elevation] of patches

  ;;; default seaLevel to minElevation (seaLevel afects patch color and landRatio measurement)
  set par_seaLevel (floor minElevation) - 1
  set seaLevel par_seaLevel

  set landRatio count patches with [elevation >= seaLevel] / count patches

  set landWithRiver count patches with [flow_accumulation >= flow_riverAccumulationAtStart]

  set mostCommonTextureType modes [p_soil_textureType] of patches

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to paint-patches

  ;;; several soil properties must be rescaled to enhance visualisation
  ;;; (the parametric max and min values of some of these are never realised for various reasons)

  if (display-mode = "elevation (m)")
  [
    ask patches
    [
      set pcolor get-elevation-color elevation
    ]
    set-legend-elevation 10
  ]
  if (display-mode = "soil formative erosion")
  [
    ask patches [ set pcolor 8 - 6 * p_soil_formativeErosion ]
    set-legend-continuous-range 1 0 8 2 6 false
  ]
  if (display-mode = "soil depth (mm)")
  [
    let mindepth min [p_soil_depth] of patches
    let maxdepth max [p_soil_depth] of patches

    ask patches [ set pcolor 38 - 6 * (p_soil_depth - mindepth) / (maxdepth - mindepth) ]
    set-legend-continuous-range maxdepth mindepth 38 32 6 false
  ]
  if (display-mode = "soil texture")
  [
    let min%sand min [p_soil_%sand] of patches
    let max%sand max [p_soil_%sand] of patches
    let min%silt min [p_soil_%silt] of patches
    let max%silt max [p_soil_%silt] of patches
    let min%clay min [p_soil_%clay] of patches
    let max%clay max [p_soil_%clay] of patches

    ask patches
    [
      ;;; red: sand, green: silt, blue: clay
      set pcolor get-texture-color (list p_soil_%sand min%sand max%sand) (list p_soil_%silt min%silt max%silt) (list p_soil_%clay min%clay max%clay)
    ]
    set-legend-soil-texture (list min%sand max%sand) (list min%silt max%silt) (list min%clay max%clay)
  ]
  if (display-mode = "soil texture types")
  [
    ask patches
    [
      set pcolor get-textureType-color p_soil_textureType
    ]
    set-legend-soil-textureType
  ]
  ;;; other modes of display can be added here

  display-flows

end

to-report get-elevation-color [ elevationValue ]

  let elevationGradient 0

  ifelse (elevationValue < seaLevel)
  [
    let normSubElevation (-1) * (seaLevel - elevationValue)
    let normSubMinElevation (-1) * (seaLevel - minElevation) + 1E-6
    set elevationGradient 20 + (200 * (1 - normSubElevation / normSubMinElevation))
    report rgb 0 0 elevationGradient
  ]
  [
    let normSupElevation elevationValue - seaLevel
    let normSupMaxElevation maxElevation - seaLevel + 1E-6
    set elevationGradient 100 + (155 * (normSupElevation / normSupMaxElevation))
    report rgb (elevationGradient - 100) elevationGradient 0
  ]

end

to-report get-texture-color [ %sandData %siltData %clayData ]

  report rgb
        (240 * (((item 0 %sandData) - (item 1 %sandData)) / ((item 2 %sandData) - (item 1 %sandData))))
        (240 * (((item 0 %siltData) - (item 1 %siltData)) / ((item 2 %siltData) - (item 1 %siltData))))
        (240 * (((item 0 %clayData) - (item 1 %clayData)) / ((item 2 %clayData) - (item 1 %clayData))))
      ;;; with range fixed at 0-100
; report rgb
;        (240 * ((item 0 %sandData) / 100))
;        (240 * ((item 0 %siltData) / 100))
;        (240 * ((item 0 %clayData) / 100))

end

to-report get-textureType-color [ textureTypeName ]

  report 15 + 10 * (position textureTypeName soil_textureTypes_display)

end

to set-legend-elevation [ numberOfKeys ]

  set-current-plot "Legend"

  clear-plot

  let step precision ((maxElevation - minElevation) / numberOfKeys) 4

  let value maxElevation

  while [ value > minElevation ]
  [
    create-temporary-plot-pen (word "" (precision value 4) "")
    set-plot-pen-color get-elevation-color value
    set value value - step
  ]

end

to set-legend-continuous-range [ maximum minimum maxShade minShade numberOfKeys ascendingOrder? ]

  set-current-plot "Legend"

  clear-plot

  let step precision ((maximum - minimum) / numberOfKeys) 4

  ifelse (ascendingOrder?)
  [
    let value precision minimum 4

    while [ value < maximum ]
    [
      create-temporary-plot-pen (word "" (precision value 4) "")
      set-plot-pen-color minShade + (maxShade - minShade) * (value - minimum) / (maximum - minimum)
      set value value + step
    ]
  ]
  [
    let value precision maximum 4

    while [ value > minimum ]
    [
      create-temporary-plot-pen (word "" (precision value 4) "")
      set-plot-pen-color maxShade - (maxShade - minShade) * (value - minimum) / (maximum - minimum)
      set value value - step
    ]
  ]

end

to set-legend-soil-textureType

  set-current-plot "Legend"

  clear-plot

  foreach soil_textureTypes_display
  [
    textureTypeName ->
    create-temporary-plot-pen textureTypeName
    set-plot-pen-color get-textureType-color textureTypeName
  ]

end

to set-legend-soil-texture [ %sandRange %siltRange %clayRange ]

  set-current-plot "Legend"

  clear-plot

  ;;; red: sand, green: silt, blue: clay
  create-temporary-plot-pen (word "max %sand = " round (item 1 %sandRange) )
  set-plot-pen-color get-texture-color (list (item 1 %sandRange) (item 0 %sandRange) (item 1 %sandRange))
                                       (list (item 0 %siltRange) (item 0 %siltRange) (item 1 %siltRange))
                                       (list (item 0 %clayRange) (item 0 %clayRange) (item 1 %clayRange))
  create-temporary-plot-pen (word "max %silt = " round (item 1 %siltRange) )
  set-plot-pen-color get-texture-color (list (item 0 %sandRange) (item 0 %sandRange) (item 1 %sandRange))
                                       (list (item 1 %siltRange) (item 0 %siltRange) (item 1 %siltRange))
                                       (list (item 0 %clayRange) (item 0 %clayRange) (item 1 %clayRange))
  create-temporary-plot-pen (word "max %clay = " round (item 1 %clayRange) )
  set-plot-pen-color get-texture-color (list (item 0 %sandRange) (item 0 %sandRange) (item 1 %sandRange))
                                       (list (item 0 %siltRange) (item 0 %siltRange) (item 1 %siltRange))
                                       (list (item 1 %clayRange) (item 0 %clayRange) (item 1 %clayRange))

end

to display-flows

  if (not any? flowHolders)
  [
    ask patches [ sprout-flowHolders 1 [ set hidden? true ] ]
  ]

  ifelse (show-flows)
  [
    ask patches ;with [ has-flow-direction-code ]
    [
      let flow_directionHere flow_direction
      let nextPatchInFlow get-patch-in-flow-direction flow_direction
      let flow_accumulationHere flow_accumulation

      ask one-of flowHolders-here
      [
        ifelse (nextPatchInFlow != nobody)
        [
          if (link-with one-of [flowHolders-here] of nextPatchInFlow = nobody)
          [ create-link-with one-of [flowHolders-here] of nextPatchInFlow ]

          ask link-with one-of [flowHolders-here] of nextPatchInFlow
          [
            set hidden? false
            let multiplier 1E100 ^ (1 - flow_accumulationHere / (max [flow_accumulation] of patches)) / 1E100
            set color 92 + (5 * multiplier)
            set thickness 0.4 * ( 1 - ((color - 92) / 5))
          ]
        ]
        [
          set hidden? false
          let multiplier 1E100 ^ (1 - flow_accumulationHere / (max [flow_accumulation] of patches)) / 1E100
          set color 92 + (5 * multiplier)
          if (color <= 97) [ set shape "line half" ]
          if (color < 95) [ set shape "line half 1" ]
          if (color < 93) [ set shape "line half 2" ]
          set heading get-angle-in-flow-direction flow_direction
        ]
      ]
    ]
  ]
  [
    ask flowHolders
    [
      set hidden? true
      ask my-links [ set hidden? true ]
    ]
  ]

end

to-report get-angle-in-flow-direction [ neighborEncoding ]

  ; 64 128 1
  ; 32  x  2
  ; 16  8  4

  if (neighborEncoding = 16) [ report 225 ]
  if (neighborEncoding = 32) [ report 270 ]
  if (neighborEncoding = 64) [ report 315 ]

  if (neighborEncoding = 8) [ report 180 ]
  if (neighborEncoding = 128) [ report 0 ]

  if (neighborEncoding = 4) [ report 135 ]
  if (neighborEncoding = 2) [ report 90 ]
  if (neighborEncoding = 1) [ report 45 ]

  report nobody

end

to refresh-view

  update-plots

  paint-patches

end

to refresh-view-after-seaLevel-change

  set seaLevel par_seaLevel

  set landRatio count patches with [elevation >= seaLevel] / count patches

  update-plots

  paint-patches

end

to setup-patch-coordinates-labels [ XcoordPosition YcoordPosition ]

  let xspacing floor (world-width / patch-size)
  let yspacing floor (world-height / patch-size)

  ifelse (XcoordPosition = "bottom")
  [
    ask patches with [ pycor = min-pycor + 1 ]
    [
      if (pxcor mod xspacing = 0)
      [ set plabel (word pxcor) ]
    ]
  ]
  [
    ask patches with [ pycor = max-pycor - 1 ]
    [
      if (pxcor mod xspacing = 0)
      [ set plabel (word pxcor) ]
    ]
  ]

  ifelse (YcoordPosition = "left")
  [
    ask patches with [ pxcor = min-pxcor + 1 ]
    [
      if (pycor mod yspacing = 0)
      [ set plabel (word pycor) ]
    ]
  ]
  [
    ask patches with [ pycor = max-pycor - 1 ]
    [
      if (pycor mod yspacing = 0)
      [ set plabel (word pycor) ]
    ]
  ]

end

to setup-transect

  ask patches with [ pxcor = xTransect ]
  [
    sprout-transectLines 1 [ set shape "line" set heading 0 set color white ]
  ]

  ask patches with [ pycor = yTransect ]
  [
    sprout-transectLines 1 [ set shape "line" set heading 90 set color white ]
  ]

  if (not show-transects)
  [
    ask transectLines [ set hidden? true ]
  ]

end

to update-transects

  ifelse (show-transects)
  [
    ask transectLines
    [
      ifelse (heading = 0) [ set xcor xTransect ] [ set ycor yTransect ]
      set hidden? false
    ]
  ]
  [
    ask transectLines [ set hidden? true ]
  ]

end

to plot-horizontal-transect

  foreach (n-values world-width [ j -> min-pxcor + j ])
  [
    x ->
    plotxy x ([elevation] of patch x yTransect)
  ]
  plot-pen-up

end

to plot-sea-level-horizontal-transect

  foreach (n-values world-width [ j -> min-pxcor + j ])
  [
    x ->
    plotxy x seaLevel
  ]
  plot-pen-up

end

to plot-vertical-transect

  foreach (n-values world-height [ j -> min-pycor + j ])
  [
    y ->
    plotxy ([elevation] of patch xTransect y) y
  ]
  plot-pen-up

end

to plot-sea-level-vertical-transect

  foreach (n-values world-height [ j -> min-pycor + j ])
  [
    y ->
    plotxy seaLevel y
  ]
  plot-pen-up

end

to-report get-variable-per-flowAccumulation [ variableName ]

  let stepInSequence 1;maxFlowAccumulation / 100
  let lengthOfSequence maxFlowAccumulation
  let sequence (list)

  if (variableName = "Depth")
  [
    foreach (n-values lengthOfSequence [ j -> j + stepInSequence ])
    [
      i ->
      set sequence lput (get-soil-depth (get-soil-formative-erosion i)) sequence
    ]
  ]
  if (variableName = "Sand")
  [
    foreach (n-values lengthOfSequence [ j -> j + stepInSequence ])
    [
      i ->
      set sequence lput (get-soil-%sand (get-soil-formative-erosion i)) sequence
    ]
  ]
  if (variableName = "Silt")
  [
    foreach (n-values lengthOfSequence [ j -> j + stepInSequence ])
    [
      i ->
      set sequence lput (get-soil-%silt (get-soil-formative-erosion i)) sequence
    ]
  ]
  if (variableName = "Clay")
  [
    foreach (n-values lengthOfSequence [ j -> j + stepInSequence ])
    [
      i ->
      set sequence lput (get-soil-%clay (get-soil-formative-erosion i)) sequence
    ]
  ]

  report sequence

end

to plot-table [ values ]

  let j 0
  foreach values
  [
    i ->
    plotxy j i
    set j j + 1
  ]
  plot-pen-up

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE HANDLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to export-random-terrain

  set type-of-experiment "random"

  set randomSeed randomSeed + 1 ; this allows for creating multiple terrains when executing this procedure continuosly

  create-terrain

  export-terrain

end

to export-terrain

  ;;; make sure that display-mode is "elevation (m)" in order to avoid complications with patch colors (number vs rgb)
  if (display-mode != "elevation (m)") [ set display-mode "elevation (m)" refresh-view ]

  set show-transects false

  update-transects

  ;;; build a file name as unique to this setting as possible
  let filePath (word "terrains//terrain_" type-of-experiment "_w=" world-width "_h=" world-height "_a=" elev_algorithm-style "_fill-sinks=" flow_do-fill-sinks "_seed=" randomSeed)

  if (type-of-experiment = "user-defined") [ set filePath (word filePath "_" random 9999) ]
  ;if (type-of-experiment = "defined by expNumber") [set filePath (word filePath "_" expNumber) ]

  ;print filePath print length filePath ; de-bug print

;;; check that filePath does not exceed 100 (not common in this context)
  if (length filePath > 100) [ print "WARNING: file path may be too long, depending on your current directory. Decrease length of file name or increase the limit." set filePath substring filePath 0 100 ]

  let filePathCSV (word filePath ".csv")

  let filePathPNG (word filePath ".png")

  export-view filePathPNG
  export-world filePathCSV

end

to import-terrain

  clear-all

  ;;; load a terrain from the "terrains" folder
  ;;; corresponding to the random seed given as a parameter in the interface

  ;;; build a unique file name according to the user setting
  let filePath (word "terrains//terrain_" type-of-experiment "_w=" world-width "_h=" world-height "_a=" elev_algorithm-style "_fill-sinks=" flow_do-fill-sinks "_seed=" randomSeed)

  if (type-of-experiment = "user-defined") [ set filePath (word filePath "_" date-and-time) ]
  ;if (type-of-experiment = "defined by expNumber") [set filePath (word filePath "_" expNumber) ]

  ;;; check that filePath does not exceed 100 (not common in this context)
  if (length filePath > 100) [ print "WARNING: file path may be too long, depending on your current directory. Decrease length of file name or increase the limit." set filePath substring filePath 0 100 ]

  set filePath (word filePath ".csv")

  ifelse (not file-exists? filePath)
  [ print (word "WARNING: could not find '" filePath "'") ]
  [
    file-open filePath

    while [not file-at-end?]
    [
      let thisLine csv:from-row file-read-line

      if (item 0 thisLine = "GLOBALS")
      [
        ;;; read and set basic NetLogo globals
        let globalNames csv:from-row file-read-line
        let globalValues csv:from-row file-read-line

        ;;; apply world dimensions
        resize-world (item 0 globalValues) (item 1 globalValues) (item 2 globalValues) (item 3 globalValues)

        ;;; read relevant globals searching for specific names
        foreach (n-values length(globalValues) [j -> j])
        [
          globalIndex ->

          if (item globalIndex globalNames = "display-mode") [ set display-mode read-from-string item globalIndex globalValues ]

          if (item globalIndex globalNames = "sealevel") [ set seaLevel item globalIndex globalValues ]

          if (item globalIndex globalNames = "elev_algorithm-style") [ set elev_algorithm-style read-from-string item globalIndex globalValues ]

          if (item globalIndex globalNames = "flow_do-fill-sinks") [ set flow_do-fill-sinks item globalIndex globalValues ]

          if (item globalIndex globalNames = "elev_numprotuberances") [ set elev_numProtuberances item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_numdepressions") [ set elev_numDepressions item globalIndex globalValues ]

          if (item globalIndex globalNames = "elev_numranges") [ set elev_numRanges item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_rangelength") [ set elev_rangeLength item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_rangeheight") [ set elev_rangeHeight item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_rangeaggregation") [ set elev_rangeAggregation item globalIndex globalValues ]

          if (item globalIndex globalNames = "elev_numrifts") [ set elev_numRifts item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_riftlength") [ set elev_riftLength item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_riftheight") [ set elev_riftHeight item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_riftaggregation") [ set elev_riftAggregation item globalIndex globalValues ]

          if (item globalIndex globalNames = "elev_featureanglerange") [ set elev_featureAngleRange item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_inversioniterations") [ set elev_inversionIterations item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_noise") [ set elev_noise item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_smoothstep") [ set elev_smoothStep item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_smoothingradius") [ set elev_smoothingradius item globalIndex globalValues ]

          if (item globalIndex globalNames = "elev_xslope") [ set elev_xSlope item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_yslope") [ set elev_ySlope item globalIndex globalValues ]

          if (item globalIndex globalNames = "elev_valleyaxisinclination") [ set elev_valleyAxisInclination item globalIndex globalValues ]
          if (item globalIndex globalNames = "elev_valleyslope") [ set elev_valleySlope item globalIndex globalValues ]

          if (item globalIndex globalNames = "flow_riveraccumulationatstart") [ set flow_riverAccumulationAtStart item globalIndex globalValues ]

          if (item globalIndex globalNames = "soil_formativeerosionrate") [ set soil_formativeErosionRate item globalIndex globalValues ]

          if (item globalIndex globalNames = "soil_mindepth") [ set soil_minDepth item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_maxdepth") [ set soil_maxDepth item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_depthnoise") [ set soil_depthNoise item globalIndex globalValues ]

          if (item globalIndex globalNames = "soil_min%sand") [ set soil_min%sand item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_max%sand") [ set soil_max%sand item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_min%silt") [ set soil_min%silt item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_max%silt") [ set soil_max%silt item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_min%clay") [ set soil_min%clay item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_max%clay") [ set soil_max%clay item globalIndex globalValues ]

          if (item globalIndex globalNames = "soil_texturenoise") [ set soil_textureNoise item globalIndex globalValues ]

          if (item globalIndex globalNames = "soil_texturetypes_display") [ set soil_textureTypes_display read-from-string item globalIndex globalValues ]
        ]
      ]

      if (item 0 thisLine = "TURTLES")
      [
        set thisLine csv:from-row file-read-line ;;; skip variable names
        set thisLine csv:from-row file-read-line ;;; first row of data

        ;;; create a auxiliar turtles
        while [ length thisLine > 1 ]
        [
          if (item 8 thisLine = "{breed flowholders}")
          [
            create-flowHolders 1
            [
              set xcor item 3 thisLine
              set ycor item 4 thisLine
              set hidden? item 9 thisLine
              if (xcor = max-pxcor or xcor = min-pxcor or ycor = max-pycor or ycor = min-pycor)
              [
                set color item 1 thisLine
                set heading item 2 thisLine
                set shape read-from-string item 5 thisLine
              ]
            ]
          ]
          set thisLine csv:from-row file-read-line
        ]
      ]

      if (item 0 thisLine = "PATCHES")
      [
        let patchVarsNames csv:from-row file-read-line ;;; save variable names
        set thisLine csv:from-row file-read-line ;;; first row of data

        ;;; load patch variables per each patch
        while [ length thisLine > 1 ]
        [
          ask patch (item 0 thisLine) (item 1 thisLine)
          [
            let colorRGBValues read-from-string (item 2 thisLine)
            set pcolor rgb (item 0 colorRGBValues) (item 1 colorRGBValues) (item 2 colorRGBValues)

            set elevation item 5 thisLine
            set flow_direction item 6 thisLine
            set flow_receive item 7 thisLine
            set flow_accumulation item 8 thisLine
            set flow_accumulationstate read-from-string item 9 thisLine
            set p_soil_formativeErosion item 10 thisLine
            set p_soil_depth item 11 thisLine
            set p_soil_%sand item 12 thisLine
            set p_soil_%silt item 13 thisLine
            set p_soil_%clay item 14 thisLine
            set p_soil_textureType read-from-string item 15 thisLine
          ]
          set thisLine csv:from-row file-read-line
        ]
      ]

      if (item 0 thisLine = "LINKS")
      [
        set thisLine csv:from-row file-read-line ;;; skip variable names

        set thisLine csv:from-row file-read-line ;;; first row of data

        ;;; create links
        while [ length thisLine > 1 ]
        [
          let flowHolderEnd1 flowHolder get-flowHolder-who-from-link-data (item 0 thisLine)
          let flowHolderEnd2 flowHolder get-flowHolder-who-from-link-data (item 1 thisLine)

          ask flowHolderEnd1
          [
            create-link-with flowHolderEnd2
            [
              set color item 2 thisLine
              set thickness item 7 thisLine
            ]
          ]
          set thisLine csv:from-row file-read-line
        ]
      ]
    ]
    file-close
  ]

  set-output-stats

end

to-report get-flowHolder-who-from-link-data [ linkDataEntry ]

  let str remove "{" linkDataEntry
  set str remove "f" str
  set str remove "l" str
  set str remove "o" str
  set str remove "w" str
  set str remove "h" str
  set str remove "d" str
  set str remove "e" str
  set str remove "r" str
  set str remove " " str
  report read-from-string remove "}" str

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; numeric generic functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report clamp01 [ value ]
  report min (list 1 (clampMin0 value))
end

to-report clampMin0 [ value ]
  report (max (list 0 value))
end

to-report clampMinMax [ value minValue maxValue ]
  report min (list maxValue (max (list minValue value)))
end

to-report get-value-in-sigmoid [ xValue shapeParameter ]

  report 1 - e ^ (-1 * shapeParameter * xValue)

end
@#$#@#$#@
GRAPHICS-WINDOW
728
43
1204
520
-1
-1
9.36
1
10
1
1
1
0
0
0
1
0
49
0
49
0
0
1
ticks
30.0

PLOT
1246
36
1562
522
Legend
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"              Patch color legend                " 1.0 0 -1 true "" "plot 1"

BUTTON
9
10
76
57
create
create-terrain
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

TEXTBOX
167
93
238
118
ELEVATION
11
0.0
1

TEXTBOX
10
332
315
576
---------- used when algorithm-style = C# -------------------------------------------\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|                                                                                                  |\n|___________________________________________________________|
9
0.0
1

TEXTBOX
322
427
488
452
used when algorithm-style = NetLogo
9
0.0
1

MONITOR
559
368
660
413
land ratio
landRatio
4
1
11

SLIDER
492
150
687
183
par_seaLevel
par_seaLevel
round min (list minElevation par_elev_riftHeight)
round max (list maxElevation par_elev_rangeHeight)
-37.0
1
1
m
HORIZONTAL

SLIDER
14
348
186
381
par_elev_noise
par_elev_noise
0
(par_elev_rangeHeight - par_elev_riftHeight) / 2
1.0
1
1
m
HORIZONTAL

SLIDER
6
181
188
214
par_elev_smoothStep
par_elev_smoothStep
0
1
1.0
0.01
1
NIL
HORIZONTAL

INPUTBOX
80
10
156
70
randomSeed
0.0
1
0
Number

INPUTBOX
345
438
468
498
par_elev_inversionIterations
5.0
1
0
Number

MONITOR
564
457
662
502
sdElevation
precision sdElevation 4
4
1
11

MONITOR
527
412
609
457
minElevation
precision minElevation 4
4
1
11

MONITOR
603
412
690
457
maxElevation
precision maxElevation 4
4
1
11

INPUTBOX
307
106
425
166
par_elev_numRanges
1.0
1
0
Number

SLIDER
309
229
516
262
par_elev_rangeLength
par_elev_rangeLength
0
100
100.0
1
1
% patches
HORIZONTAL

INPUTBOX
308
165
425
225
par_elev_numRifts
1.0
1
0
Number

SLIDER
310
262
517
295
par_elev_riftLength
par_elev_riftLength
0
100
100.0
1
1
% patches
HORIZONTAL

SLIDER
15
109
187
142
par_elev_riftHeight
par_elev_riftHeight
-500
0
0.0
1
1
m
HORIZONTAL

BUTTON
486
188
694
221
refresh after changing sea level
refresh-view-after-seaLevel-change
NIL
1
T
OBSERVER
NIL
3
NIL
NIL
1

SLIDER
15
142
187
175
par_elev_rangeHeight
par_elev_rangeHeight
0
500
15.0
1
1
m
HORIZONTAL

MONITOR
523
312
608
357
NIL
count patches
0
1
11

SLIDER
16
469
214
502
par_elev_rangeAggregation
par_elev_rangeAggregation
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
17
502
211
535
par_elev_riftAggregation
par_elev_riftAggregation
0
1
0.9
.01
1
NIL
HORIZONTAL

INPUTBOX
14
380
137
440
par_elev_numProtuberances
1.0
1
0
Number

INPUTBOX
154
380
270
440
par_elev_numDepressions
1.0
1
0
Number

SLIDER
6
214
187
247
par_elev_smoothingRadius
par_elev_smoothingRadius
0
.1
0.1
.01
1
NIL
HORIZONTAL

MONITOR
613
312
678
357
maxDist
precision maxDist 4
4
1
11

MONITOR
40
247
189
284
smoothing neighborhood size
(word (count patches with [ distance patch 0 0 < elev_smoothingRadius ] - 1) \" patches\")
0
1
9

PLOT
320
507
694
627
Elevation per patch
m
NIL
0.0
10.0
0.0
10.0
true
false
"set-histogram-num-bars 100\nset-plot-x-range (round min [elevation] of patches - 1) (round max [elevation] of patches + 1)" "set-histogram-num-bars 100\nset-plot-x-range (round min [elevation] of patches - 1) (round max [elevation] of patches + 1)"
PENS
"default" 1.0 1 -16777216 true "" "histogram [elevation] of patches"
"pen-1" 1.0 1 -2674135 true "" "histogram n-values plot-y-max [j -> seaLevel]"

CHOOSER
322
365
506
410
elev_algorithm-style
elev_algorithm-style
"NetLogo" "C#"
1

SLIDER
9
287
189
320
par_elev_featureAngleRange
par_elev_featureAngleRange
0
360
0.0
1
1
º
HORIZONTAL

SLIDER
15
588
211
621
par_elev_ySlope
par_elev_ySlope
-0.1
0.1
0.025
0.001
1
NIL
HORIZONTAL

CHOOSER
488
66
698
111
display-mode
display-mode
"elevation (m)" "soil formative erosion" "soil depth (mm)" "soil texture" "soil texture types"
0

SLIDER
15
555
211
588
par_elev_xSlope
par_elev_xSlope
-0.1
0.1
0.01
0.001
1
NIL
HORIZONTAL

BUTTON
573
113
645
146
refresh
refresh-view
NIL
1
T
OBSERVER
NIL
2
NIL
NIL
1

PLOT
708
514
1204
634
Horizontal transect
pxcor
m
0.0
10.0
0.0
10.0
true
false
"" "clear-plot\nset-plot-x-range (min-pxcor - 1) (max-pxcor + 1)\nset-plot-y-range (round min [elevation] of patches - 1) (round max [elevation] of patches + 1)"
PENS
"default" 1.0 0 -16777216 true "" "plot-horizontal-transect"
"pen-1" 1.0 0 -13345367 true "" "plot-sea-level-horizontal-transect"
"pen-2" 1.0 0 -2674135 true "" "plotxy xTransect plot-y-max plotxy xTransect plot-y-min"

SLIDER
698
39
731
517
yTransect
yTransect
min-pycor
max-pycor
0.0
1
1
NIL
VERTICAL

SLIDER
725
12
1211
45
xTransect
xTransect
min-pxcor
max-pxcor
0.0
1
1
NIL
HORIZONTAL

PLOT
1203
36
1363
521
vertical transect
m
pycor
0.0
10.0
0.0
10.0
true
false
"" "clear-plot\nset-plot-y-range (min-pycor - 1) (max-pycor + 1)\nset-plot-x-range (round min [elevation] of patches - 1) (round max [elevation] of patches + 1)"
PENS
"default" 1.0 0 -16777216 true "" "plot-vertical-transect"
"pen-1" 1.0 0 -13345367 true "" "plot-sea-level-vertical-transect"
"pen-2" 1.0 0 -2674135 true "" "plotxy  plot-x-max yTransect plotxy plot-x-min yTransect"

BUTTON
1230
579
1330
612
update transects
update-transects\nupdate-plots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1220
545
1345
578
show-transects
show-transects
1
1
-1000

SLIDER
15
623
217
656
par_elev_valleyAxisInclination
par_elev_valleyAxisInclination
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
15
656
210
689
par_elev_valleySlope
par_elev_valleySlope
-0.1
0.1
0.02
0.001
1
NIL
HORIZONTAL

CHOOSER
393
14
527
59
type-of-experiment
type-of-experiment
"random" "user-defined" "defined by expNumber"
0

BUTTON
159
13
269
46
NIL
export-terrain
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
272
13
380
46
import-terrain
import-terrain\nsetup-patch-coordinates-labels \"bottom\" \"left\"\nsetup-transect\nupdate-transects\nupdate-plots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
171
50
367
83
export-random-terrain (100x)
repeat 100 [ export-random-terrain ]
NIL
1
T
OBSERVER
NIL
9
NIL
NIL
1

BUTTON
536
21
688
54
parameters to default
parameters-to-default
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
186
108
263
145
NIL
elev_riftHeight
2
1
9

MONITOR
186
142
273
179
NIL
elev_rangeHeight
2
1
9

MONITOR
185
346
260
383
NIL
elev_noise
2
1
9

MONITOR
188
179
288
216
NIL
elev_smoothStep
2
1
9

MONITOR
186
214
298
251
NIL
elev_smoothingRadius
2
1
9

MONITOR
369
135
457
172
NIL
elev_numRanges
0
1
9

MONITOR
370
195
457
232
NIL
elev_numRifts
0
1
9

MONITOR
335
295
420
332
NIL
elev_rangeLength
0
1
9

MONITOR
420
295
495
332
NIL
elev_riftLength
0
1
9

MONITOR
190
286
294
323
NIL
elev_featureAngleRange
0
1
9

MONITOR
215
552
281
589
NIL
elev_xSlope
4
1
9

MONITOR
217
589
281
626
NIL
elev_ySlope
4
1
9

MONITOR
217
626
336
663
NIL
elev_valleyAxisInclination
4
1
9

MONITOR
216
663
317
700
NIL
elev_valleySlope
4
1
9

MONITOR
15
427
128
464
NIL
elev_numProtuberances
0
1
9

MONITOR
154
425
258
462
NIL
elev_numDepressions
0
1
9

MONITOR
209
465
306
502
NIL
elev_rangeAggregation
4
1
9

MONITOR
210
502
293
539
NIL
elev_riftAggregation
4
1
9

SWITCH
560
237
661
270
flow_do-fill-sinks
flow_do-fill-sinks
0
1
-1000

SWITCH
558
270
662
303
show-flows
show-flows
0
1
-1000

INPUTBOX
335
637
515
697
par_flow_riverAccumulationAtStart
1000000.0
1
0
Number

MONITOR
536
651
676
688
NIL
flow_riverAccumulationAtStart
2
1
9

MONITOR
674
651
753
688
NIL
landWithRiver
6
1
9

SLIDER
401
721
624
754
par_soil_min%sand
par_soil_min%sand
0
par_soil_max%sand - 1
60.0
1.0
1
%
HORIZONTAL

SLIDER
401
753
624
786
par_soil_max%sand
par_soil_max%sand
par_soil_min%sand + 1
100.0
90.0
1.0
1
%
HORIZONTAL

SLIDER
636
721
852
754
par_soil_min%silt
par_soil_min%silt
0
par_soil_max%silt - 1
40.0
1.0
1
%
HORIZONTAL

SLIDER
635
753
851
786
par_soil_max%silt
par_soil_max%silt
par_soil_min%silt + 1
100.0
70.0
1.0
1
%
HORIZONTAL

SLIDER
859
722
1078
755
par_soil_min%clay
par_soil_min%clay
0
par_soil_max%clay - 1
0.0
1.0
1
%
HORIZONTAL

SLIDER
859
754
1079
787
par_soil_max%clay
par_soil_max%clay
par_soil_min%clay + 1
100.0
50.0
1.0
1
%
HORIZONTAL

SLIDER
548
838
743
871
par_soil_textureNoise
par_soil_textureNoise
0.0
20.0
5.0
1.0
1
%
HORIZONTAL

TEXTBOX
711
703
794
721
SOIL TEXTURE
11
0.0
1

PLOT
398
885
1081
1035
Erosion curves of soil formation
flow accumulation
% of soil
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"%sand" 1.0 0 -2674135 true "" "plot-table get-variable-per-flowAccumulation \"Sand\""
"%silt" 1.0 0 -10899396 true "" "plot-table get-variable-per-flowAccumulation \"Silt\""
"%clay" 1.0 0 -13345367 true "" "plot-table get-variable-per-flowAccumulation \"Clay\""

MONITOR
750
837
921
874
soil_textureNoise
precision soil_textureNoise 4
17
1
9

TEXTBOX
173
704
323
722
SOIL DEPTH
11
0.0
1

SLIDER
18
724
196
757
par_soil_minDepth
par_soil_minDepth
0
par_soil_maxDepth - 1
300.0
1
1
mm
HORIZONTAL

SLIDER
18
757
196
790
par_soil_maxDepth
par_soil_maxDepth
par_soil_minDepth + 1
600
500.0
1
1
mm
HORIZONTAL

SLIDER
70
837
311
870
par_soil_formativeErosionRate
par_soil_formativeErosionRate
0
3
0.68
0.01
1
NIL
HORIZONTAL

SLIDER
197
742
399
775
par_soil_depthNoise
par_soil_depthNoise
0
100
45.0
1
1
mm
HORIZONTAL

MONITOR
89
792
319
829
depth parameters
(word \"MIN = \" (precision soil_minDepth 2) \", MAX = \" (precision soil_maxDepth 2) \", depth noise = \" (precision soil_depthNoise 2))
17
1
9

PLOT
11
885
381
1035
Erosion curve of soil formation (soil depth)
flow accumulation
mm
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range (round soil_minDepth - 1) (round soil_maxDepth + 1)" "set-plot-y-range (round soil_minDepth - 1) (round soil_maxDepth + 1)"
PENS
"default" 1.0 0 -16777216 true "" "plot-table get-variable-per-flowAccumulation \"Depth\""

MONITOR
428
791
513
828
NIL
soil_min%sand
2
1
9

MONITOR
516
791
603
828
NIL
soil_max%sand
2
1
9

MONITOR
674
789
749
826
NIL
soil_min%silt
2
1
9

MONITOR
751
789
829
826
NIL
soil_max%silt
2
1
9

MONITOR
888
789
973
826
NIL
soil_min%clay
2
1
9

MONITOR
974
789
1059
826
NIL
soil_max%clay
2
1
9

MONITOR
311
835
447
872
NIL
soil_formativeErosionRate
2
1
9

MONITOR
768
652
893
689
NIL
mostCommonTextureType
0
1
9

@#$#@#$#@
## WHAT IS IT?

this version creates rivers over the terrain generated by v1 algorithms and derives the soil moisture of patches from rivers and meters below sea level. Rivers are formed by one or more streams which start at random patches and move from patch to patch towards the least elevation. There are two algorithms implementing the movement of streams: choosing only among neighbors (`river-algorithm = "least neighbor"`), favouring connections between basins, or neighbors *AND* the patch considered (`river-algorithm = "absolute downhill"`), producing 'stump' rivers more often. Every time a stream is formed, the elevation of the patches involved is depressed by a quantity (`par_waterDepression`) and then smoothed, together with that of neighboring patches. A passing stream will add 1 unit of `water` to a patch while patches below sea level have `water` units proportional to their depth. The amount of `water` of patches is converted to units of `moisture` and then moisture is distributed to other 'dry' patches using NetLogo's primitive `diffuse` (NOTE: not ideal because it does not account for the difference in elevation). 

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

line half 1
true
0
Line -7500403 true 150 0 150 300
Rectangle -7500403 true true 135 0 165 150

line half 2
true
0
Line -7500403 true 150 0 150 300
Rectangle -7500403 true true 120 0 180 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
