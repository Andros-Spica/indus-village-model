;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Integrated Land Unit model
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

extensions [ csv vid ]

breed [ transectLines transectLine ]
breed [ flowHolders flowHolder ]

globals
[
  ;;; constants
  patchArea
  maxDist

  yearLengthInDays

  ; randomSeed (GUI): seed of random number generator used for setting parameters and general stocahstic processes.

  ;************************************************************************
  ;*** LAND model

  ;***** non-fixed parameters (to select terrain to import):

  ; terrainRandomSeed (GUI): seed of the random number generator corresponding to the pre-processed terrain to import.

  ; elev_algorithm-style (GUI): style of algorithm to create land features "ranges" and "rifts".
  ; The algorithms styles available are: "NetLogo", using auxiliary agents and
  ; less parameters but with less control, and "C#", without agents and more
  ; parameters.

  ; flow_do-fill-sinks (GUI): whether to apply algorithm to fill all drainage "sinks" (land units
  ;                      not on the edge of the map where there is no flow towards a neighbour).
  ;                      Those sinks are likely to have been created by land forming algorithms.

  ;;; NOTE: terrainRandomSeed, elev_algorithm-style and flow_do-fill-sinks are used to select the file containing the terrain to import.

  ;***** fixed parameters (imported terrain):

  ;;; table inputs
  ;;;;; hydrologic Soil Groups table
  soil_textureTypes                           ; Types of soil according to % of sand, silt and clay (ternary diagram) established by USDA
  soil_textureTypes_display                   ; The content of soil_textureTypes ordered specifically for display (i.e. meaninful fixed colour pallete)
  soil_hydrologicSoilGroups                   ; USDA classification of soils according to water infiltration (A, B, C, and D) per each texture type

  ;;;;; run off curve number table
  soil_runOffCurveNumberTable                 ; table (list of lists) with run off curve numbers of Hydrologic Soil Group (columns) combination of cover type-treatment-hydrologic condition

  ;;;;; Field Capacity and Water Holding capacity table
  soil_fieldCapacity                   ; field capacity (fraction of soil volume) per texture type
  soil_saturation                      ; saturation (fraction of soil volume) per texture type (not currently used)
  soil_intakeRate                      ; intake rate (mm/hour) per texture type
  soil_minWaterHoldingCapacity         ; minimum and maximum water holding capacity (in/ft) per texture type (not currently used)
  soil_maxWaterHoldingCapacity

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

  ;;;; ecological community
  ecol_grassFrequencyInflection          ; flow accumulation required for having 50% of grass coverage (inflection point of the logistic curve)
  ecol_grassFrequencyRate                ; rate of increase in percentage of grass coverage, depending on flow_accumulation (rate or slope parameter of the logistic curve)
  ecol_brushFrequencyInflection          ; flow accumulation required for having 50% of brush coverage (inflection point of the logistic curve)
  ecol_brushFrequencyRate                ; rate of increase in percentage of brush coverage, depending on flow_accumulation (rate or slope parameter of the logistic curve)
  ecol_woodFrequencyInflection           ; flow accumulation required for having 50% of wood coverage (inflection point of the logistic curve)
  ecol_woodFrequencyRate                 ; rate of increase in percentage of wood coverage, depending on flow_accumulation (rate or slope parameter of the logistic curve)

  ;;;; derived measures
  landRatio                            ; the ratio of land units above seaLevel.
  elevationDistribution                ; the set or list containing the elevation of all land units
  minElevation                         ; statistics on the elevation of land units.
  sdElevation
  maxElevation

  landWithRiver                        ; count of land units with passing river
  maxFlowAccumulation

  mostCommonTextureType       ; the most common of texture type, see soil_textureTypes
  meanRunOffCurveNumber       ; mean runoff curve number of land units
  meanWaterHoldingCapacity    ; mean water holding capacity of land units (fraction of soil volume)
  meanDeepDrainageCoefficient ; mean deep drainage coefficient (1/day)

  mostCommonCoverType        ; the most common of cover type, see p_ecol_coverType

  ;****************************************************************************

  ;;;; soil water balance constants (?)
  soil_rootWaterUptakeCoefficient        ; (root) Water Uptake coefficient (mm^3.mm^-3) (MUF)

  ;;; parameters (modified copies of interface input) ===============================================================

  ;;; LAND ---------------------------------------------------------------------
  elev_seaLevelReferenceShift            ; the shift applied to re-centre elevations, pointing to the new 0 reference as the sea level (m)

  ;;; WEATHER ------------------------------------------------------------------
  ;;;; temperature (ºC)
  temperature_annualMaxAt2m
  temperature_annualMinAt2m
  temperature_meanDailyFluctuation
  temperature_dailyLowerDeviation
  temperature_dailyUpperDeviation

  ;;;; precipitation (mm)
  precipitation_yearlyMean
  precipitation_yearlySd
  precipitation_dailyCum_nSamples
  precipitation_dailyCum_maxSampleSize
  precipitation_dailyCum_plateauValue_yearlyMean
  precipitation_dailyCum_plateauValue_yearlySd
  precipitation_dailyCum_inflection1_yearlyMean
  precipitation_dailyCum_inflection1_yearlySd
  precipitation_dailyCum_rate1_yearlyMean
  precipitation_dailyCum_rate1_yearlySd
  precipitation_dailyCum_inflection2_yearlyMean
  precipitation_dailyCum_inflection2_yearlySd
  precipitation_dailyCum_rate2_yearlyMean
  precipitation_dailyCum_rate2_yearlySd

  ;;;; Solar radiation (kWh/m2)
  solar_annualMax
  solar_annualMin
  solar_meanDailyFluctuation

  ;;;; ETr
  ecol_minAlbedo
  ecol_maxAlbedo

  ;;; SOIL WATER BALANCE ------------------------------------------------------------------
  ecol_minRootZoneDepth
  ecol_maxRootZoneDepth

  ;;; RIVER -------------------------------------------------------------------------------
  riverWaterPerFlowAccumulation ; average river stage increment per flow accumulation at the river's starting land unit ( mm (height) / m^2 (area) ).
                                ; Because there are many factors subtracting river flow (assumin that the catchment area is large enough,
                                ; this parameter should be always very small; for the Indus Basin it would be between 1E-3 and 1E-5
                                ; (near their mounths, this value would be around 5.66E-5 for the Indus (av. discharge 800.6 m^3/s, basin area 2,615,500 ha)
                                ; and 3.061E-4 for the Chenab (av. discharge 6,600 m^3/s, basin area 116,500,000 ha).
                                ; see definition of river stage in:
                                ; https://www.usgs.gov/special-topic/water-science-school/science/water-qa-what-does-term-river-stage-mean?qt-science_center_objects=0#qt-science_center_objects
                                ; and:
                                ; https://www.fondriest.com/environmental-measurements/measurements/hydrological-measurements/streamflow-measurements/
                                ; and about river width/depth ratio:
                                ; https://cfpub.epa.gov/watertrain/moduleFrame.cfm?parent_object_id=1262#:~:text=The%20width%2Fdepth%20(W%2F,the%20channel%20to%20move%20sediment
                                ; and about stream types:
                                ; https://cfpub.epa.gov/watertrain/moduleFrame.cfm?parent_object_id=1199

  ;;; variables ===============================================================

  ;;;; time tracking
  currentYear
  currentDayOfYear

  ;;;; main (these follow a seasonal pattern and apply for all patches)

  temperature                          ; average temperature of current day (ºC) at 2 meters
  maxTemperature                       ; maximum temperature of current day (ºC)
  minTemperature                       ; minimum temperature of current day (ºC)

  solarRadiation                       ; solar radiation of current day (MJ m-2)

  precipitation                                 ; precipitation of current day ( mm (height) / m^2 (area) )
  precipitation_yearSeries
  precipitation_cumYearSeries

  ;;;; counters and final measures

]

patches-own
[
  ;========= LAND model ======================================================================
  ;;; keep all LAND model variables in place and in order or remember to update "import-terrain" procedure
  ;;; topography
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
                        ; A flow unit is the volume of runoff water flowing from one land unit
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
                              ; see "03-land-model/ternaryPlots/USDA-texturalSoilClassification.png"
  p_soil_hydrologicSoilGroup  ; USDA simplification of soil texture types into four categories

  p_soil_coverTreatmentAndHydrologicCondition  ; the type of combination of cover, treatment and hydrologic condition used to estimate runoff curve number (see "runOffCurveNumberTable.csv")
  p_soil_runOffCurveNumber                     ; runoff curve number (0=full retention to 100=full impermeability)

  ; Soil water capacities:
  ; ___ saturation
  ;  |
  ;  |  gravitational water
  ;  |  (rapid drainage)
  ;  |
  ; --- field capacity
  ;  |
  ;  |  water holding capacity or available soil moisture or capilary water
  ;  |  (slow drainage)
  ; --- permanent wilting point
  ;  |
  ;  |  unavailable soil moisture or hydroscopic water
  ;  |  (no drainage)
  ; --- oven dry

  p_soil_saturation                 ; saturation (fraction of soil volume)
  p_soil_fieldCapacity              ; field capacity (fraction of soil volume)
  p_soil_waterHoldingCapacity       ; water holding capacity (fraction of soil volume)
  p_soil_wiltingPoint               ; permanent wilting point (fraction of soil volume)

  p_soil_deepDrainageCoefficient    ; saturated hydraulic conductivity or fraction of soil water above field capacity drained per day (mm/day)

  ;;; initial ecological communities
  p_ecol_%grass                     ; percentage of grass vegetation (biomass) in ecological community
  p_ecol_%brush                     ; percentage of brush/shrub vegetation (biomass) in ecological community
  p_ecol_%wood                      ; percentage of wood vegetation (biomass) in ecological community

  p_ecol_coverType                  ; cover type summarising the composition of vegetation types in ecological community.
                                    ; Namely, they are: "desert", "grassland", "wood-grass", "shrubland", and "woodland"
                                    ; see "03-land-model/ternaryPlots/coverTypePerEcologicalCommunity.png"

  ;========= SOIL WATER BALANCE model ======================================================================

  ;;; surface water
  p_ecol_albedo                     ; canopy reflection
  p_netSolarRadiation               ; net solar radiation discount canopy reflection or albedo (MJ m-2)
  p_ETr                             ; reference evapotranspiration ( mm (height) / m^2 (area) )
  p_water                           ; surface water ( mm (height) / m^2 (area) )
  p_runoff                          ; Daily runoff ( mm (height) / m^2 (area) )

  ;;; soil water
  p_soil_waterContent               ; Water content in the soil profile for the rooting depth ( mm (height) / m^2 (area) )
  p_soil_waterContentRatio          ; Volumetric Soil Water content (fraction : mm.mm-1). calculated as WAT/z
  p_ecol_rootZoneDepth              ; root zone depth (mm).
  p_soil_ARID                       ; ARID index after Woli et al. 2012, ranging form 0 (no water shortage) to 1 (extreme water shortage)
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  ; --- import terrain -----------------------

  import-terrain

  ; --- loading/testing parameters -----------

  set-constants

  set-parameters

  ; --- core procedures ----------------------

  set currentDayOfYear 1

  setup-patches

  setup-river-water

  update-weather

  update-water

  ; ------------------------------------------

  set-output-stats

  setup-patch-coordinates-labels "bottom" "left"

  setup-transect

  refresh-to-display-mode

  reset-ticks

end

to set-constants

  ; "constants" are variables that will not be explored as parameters
  ; and may be used during a simulation.

  ; land units are 1 ha = 10,000 m^2
  set patchArea 10000

  set yearLengthInDays 365

  ; MUF : Water Uptake coefficient (mm^3 mm^-3)
  set soil_rootWaterUptakeCoefficient 0.096

end

to set-parameters

  ; set random seed
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

  ; check parameters values
  parameters-check

  ;;; setup parameters depending on the type of experiment
  if (type-of-experiment = "user-defined")
  [
    ;;; load parameters from user interface

    ;;; set sea level (no more just a display issue; it is relevant for ETr and limiting coastlines)
    set elev_seaLevelReferenceShift par_elev_seaLevelReferenceShift

    ;;; weather generation
    set temperature_annualMaxAt2m temperature_annual-max-at-2m
    set temperature_annualMinAt2m temperature_annual-min-at-2m
    set temperature_meanDailyFluctuation temperature_mean-daily-fluctuation
    set temperature_dailyLowerDeviation temperature_daily-lower-deviation
    set temperature_dailyUpperDeviation temperature_daily-upper-deviation

    set solar_annualMax solar_annual-max
    set solar_annualMin solar_annual-min
    set solar_meanDailyFluctuation solar_mean-daily-fluctuation

    set precipitation_yearlyMean precipitation_yearly-mean
    set precipitation_yearlySd precipitation_yearly-sd
    set precipitation_dailyCum_nSamples precipitation_daily-cum_n-samples
    set precipitation_dailyCum_maxSampleSize precipitation_daily-cum_max-sample-size
    set precipitation_dailyCum_plateauValue_yearlyMean precipitation_daily-cum_plateau-value_yearly-mean
    set precipitation_dailyCum_plateauValue_yearlySd precipitation_daily-cum_plateau-value_yearly-sd
    set precipitation_dailyCum_inflection1_yearlyMean precipitation_daily-cum_inflection1_yearly-mean
    set precipitation_dailyCum_inflection1_yearlySd precipitation_daily-cum_inflection1_yearly-sd
    set precipitation_dailyCum_rate1_yearlyMean precipitation_daily-cum_rate1_yearly-mean
    set precipitation_dailyCum_rate1_yearlySd precipitation_daily-cum_rate1_yearly-sd
    set precipitation_dailyCum_inflection2_yearlyMean precipitation_daily-cum_inflection2_yearly-mean
    set precipitation_dailyCum_inflection2_yearlySd precipitation_daily-cum_inflection2_yearly-sd
    set precipitation_dailyCum_rate2_yearlyMean precipitation_daily-cum_rate2_yearly-mean
    set precipitation_dailyCum_rate2_yearlySd precipitation_daily-cum_rate2_yearly-sd

    set ecol_minAlbedo par_ecol_minAlbedo
    set ecol_maxAlbedo par_ecol_maxAlbedo

    ;;; Soil Water Balance model
    set ecol_minRootZoneDepth par_ecol_minRootZoneDepth
    set ecol_maxRootZoneDepth par_ecol_maxRootZoneDepth

    ;;; River water (dependent on flow_riverAccumulationAtStart, which is set by Land model)
    set riverWaterPerFlowAccumulation par_riverWaterPerFlowAccumulation
  ]
  if (type-of-experiment = "random")
  [
    ;;; sometimes use values from user interface as a maximum for random uniform distributions

    ;;; set sea level (no more just a display issue; it is relevant for ETr and limiting coastlines)
    set elev_seaLevelReferenceShift -1 * random 1000

    ;;; weather generation
    set temperature_annualMaxAt2m 15 + random-float 35
    set temperature_annualMinAt2m -15 + random-float 30
    set temperature_meanDailyFluctuation random-float temperature_mean-daily-fluctuation
    set temperature_dailyLowerDeviation random-float temperature_daily-lower-deviation
    set temperature_dailyUpperDeviation random-float temperature_daily-upper-deviation

    set solar_annualMin random-normal 4 0.1
    set solar_annualMax solar_annualMin + random-float 2
    set solar_meanDailyFluctuation 0.01

    set precipitation_yearlyMean 200 + random-float 800
    set precipitation_yearlySd random-float 200
    set precipitation_dailyCum_nSamples 100 + random 200
    set precipitation_dailyCum_maxSampleSize 5 + random 20
    set precipitation_dailyCum_plateauValue_yearlyMean random-float 1
    set precipitation_dailyCum_plateauValue_yearlySd random-float 0.2
    set precipitation_dailyCum_inflection1_yearlyMean 10 + random 60
    set precipitation_dailyCum_inflection1_yearlySd 1 + random 10
    set precipitation_dailyCum_rate1_yearlyMean 0.01 + random-float 0.2
    set precipitation_dailyCum_rate1_yearlySd 0.001 + random-float 0.05
    set precipitation_dailyCum_inflection2_yearlyMean 150 + random 100
    set precipitation_dailyCum_inflection2_yearlySd 1 + random 10
    set precipitation_dailyCum_rate2_yearlyMean 0.01 + random-float 0.2
    set precipitation_dailyCum_rate2_yearlySd 0.001 + random-float 0.05

    ;;; ETr
    set ecol_minAlbedo 1E-6 + random-float 0.3
    set ecol_maxAlbedo ecol_minAlbedo + random-float 0.3

    ;;; Soil Water Balance model
    set ecol_minRootZoneDepth random-float 1000
    set ecol_maxRootZoneDepth ecol_minRootZoneDepth + random-float 1000

    ;;; River water (effect on p_water depends on flow_riverAccumulationAtStart, which is set by the Land model)
    set riverWaterPerFlowAccumulation 1E-4 + random-float 0.00099 ; range between 1E-3 and 1E-5
  ]

end

to parameters-check

  ;;; check if values were reset to 0 (NetLogo does that from time to time...!)
  ;;; and set default values (assuming they are not 0)

  ;;; sea level shift
  if (par_elev_seaLevelReferenceShift = 0)                       [ set par_elev_seaLevelReferenceShift                       -1000 ]


  ;;; the default values of weather parameters aim to broadly represent conditions in Haryana, NW India.

  if (temperature_annual-max-at-2m = 0)                          [ set temperature_annual-max-at-2m                             40 ]
  if (temperature_annual-min-at-2m = 0)                          [ set temperature_annual-min-at-2m                             15 ]
  if (temperature_mean-daily-fluctuation = 0)                    [ set temperature_mean-daily-fluctuation                        5 ]
  if (temperature_daily-lower-deviation = 0)                     [ set temperature_daily-lower-deviation                         5 ]
  if (temperature_daily-upper-deviation = 0)                     [ set temperature_daily-upper-deviation                         5 ]

  ;;; Global Horizontal Irradiation can vary from about 2 to 7 KWh/m-2 per day.
  ;;; See approx. values in https://globalsolaratlas.info/
  ;;; and https://www.researchgate.net/publication/271722280_Solmap_Project_In_India%27s_Solar_Resource_Assessment
  ;;; see general info in http://www.physicalgeography.net/fundamentals/6i.html
  if (solar_annual-max = 0)                                      [ set solar_annual-max                                          7 ]
  if (solar_annual-min = 0)                                      [ set solar_annual-min                                          3 ]
  if (solar_mean-daily-fluctuation = 0)                          [ set solar_mean-daily-fluctuation                              1 ]

  if (precipitation_yearly-mean = 0)                             [ set precipitation_yearly-mean                               400 ]
  if (precipitation_yearly-sd = 0)                               [ set precipitation_yearly-sd                                 130 ]
  if (precipitation_daily-cum_n-samples = 0)                      [ set precipitation_daily-cum_n-samples                      200 ]
  if (precipitation_daily-cum_max-sample-size = 0)               [ set precipitation_daily-cum_max-sample-size                  10 ]
  if (precipitation_daily-cum_plateau-value_yearly-mean = 0)     [ set precipitation_daily-cum_plateau-value_yearly-mean         0.1 ]
  if (precipitation_daily-cum_plateau-value_yearly-sd = 0)       [ set precipitation_daily-cum_plateau-value_yearly-sd           0.05 ]
  if (precipitation_daily-cum_inflection1_yearly-mean = 0)       [ set precipitation_daily-cum_inflection1_yearly-mean           40 ]
  if (precipitation_daily-cum_inflection1_yearly-sd = 0)         [ set precipitation_daily-cum_inflection1_yearly-sd             20 ]
  if (precipitation_daily-cum_rate1_yearly-mean = 0)             [ set precipitation_daily-cum_rate1_yearly-mean                 0.15 ]
  if (precipitation_daily-cum_rate1_yearly-sd = 0)               [ set precipitation_daily-cum_rate1_yearly-sd                   0.02 ]
  if (precipitation_daily-cum_inflection2_yearly-mean = 0)       [ set precipitation_daily-cum_inflection2_yearly-mean           200 ]
  if (precipitation_daily-cum_inflection2_yearly-sd = 0)         [ set precipitation_daily-cum_inflection2_yearly-sd             20 ]
  if (precipitation_daily-cum_rate2_yearly-mean = 0)             [ set precipitation_daily-cum_rate2_yearly-mean                 0.05 ]
  if (precipitation_daily-cum_rate2_yearly-sd = 0)               [ set precipitation_daily-cum_rate2_yearly-sd                   0.01 ]

  if (par_ecol_minAlbedo = 0)                                        [ set par_ecol_minAlbedo                                            0.1 ]
  if (par_ecol_maxAlbedo = 0)                                        [ set par_ecol_maxAlbedo                                            0.5 ]

  if (par_ecol_minRootZoneDepth = 0)                             [ set par_ecol_minRootZoneDepth                               200 ]
  if (par_ecol_maxRootZoneDepth = 0)                             [ set par_ecol_maxRootZoneDepth                              2000 ]

  if (par_riverWaterPerFlowAccumulation = 0)                     [ set par_riverWaterPerFlowAccumulation                        1E-4 ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set par_elev_seaLevelReferenceShift                       -1000

  set temperature_annual-max-at-2m                             40
  set temperature_annual-min-at-2m                             15
  set temperature_mean-daily-fluctuation                        5
  set temperature_daily-lower-deviation                         5
  set temperature_daily-upper-deviation                         5

  set solar_annual-max                                          7
  set solar_annual-min                                          3
  set solar_mean-daily-fluctuation                              1

  set precipitation_yearly-mean                               400
  set precipitation_yearly-sd                                 130
  set precipitation_daily-cum_n-samples                       200
  set precipitation_daily-cum_max-sample-size                  10
  set precipitation_daily-cum_plateau-value_yearly-mean         0.1
  set precipitation_daily-cum_plateau-value_yearly-sd           0.05
  set precipitation_daily-cum_inflection1_yearly-mean           40
  set precipitation_daily-cum_inflection1_yearly-sd             20
  set precipitation_daily-cum_rate1_yearly-mean                 0.15
  set precipitation_daily-cum_rate1_yearly-sd                   0.02
  set precipitation_daily-cum_inflection2_yearly-mean           200
  set precipitation_daily-cum_inflection2_yearly-sd             20
  set precipitation_daily-cum_rate2_yearly-mean                 0.05
  set precipitation_daily-cum_rate2_yearly-sd                   0.01

  set par_ecol_minAlbedo                                            0.1
  set par_ecol_maxAlbedo                                            0.5

  set par_ecol_minRootZoneDepth                               200
  set par_ecol_maxRootZoneDepth                              2000

  set par_riverWaterPerFlowAccumulation                        1E-4

end

to setup-patches

  ask patches
  [
    ;;; recentre elevation so negative values are only below sea level
    set elevation get-recentred-elevation

    set p_ecol_albedo ecol_minAlbedo + random-float ecol_maxAlbedo

    ; z : root zone depth (mm)
    let efectiveMaxRootZoneDepth min (list p_soil_depth par_ecol_maxRootZoneDepth)
    set p_ecol_rootZoneDepth ecol_minRootZoneDepth + random-float efectiveMaxRootZoneDepth

    ; WAT0 : Initial Water content (mm)
    set p_soil_waterContent  p_ecol_rootZoneDepth * p_soil_fieldCapacity
  ]

end

to setup-river-water

  ask patches with [flow_accumulation > flow_riverAccumulationAtStart] ; patches containing the river
  [
    set p_water flow_accumulation * riverWaterPerFlowAccumulation
  ]

end

to-report get-recentred-elevation

  ;;; get re-centre value of elevation in relation to new seaLevel
  report elevation - elev_seaLevelReferenceShift

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ; --- core procedures -------------------------

  update-weather

  update-water

  ; --------------------------------------------

  advance-time

  refresh-to-display-mode

  tick

  ; --- stop conditions -------------------------

  if (ticks = end-simulation-in-tick) [stop]

end

;;; GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to advance-time

  set currentDayOfYear currentDayOfYear + 1
  if (currentDayOfYear > yearLengthInDays)
  [
    set currentYear currentYear + 1
    set currentDayOfYear 1
  ]

end

to update-weather

  ;;; values are assigned using simple parametric models
  ;;; alternatively, a specific time series could be used

  update-temperature currentDayOfYear

  update-precipitation currentDayOfYear

  set solarRadiation get-solar-radiation currentDayOfYear

  ask patches
  [
    set p_netSolarRadiation (1 - p_ecol_albedo) * solarRadiation
    set p_ETr get-ETr
  ]

end

to update-temperature [ dayOfYear ]

  set temperature get-temperature dayOfYear

  set minTemperature temperature - temperature_dailyLowerDeviation

  set maxTemperature temperature + temperature_dailyUpperDeviation

end

to-report get-temperature [ dayOfYear ]

  ;;; get temperature base level for the current day (ºC at lowest elevation)

  report (get-annual-sinusoid-with-fluctuation temperature_annualMinAt2m temperature_annualMaxAt2m temperature_meanDailyFluctuation dayOfYear)

end

to update-precipitation [ dayOfYear ]

  if (dayOfYear = 1) [ set-precipitation-of-year ]

  set precipitation item (dayOfYear - 1) precipitation_yearSeries

end

to set-precipitation-of-year

  ;;;===============================================================================
  ;;; Simulate *cumulative proportion of year precipitation*.
  set-daily-cumulative-precipitation

  ;;;===============================================================================
  ;;; Derivate *daily proportion of year precipitation* from simulated *cumulative proportion of year precipitation*.
  ;;; These are the difference between day i and day i - 1

  let precipitation_propYearSeries precipitation_cumYearSeries

  foreach n-values (length precipitation_cumYearSeries) [j -> j]
  [
    dayOfYearIndex ->
    if (dayOfYearIndex > 0) ; do not iterate for the first (0) element
    [
      set precipitation_propYearSeries replace-item dayOfYearIndex precipitation_propYearSeries (item dayOfYearIndex precipitation_cumYearSeries - item (dayOfYearIndex - 1) precipitation_cumYearSeries)
    ]
  ]

  ;;;===============================================================================
  ;;; Calculate *daily precipitation* values by multipling *daily proportions of year precipitation* by the *year total precipitation*

  ;;; get randomised total precipitation of current year
  let totalYearPrecipitation random-normal precipitation_yearlyMean precipitation_yearlySd

  ;;; multiply every precipitation_propYearSeries value by totalYearPrecipitation, excluding the first element (which is the extra theoretical day used for calculate difference
  set precipitation_yearSeries but-first map [ i -> i * totalYearPrecipitation ] precipitation_propYearSeries

end

to set-daily-cumulative-precipitation

  ;;;===============================================================================
  ;;; get double logistic curve as a proxy of the year series of daily cumulative precipitation

  ;;; get randomised values for parameters of the double logistic curve
  let plateauValue clamp01 (random-normal precipitation_dailyCum_plateauValue_yearlyMean precipitation_dailyCum_plateauValue_yearlySd)
  let inflection1 clampMinMax (random-normal precipitation_dailyCum_inflection1_yearlyMean precipitation_dailyCum_inflection1_yearlySd) 1 yearLengthInDays
  let rate1 clampMin0 (random-normal precipitation_dailyCum_rate1_yearlyMean precipitation_dailyCum_rate1_yearlySd)
  let inflection2 clampMinMax (random-normal precipitation_dailyCum_inflection2_yearlyMean precipitation_dailyCum_inflection2_yearlySd) 1 yearLengthInDays
  let rate2 clampMin0 (random-normal precipitation_dailyCum_rate2_yearlyMean precipitation_dailyCum_rate2_yearlySd)
  ;print (word "plateauValue = " plateauValue ", inflection1 = " inflection1 ", rate1 = " rate1 ", inflection2 = " inflection2 ", rate2 = " rate2)

  ;;; get curve (we want one more point besides yearLengthInDays to account for the initial difference or daily precipitation
  set precipitation_cumYearSeries get-double-logistic-curve (yearLengthInDays + 1) plateauValue inflection1 rate1 inflection2 rate2

  ;;;===============================================================================
  ;;; modify the curve breaking the continuous pattern by randomly aggregating values

  foreach n-values precipitation_dailyCum_nSamples [j -> j + 1] ; do not iterate for the first (0) element
  [
    sampleIndex ->
    ; get a decreasing sample size proportionally to sample index
    let thisSampleSize ceiling (precipitation_dailyCum_maxSampleSize * sampleIndex / precipitation_dailyCum_nSamples)
    ; get random day of year to have rain (we exclude 0, which is the extra day or the last day of previous year)
    let rainDOY 1 + random yearLengthInDays
    ; set sample limits
    let earliestNeighbour max (list 1 (rainDOY - thisSampleSize))
    let latestNeighbour min (list yearLengthInDays (rainDOY + thisSampleSize))
    ; get mean of neighbourhood
    let meanNeighbourhood mean (sublist precipitation_cumYearSeries earliestNeighbour latestNeighbour)
    ;print (word "thisSampleSize = " thisSampleSize ", rainDOY = " rainDOY ", earliestNeighbour = " earliestNeighbour ", latestNeighbour = " latestNeighbour)
    ;print meanNeighbourhood
    ; assign mean to all days in neighbourhood
    foreach n-values (latestNeighbour - earliestNeighbour) [k -> earliestNeighbour + k]
    [
      dayOfYearIndex ->
      set precipitation_cumYearSeries replace-item dayOfYearIndex precipitation_cumYearSeries meanNeighbourhood
    ]
  ]

end

to-report get-solar-radiation [ dayOfYear ]

  ;;; get solar radiation for the current day (MJ/m2)
  ;;; return value converted from kWh/m2 to MJ/m2 (1 : 3.6)

  report (get-annual-sinusoid-with-fluctuation solar_annualMin solar_annualMax solar_meanDailyFluctuation dayOfYear) * 3.6
  ;;; NOTE: it might be possible to decrease solar radiation depending on the current day precipitation. Further info on precipitation effect on solar radiation is needed.

end

to-report get-ETr

  ;;; useful references:
  ;;; Suleiman A A and Hoogenboom G 2007
  ;;; Comparison of Priestley-Taylor and FAO-56 Penman-Monteith for Daily Reference Evapotranspiration Estimation in Georgia
  ;;; J. Irrig. Drain. Eng. 133 175–82 Online: http://ascelibrary.org/doi/10.1061/%28ASCE%290733-9437%282007%29133%3A2%28175%29
  ;;; also: Jia et al. 2013 - doi:10.4172/2168-9768.1000112
  ;;; Allen, R. G., Pereira, L. A., Raes, D., and Smith, M. 1998.
  ;;; “Crop evapotranspiration.”FAO irrigation and  drainage paper 56, FAO, Rome.
  ;;; also: http://www.fao.org/3/X0490E/x0490e07.htm
  ;;; constants found in: http://www.fao.org/3/X0490E/x0490e07.htm
  ;;; see also r package: Evapotranspiration (consult source code)

  let windSpeed 2 ; as recommended by: http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data

  ;;; estimation of saturated vapour pressure (e_s) and actual vapour pressure (e_a)
  let estimationSaturatedVapourPressure  (get-vapour-pressure maxTemperature  + get-vapour-pressure minTemperature ) / 2
  let estimationActualVapourPressure  get-vapour-pressure minTemperature
  ; ... in absence of dew point temperature, as recommended by
  ; http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data
  ; however, possibly min temp > dew temp under arid conditions

  ;;; slope of  the  vapor  pressure-temperature  curve (kPa ºC−1)
  let slopeVapourPressurePerTemperature 4098 * (get-vapour-pressure temperature) / (temperature + 237.3) ^ 2

  ;;; latent heat of vaporisation = 2.45 MJ.kg^-1
  let latentHeatVaporisation 2.45

  ;;; specific heat at constant pressure, 1.013 10-3 [MJ kg-1 °C-1]
  let specificHeatAtConstantPressure 1.013 * 10 ^ -3
  ;;; ratio molecular weight of water vapour/dry air
  let ratioMolecularWeightWaterVapourOverDryAir 0.622
  ;;; atmospheric pressure (kPa)
  let atmosphericPressure 101.3 * ((293 - 0.0065 * elevation) / 293) ^ 5.26
  ;;; psychometric constant (kPa ºC−1)
  let psychometricConstant specificHeatAtConstantPressure * atmosphericPressure / (ratioMolecularWeightWaterVapourOverDryAir * latentHeatVaporisation)

  ;;; Penman-Monteith equation from: fao.org/3/X0490E/x0490e0 ; and from: weap21.org/WebHelp/Mabia_Alg ETRef.htm

  ; Ad-hoc constants specific for vegetation cover
  ; 900 and 0.34 for the grass reference; 1600 and 0.38 for the alfalfa reference
  let C_n 900
  let C_d 0.34

  let ETr_temp (0.408 * slopeVapourPressurePerTemperature * p_netSolarRadiation + psychometricConstant * (C_n / (temperature + 273)) * windSpeed * (estimationSaturatedVapourPressure  - estimationActualVapourPressure )) / (slopeVapourPressurePerTemperature + psychometricConstant * (1 + C_d * windSpeed))

  report ETr_temp

end

to-report get-vapour-pressure [ temp ]

  report (0.6108 * exp(17.27 * temp / (temp + 237.3)))

end

;=======================================================================================================
;;; START of water flow and soil water algorithms (combined)
;;; soil water balance model is based on:
;;; 'Working with dynamic crop models: Methods, tools, and examples for agriculture and enviromnent'
;;; Daniel Wallach, David Makowski, James W. Jones, François Brun (2006, 2014, 2019)
;;; Model description in p. 24-28, R code example in p. 138-144.
;;; see also https://github.com/cran/ZeBook/blob/master/R/watbal.model.r
;;; Some additional info about run off at: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
;;; and at: https://en.wikipedia.org/wiki/Runoff_curve_number
;;;
;;; flow algorithms are based on:
;;; Jenson, S. K., & Domingue, J. O. (1988).
;;; Extracting topographic structure from digital elevation data for geographic information system analysis.
;;; Photogrammetric engineering and remote sensing, 54(11), 1593-1600.
;;; ===BUT used elsewhere, such as in the algorithms based on:
;;; Huang, P., Lee, K.T. A simple depression-filling method for raster and irregular elevation datasets.
;;; J Earth Syst Sci 124, 1653–1665 (2015). https://doi.org/10.1007/s12040-015-0641-2
;;; See also: "02-Soil-Water-Balance-model" and "03-land-model" directory within "indus-village-model".
;=======================================================================================================

to update-water

  add-river-water

  add-precipitation-water

  solve-runoff-exchange

  drain-soil-water

end

to add-river-water

  ask patches with [flow_accumulation > flow_riverAccumulationAtStart] ; patches containing the river or only startin point: = flow_riverAccumulationAtStart + 1]
  [
    set p_water flow_riverAccumulationAtStart * riverWaterPerFlowAccumulation ; no accumulation in river between one day to the other
  ]

end

to add-precipitation-water

  ask patches
  [
    ; 1m^2 (area) -> 1mm (precipitation)
    set p_water p_water + precipitation
  ]

end

to solve-runoff-exchange

  ; The runoff exchange algorithm uses the same logic that was used to calculate flow_accumulation in the Land model.
  ;
  ; From Jenson, S. K., & Domingue, J. O. (1988), p. 1594
  ; "FLOW ACCUMULATION DATA SET
  ; The third procedure of the conditioning phase makes use of the flow direction data set to create the flow accumulation data set,
  ; where each cell is assigned a value equal to the number of cells that flow to it (O’Callaghan and Mark, 1984).
  ; Cells having a flow accumulation value of zero (to which no other cells flow) generally correspond to the pattern of ridges.
  ; Because all cells in a depressionless DEM have a path to the data set edge, the pattern formed by highlighting cells
  ; with values higher than some threshold delineates a fully connected drainage network. As the threshold value is increased,
  ; the density of the drainage network decreases. The flow accumulation data set that was calculated for the numeric example
  ; is shown in Table 2d, and the visual example is shown in Plate 1c."

  ; identify patches that receive flow and those that do not (this makes the next step much easier)
  ask patches
  [
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
        set flow_accumulationState "pending"
        ;set pcolor yellow
      ]
    ]
  ]

  ; main process: progressively compare water between start and next pending patch in flow, record effective runoff of start patch, and update water in pending patch.
  let maxIterations 100000 ; just as a safety measure, to avoid infinite loop
  while [count patches with [flow_accumulationState = "pending" and not flow-direction-is-loop] > 0 and maxIterations > 0 and count patches with [flow_accumulationState = "start"] > 0 ]
  [
    ask one-of patches with [flow_accumulationState = "start"]
    [
      let thisPatch self
      let downstreamPatch get-patch-in-flow-direction flow_direction

      ; calculate runoff and infiltration in soil
      set-runoff
      infiltrate-soil-water

      set flow_accumulationState "done"
      ;set pcolor orange

      if (downstreamPatch != nobody)
      [
        try-send-runoff-to downstreamPatch

        ask downstreamPatch
        [
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

to set-runoff

  ; Maximum abstraction (mm; for run off)
  let maximumAbstraction 25400 / p_soil_runOffCurveNumber - 254

  ; Initial Abstraction (mm; for run off)
  let initialAbstraction 0.2 * maximumAbstraction

  ; Change in Water Before Drainage (p_water - p_runoff)
  set p_runoff 0
  if (p_water > initialAbstraction)
  [ set p_runoff ((p_water - 0.2 * maximumAbstraction) ^ 2) / (p_water + 0.8 * maximumAbstraction) ]

  ; add incoming river water at river start
  if (flow_accumulation = flow_riverAccumulationAtStart + 1)
  [
    set p_water p_water + flow_riverAccumulationAtStart * riverWaterPerFlowAccumulation
  ]

end

to infiltrate-soil-water

  ; WATst : Maximum Water content at saturation (mm)
  let WATst p_soil_saturation * p_ecol_rootZoneDepth

  let potentialSoilWaterChange p_water - p_runoff
  let soilWaterChange 0
  ;print self print (word "p_runoff=" p_runoff)
  ifelse ( p_soil_waterContent + (p_water - p_runoff) > WATst )
  [
    ; soil is or becomes saturated and water accumulates on surface adding to runoff
    set soilWaterChange WATst - p_soil_waterContent
    set p_runoff p_runoff + (potentialSoilWaterChange - soilWaterChange)

    ; saturation sets the runnoffCurveNumber to maximum
    set p_soil_runOffCurveNumber 100
  ]
  [
    ; soil absorbes all water not running off
    set soilWaterChange potentialSoilWaterChange

    ; desaturation recovers the original runnoffCurveNumber, if it has been set to maximum
    if (p_soil_runOffCurveNumber = 100)
    [
      set p_soil_runOffCurveNumber get-runOffCurveNumber p_soil_coverTreatmentAndHydrologicCondition p_soil_hydrologicSoilGroup
    ]
  ]
  ;print self print (word "p_runoff=" p_runoff)
  ;print self print (word p_water " = " (soilWaterChange + p_runoff) )
  ;;; add amount to soil water content
  set p_soil_waterContent p_soil_waterContent + soilWaterChange
  set p_water p_water - (soilWaterChange + p_runoff)

end

to try-send-runoff-to [ downstreamPatch ]

  ;;; from the perspective of a patch,
  ;;; try to send the current runoff to another patch,
  ;;; considering the elevation change produced by water accumulation

  ;;; algorithm based on:
  ;;; Yang L E, Scheffran J, Süsser D, Dawson R and Chen Y D 2018 Assessment of Flood Losses with Household Responses:
  ;;; Agent-Based Simulation in an Urban Catchment Area Environ. Model. Assess. 23 369–88
  ;;; http://link.springer.com/10.1007/s10666-018-9597-3

  let thisPatch_h1 elevation
  let thisPatch_h2 p_runoff / 1000 ; in m

  let downstreamPatch_h1 [elevation] of downstreamPatch
  let downstreamPatch_h2 [p_water / 1000] of downstreamPatch ; in m

  ;;; schematics:

  ;==== case 1: h1 of A is higher than (h2 of B) + (h2 - h1) of A
  ;
  ;----- h2 ----|
  ;----- h1 ----|                      |----- h1 ----|
  ;             |                --->  |             |----- h2 ----|
  ;             |----- h2 ----|        |             |             |
  ;             |----- h1 ----|        |             |----- h1 ----|
  ;______A______|______B______|        |______A______|______B______|

  ;==== case 2: h1 of A is lower than (h2 of B) + (h2 - h1) of A
  ;
  ;----- h2 ----|                      _ _ _ _h2_ _ _ _ _ _ h2_ _ _
  ;             |----- h2 ----|        |             |             |
  ;----- h1 ----|             |  --->  |----- h1 ----|             |
  ;             |             |        |             |             |
  ;             |----- h1 ----|        |             |----- h1 ----|
  ;______A______|______B______|        |______A______|______B______|

  ;==== case 3: next downstream patch (B) is already overflowed (h2 of B > h2 of A)
  ;
  ;             |----- h2 ----|
  ;             |             |        |----- h2 ----|----- h2 ----|
  ;----- h2 ----|             |  --->  |             |             |
  ;----- h1 ----|             |        |----- h1 ----|             |
  ;             |----- h1 ----|        |             |----- h1 ----|
  ;______A______|______B______|        |______A______|______B______|

  ;print self
  ;;; case 1:
  ;;; if the elevation with water of the downstream patch is lower, even after transferring all runoff
  ifelse (thisPatch_h1 > downstreamPatch_h1 + downstreamPatch_h2 + thisPatch_h2)
  [;print "case 1"
    ;;; all runoff is transfered downstream
    ask downstreamPatch
    [
      ;;; update water downstream (receives all runoff)
      ;let p_water-temp p_water
      set p_water p_water + thisPatch_h2 * 1000 ; in mm
      ;print (word p_water-temp " + " thisPatch_h2 " * 1000 = " p_water)
    ]
    ;;; update water after runoff substraction
    ;set p_water p_water - thisPatch_h2 * 1000 ; in mm
  ]
  [;print "case 2 & 3" print (word self ", h1=" thisPatch_h1 ", h2=" thisPatch_h2 "; " downstreamPatch ", h1=" downstreamPatch_h1 ", h2=" downstreamPatch_h2 )
    ;;; case 2 & case 3
    ;;; runoff is equaly distributed
    ask downstreamPatch
    [
      ;;; update water downstream
      set p_water 0.5 * (downstreamPatch_h1 + downstreamPatch_h2 + thisPatch_h1 + thisPatch_h2) - downstreamPatch_h1
      ;print (word "0.5 * (" downstreamPatch_h1 " + " downstreamPatch_h2 " + " thisPatch_h1 " + " thisPatch_h2 ") - " downstreamPatch_h1 " = " p_water)
      set p_water p_water * 1000 ; in mm
    ]
    ;;; update (effective) runoff (this should be the last instance this patch runoff can be modified during a time step)
    set p_runoff 0.5 * (downstreamPatch_h1 + downstreamPatch_h2 + thisPatch_h1 + thisPatch_h2) - downstreamPatch_h1
    set p_runoff p_runoff * 1000 ; in mm

    ;;; update water after runoff substraction
    set p_water 0.5 * (thisPatch_h1 + thisPatch_h2 + downstreamPatch_h1 + downstreamPatch_h2) - thisPatch_h1
    ;print (word "0.5 * (" thisPatch_h1 " + " thisPatch_h2 " + " downstreamPatch_h1 " + " downstreamPatch_h2 ") - " thisPatch_h1 " = " p_water)
    set p_water p_water * 1000 ; in mm
    ;print (word "total height (m): thisPatch = " (elevation + p_water / 1000) " ; downstreamPatch = " [(elevation + p_water / 1000)] of downstreamPatch)
  ]
;print "================================================"
end

to drain-soil-water

  ask patches
  [
    ; WATfc : Maximum Water content at field capacity (mm)
    let WATfc p_soil_fieldCapacity * p_ecol_rootZoneDepth

    ; WATwp : Water content at wilting Point (mm)
    let WATwp p_soil_wiltingPoint * p_ecol_rootZoneDepth

    ; Calculating the amount of deep drainage
    let deepDrainage 0
    if (p_soil_waterContent > WATfc)
    [ set deepDrainage p_soil_deepDrainageCoefficient * (p_soil_waterContent - WATfc) ]
;if (deepDrainage > p_soil_waterContent) [ print self print (word deepDrainage " = (" p_soil_deepDrainageCoefficient " / " p_ecol_rootZoneDepth ") * (" p_soil_waterContent " - " WATfc ")" ) ]

    ; Compute maximum water uptake by plant roots on a day, RWUM
    let maxWaterUptakePlantRoot soil_rootWaterUptakeCoefficient * (p_soil_waterContent - WATwp)

    ; Calculate the amount of water lost through transpiration (TR)
    let transpiration min (list maxWaterUptakePlantRoot p_ETr)

    ; Calculate rate of change of state variable p_soil_waterContent
    set p_soil_waterContent p_soil_waterContent - deepDrainage - transpiration
    if (p_soil_waterContent < 0) [ print self print p_soil_waterContent ]
    set p_soil_waterContentRatio p_soil_waterContent / p_ecol_rootZoneDepth

    ; Calculate ARID coefficient or drought index
    set p_soil_ARID 0
    if (transpiration < p_ETr)
    [ set p_soil_ARID 1 - transpiration / p_ETr ]
  ]

end

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

to-report get-runOffCurveNumber [ coverTreatmentAndHydrologicCondition hydrologicSoilGroup ]

  report (
    item
    (position coverTreatmentAndHydrologicCondition (item 0 soil_runOffCurveNumberTable))            ; selecting row
    (item (1 + position hydrologicSoilGroup (list "A" "B" "C" "D")) soil_runOffCurveNumberTable)    ; selecting column (skip column with coverTreatmentAndHydrologicCondition)
    )

end

;=======================================================================================================
;;; END of water flow and soil water algorithms (combined)
;;; flow algorithms are based on:
;;; Jenson, S. K., & Domingue, J. O. (1988).
;;; Extracting topographic structure from digital elevation data for geographic information system analysis.
;;; Photogrammetric engineering and remote sensing, 54(11), 1593-1600.
;;; ===BUT used elsewhere, such as in the algorithms based on:
;;; Huang, P., Lee, K.T. A simple depression-filling method for raster and irregular elevation datasets.
;;; J Earth Syst Sci 124, 1653–1665 (2015). https://doi.org/10.1007/s12040-015-0641-2
;;; See also: "02-Soil-Water-Balance-model" and "03-land-model" directory within "indus-village-model".
;=======================================================================================================



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT STATS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-output-stats

  set elevationDistribution [elevation] of patches

  set minElevation min [elevation] of patches

  set maxElevation max [elevation] of patches

  set sdElevation standard-deviation [elevation] of patches

  set landRatio count patches with [elevation >= 0] / count patches

  set landWithRiver count patches with [flow_accumulation >= flow_riverAccumulationAtStart]

  set mostCommonTextureType modes [p_soil_textureType] of patches
  set meanRunOffCurveNumber mean [p_soil_runOffCurveNumber] of patches
  set meanWaterHoldingCapacity mean [p_soil_waterHoldingCapacity] of patches
  set meanDeepDrainageCoefficient mean [p_soil_deepDrainageCoefficient] of patches

  set mostCommonCoverType modes [p_ecol_coverType] of patches

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to refresh-view-after-seaLevel-change

  ;;; this procedure will recentre patch elevations according to elev_seaLevelReferenceShift
  ;;; and update landRatio and display

  ask patches
  [
    ;;; recentre elevation so negative values are only below sea level
    set elevation get-recentred-elevation
  ]

  set landRatio count patches with [elevation >= 0] / count patches

  refresh-to-display-mode

end

to refresh-to-display-mode

  ;;; several soil properties must be rescaled to enhance visualisation
  ;;; (the parametric max and min values of some of these are never realised for various reasons)

  set-current-plot "Legend"

  clear-plot

  if (display-mode = "elevation and surface water depth (m)")
  [
    let minWater min [p_water] of patches with [p_water > 0]
    let maxWater max [p_water] of patches with [p_water > 0]

    let rangeWater maxWater - minWater
    if (rangeWater = 0) [ set rangeWater 1 ]


    ask patches
    [
      ; paint elevation
      set pcolor get-elevation-color elevation

      ; paint water depth
      if (p_water > 0)
      [ set pcolor 98 - 4 * (p_water - minWater) / rangeWater ]
    ]

    set-legend-elevation-and-water (maxWater / 1000) (minWater / 1000) 94 98 6 true ; in metres
  ]
  if (display-mode = "elevation (m)")
  [
    ask patches
    [
      set pcolor get-elevation-color elevation
    ]
    set-legend-elevation 10
  ]
  if (display-mode = "surface water depth (mm)")
  [
    let minWater min [p_water] of patches with [p_water > 0]
    let maxWater max [p_water] of patches with [p_water > 0]

    let rangeWater maxWater - minWater
    if (rangeWater = 0) [ set rangeWater 1 ]

    ask patches
    [
      ifelse (p_water > 0)
      [ set pcolor 94 - 4 * (p_water - minWater) / rangeWater ]
      [ set pcolor 99 ]
    ]
    set-legend-continuous-range maxWater minWater 98 94 6 false
  ]
  if (display-mode = "soil formative erosion")
  [
    ask patches [ set pcolor 8 - 6 * p_soil_formativeErosion ]
    set-legend-continuous-range 1 0 8 2 6 false
  ]
  if (display-mode = "soil depth (mm)")
  [
    let minDepth min [p_soil_depth] of patches
    let maxDepth max [p_soil_depth] of patches

    let rangeDepth maxDepth - minDepth
    if (rangeDepth = 0) [ set rangeDepth 1 ]

    ask patches [ set pcolor 38 - 6 * (p_soil_depth - mindepth) / rangeDepth ]
    set-legend-continuous-range 100 0 38 32 6 false
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
    set-legend-texture (list min%sand max%sand) (list min%silt max%silt) (list min%clay max%clay)
  ]
  if (display-mode = "soil texture types")
  [
    ask patches
    [
      set pcolor get-textureType-color p_soil_textureType
    ]
    set-legend-soil-textureType
  ]
  if (display-mode = "soil run off curve number")
  [
    ask patches [ set pcolor 18 - 6 * p_soil_runOffCurveNumber / 100 ] ;;; runoff curve number is limited between 0-100
    set-legend-continuous-range 100 0 18 12 6 false
  ]
  if (display-mode = "soil water wilting point")
  [
    let minWiltingPoint min [p_soil_wiltingPoint] of patches
    let maxWiltingPoint max [p_soil_wiltingPoint] of patches

    let rangeWiltingPoint maxWiltingPoint - minWiltingPoint
    if (rangeWiltingPoint = 0) [ set rangeWiltingPoint 1 ]

    ask patches
    [
      set pcolor 98 - 6 * (p_soil_wiltingPoint - minWiltingPoint) / rangeWiltingPoint
    ]
    set-legend-continuous-range maxWiltingPoint minWiltingPoint 98 92 6 false
  ]
  if (display-mode = "soil water holding capacity")
  [
    let minWaterHoldingCapacity min [p_soil_waterHoldingCapacity] of patches
    let maxWaterHoldingCapacity max [p_soil_waterHoldingCapacity] of patches

    let rangeWaterHoldingCapacity maxWaterHoldingCapacity - minWaterHoldingCapacity
    if (rangeWaterHoldingCapacity = 0) [ set rangeWaterHoldingCapacity 1 ]

    ask patches
    [
      set pcolor 98 - 6 * (p_soil_waterHoldingCapacity - minWaterHoldingCapacity) / rangeWaterHoldingCapacity
    ]
    set-legend-continuous-range maxWaterHoldingCapacity minWaterHoldingCapacity 98 92 6 false
  ]
  if (display-mode = "soil water field capacity")
  [
    let minFieldCapacity min [p_soil_fieldCapacity] of patches
    let maxFieldCapacity max [p_soil_fieldCapacity] of patches

    let rangeFieldCapacity maxFieldCapacity - minFieldCapacity
    if (rangeFieldCapacity = 0) [ set rangeFieldCapacity 1 ]

    ask patches
    [
      set pcolor 98 - 6 * (p_soil_fieldCapacity - minFieldCapacity) / rangeFieldCapacity
    ]
    set-legend-continuous-range maxFieldCapacity minFieldCapacity 98 92 6 false
  ]
  if (display-mode = "soil water saturation")
  [
    let minSaturation min [p_soil_saturation] of patches
    let maxSaturation max [p_soil_saturation] of patches

    let rangeSaturation maxSaturation - minSaturation
    if (rangeSaturation = 0) [ set rangeSaturation 1 ]

    ask patches
    [
      set pcolor 98 - 6 * (p_soil_saturation - minSaturation) / rangeSaturation
    ]
    set-legend-continuous-range maxSaturation minSaturation 98 92 6 false
  ]
  if (display-mode = "soil deep drainage coefficient")
  [
    let minDeepDrainageCoefficient min [p_soil_deepDrainageCoefficient] of patches
    let maxDeepDrainageCoefficient max [p_soil_deepDrainageCoefficient] of patches

    let rangeDeepDrainageCoefficient maxDeepDrainageCoefficient - minDeepDrainageCoefficient
    if (rangeDeepDrainageCoefficient = 0) [ set rangeDeepDrainageCoefficient 1 ]

    ask patches
    [
      set pcolor 102 + 6 * (p_soil_deepDrainageCoefficient - minDeepDrainageCoefficient) / rangeDeepDrainageCoefficient
      ;;; deep drainage coefficient is %, but depends on time and can vary beyond 100%
    ]
    set-legend-continuous-range maxDeepDrainageCoefficient minDeepDrainageCoefficient 108 102 6 true
  ]
  if (display-mode = "ecological community composition")
  [
    ask patches
    [
      ;;; red: sand, green: silt, blue: clay
      set pcolor get-ecologicalCommunityComposition-color p_ecol_%grass p_ecol_%brush p_ecol_%wood
    ]
    set-legend-ecologicalCommunityComposition
  ]
  if (display-mode = "cover type")
  [
    ask patches
    [
      set pcolor get-coverType-color p_ecol_coverType
    ]
    set-legend-coverType
  ]
  if (display-mode = "albedo")
  [
    let minAlbedo min [p_ecol_albedo] of patches
    let maxAlbedo max [p_ecol_albedo] of patches

    let rangeAlbedo maxAlbedo - minAlbedo
    if (rangeAlbedo = 0) [ set rangeAlbedo 1 ]

    ask patches
    [
      set pcolor 2 + 6 * (p_ecol_albedo - minAlbedo) / rangeAlbedo
    ]
    set-legend-continuous-range maxAlbedo minAlbedo 8 2 6 true
  ]
  if (display-mode = "reference evapotranspiration (ETr) (mm)")
  [
    let minETr min [p_ETr] of patches
    let maxETr max [p_ETr] of patches

    ask patches
    [
      set pcolor 12 + 6 * (p_ETr - minETr) / (maxETr - minETr)
    ]
    set-legend-continuous-range maxETr minETr 18 12 6 true
  ]
  if (display-mode = "runoff (mm)")
  [
    let minRunoff min [p_runoff] of patches
    let maxRunoff max [p_runoff] of patches

    let rangeRunoff maxRunoff - minRunoff
    if (rangeRunoff = 0) [ set rangeRunoff 1 ]

    ask patches
    [
      set pcolor 112 + 6 * (p_runoff - minRunoff) / rangeRunoff
    ]
    set-legend-continuous-range maxRunoff minRunoff 118 112 6 true
  ]
  if (display-mode = "root zone depth (mm)")
  [
    let minRootZoneDepth min [p_ecol_rootZoneDepth] of patches
    let maxRootZoneDepth max [p_ecol_rootZoneDepth] of patches

    ask patches
    [
      set pcolor 42 + 6 * (p_ecol_rootZoneDepth - minRootZoneDepth) / (maxRootZoneDepth - minRootZoneDepth)
    ]
    set-legend-continuous-range maxRootZoneDepth minRootZoneDepth 48 42 6 true
  ]
  if (display-mode = "soil water content (ratio)")
  [
    let minWaterContentRatio min [p_soil_waterContentRatio] of patches
    let maxWaterContentRatio max [p_soil_waterContentRatio] of patches

    let rangeWaterContentRatio maxWaterContentRatio - minWaterContentRatio
    if (rangeWaterContentRatio = 0) [ set rangeWaterContentRatio 1 ]

    ask patches
    [
      set pcolor 102 + 6 * (p_soil_waterContentRatio - minWaterContentRatio) / rangeWaterContentRatio
    ]
    set-legend-continuous-range maxWaterContentRatio minWaterContentRatio 108 102 6 true
  ]
  if (display-mode = "ARID coefficient")
  [
    let minARID min [p_soil_ARID] of patches
    let maxARID max [p_soil_ARID] of patches

    let rangeARID maxARID - minARID
    if (rangeARID = 0) [ set rangeARID 1 ]

    ask patches
    [
      set pcolor 12 + 6 * (p_soil_ARID - minARID) / rangeARID
    ]
    set-legend-continuous-range maxARID minARID 18 12 6 true
  ]

  ;;; other modes of display can be added here

  display-flows

end

to-report get-elevation-color [ elevationValue ]

  let elevationGradient 0

  ifelse (elevationValue < 0)
  [
    let normSubElevation (-1) * (elevationValue)
    let normSubMinElevation (-1) * (minElevation) + 1E-6
    set elevationGradient 20 + (200 * (1 - normSubElevation / normSubMinElevation))
    report rgb 0 0 elevationGradient
  ]
  [
    ;;; this fragment was adapted to also represent land that is far above sea level
    let normSupElevation elevationValue - minElevation
    let normSupMaxElevation maxElevation - minElevation + 1E-6
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

to-report get-ecologicalCommunityComposition-color [ %grass %brush %wood ]

  report rgb (240 * %grass / 100) (240 * %brush / 100) (240 * %wood / 100)

end

to-report get-coverType-color [ coverTypeName ]

  ;;; orange: grassland, yellow: wood-grass, green: shrubland, green: woodland
  let col white

  if (coverTypeName = "desert") [ set col grey ]
  if (coverTypeName = "grassland") [ set col orange ]
  if (coverTypeName = "wood-grass") [ set col yellow ]
  if (coverTypeName = "shrubland") [ set col green ]
  if (coverTypeName = "woodland") [ set col turquoise ]

  report col

end

to set-legend-elevation-and-water [ maximum minimum maxShade minShade numberOfKeys ascendingOrder? ]

  ; numberOfKeys is doubled (applies to elevation and then water)
  ; all other arguments refer to water

  set-legend-elevation numberOfKeys

  set-legend-continuous-range maximum minimum maxShade minShade numberOfKeys ascendingOrder?

end

to set-legend-elevation [ numberOfKeys ]

  set-current-plot "Legend"

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

  set maximum precision maximum 4
  set minimum precision minimum 4

  let step precision ((maximum - minimum) / numberOfKeys) 4

  if (maximum = minimum) [ set maximum maximum + 1 set step 2 ] ; this makes that at least one legend key is drawn when maximum = minimum

  let rangeValues maximum - minimum
  if (rangeValues = 0) [ set rangeValues 1 ]

  ifelse (ascendingOrder?)
  [
    let value precision minimum 4

    while [ value < maximum ]
    [
      create-temporary-plot-pen (word "" (precision value 4) "")
      set-plot-pen-color minShade + (maxShade - minShade) * (value - minimum) / rangeValues
      set value value + step
    ]
  ]
  [
    let value precision maximum 4

    while [ value > minimum ]
    [
      create-temporary-plot-pen (word "" (precision value 4) "")
      set-plot-pen-color maxShade - (maxShade - minShade) * (value - minimum) / rangeValues
      set value value - step
    ]
  ]

end

to set-legend-soil-textureType

  set-current-plot "Legend"

  foreach soil_textureTypes_display
  [
    textureTypeName ->
    create-temporary-plot-pen textureTypeName
    set-plot-pen-color get-textureType-color textureTypeName
  ]

end

to set-legend-texture [ %sandRange %siltRange %clayRange ]

  set-current-plot "Legend"

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

to set-legend-ecologicalCommunityComposition

  set-current-plot "Legend"

  ;;; red: grass, green: brush, blue: wood
  create-temporary-plot-pen "100% grass"
  set-plot-pen-color red
  create-temporary-plot-pen "100% brush"
  set-plot-pen-color green
  create-temporary-plot-pen "100% wood"
  set-plot-pen-color blue
  create-temporary-plot-pen "bare soil"
  set-plot-pen-color black

end

to set-legend-coverType

  set-current-plot "Legend"

  foreach (list "desert" "grassland" "wood-grass" "shrubland" "woodland")
  [
    coverTypeName ->
    create-temporary-plot-pen coverTypeName
    set-plot-pen-color get-coverType-color coverTypeName
  ]

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

  if (show-seaLevel-in-transects)
  [
    foreach (n-values world-width [ j -> min-pxcor + j ])
    [
      x ->
      plotxy x 0
    ]
    plot-pen-up
  ]

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

  if (show-seaLevel-in-transects)
  [
    foreach (n-values world-height [ j -> min-pycor + j ])
    [
      y ->
      plotxy 0 y
    ]
    plot-pen-up
  ]

end

to plot-precipitation-table

  clear-plot

  ;;; precipitation (mm/day) is summed by month
  foreach n-values yearLengthInDays [j -> j]
  [
    dayOfYearIndex ->
    plotxy (dayOfYearIndex + 1) (item dayOfYearIndex precipitation_yearSeries)
  ]
  plot-pen-up

end

to plot-cumPrecipitation-table

  ;;; precipitation (mm/day) is summed by month
  foreach n-values yearLengthInDays [j -> j]
  [
    dayOfYearIndex ->
    plotxy (dayOfYearIndex + 1) (item dayOfYearIndex precipitation_cumYearSeries)
  ]
  plot-pen-up

end

to plot-precipitation-table-by-month

  ;;; precipitation (mm/day) is summed by month
  let daysPerMonths [ 31 28 31 30 31 30 31 31 30 31 30 31 ] ; days per months -- (31 * 7) + (30 * 4) + 28
  foreach n-values 12 [j -> j]
  [
    month ->
    let startDay 1
    if (month = 1) [ set startDay (item 0 daysPerMonths) + 1 ]
    if (month > 1) [ set startDay sum (sublist daysPerMonths 0 month) + 1 ]
    let endDay startDay + item month daysPerMonths
    plotxy (startDay) (sum sublist precipitation_yearSeries (startDay - 1) (endDay - 1)) ; correct to list indexes (starting with 0 instead of 1)
  ]
  plot-pen-up

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE HANDLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to import-terrain

  clear-all

  ;;; load a terrain from the "terrains" folder
  ;;; corresponding to the random seed given as a parameter in the interface

  ;;; build a unique file name according to the user setting
  let filePath (word "terrains//terrain_" type-of-experiment "_w=" world-width "_h=" world-height "_a=" elev_algorithm-style "_fill-sinks=" flow_do-fill-sinks "_seed=" terrainRandomSeed)

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

          if (item globalIndex globalNames = "soil_texturetypes") [ set soil_textureTypes read-from-string item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_texturetypes_display") [ set soil_textureTypes_display read-from-string item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_hydrologicsoilgroups") [ set soil_hydrologicSoilGroups read-from-string item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_runoffcurvenumbertable") [ set soil_runOffCurveNumberTable read-from-string item globalIndex globalValues ]

          if (item globalIndex globalNames = "soil_fieldcapacity") [ set soil_fieldCapacity item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_saturation") [ set soil_saturation item globalIndex globalValues ]
          ;if (item globalIndex globalNames = "soil_minWaterholdingcapacity") [ set soil_minWaterHoldingCapacity item globalIndex globalValues ]
          ;if (item globalIndex globalNames = "soil_maxwaterholdingcapacity") [ set soil_maxWaterHoldingCapacity item globalIndex globalValues ]
          if (item globalIndex globalNames = "soil_intakerate") [ set soil_intakeRate read-from-string item globalIndex globalValues ]

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

          if (item globalIndex globalNames = "ecol_brushfrequencyinflection") [ set ecol_brushFrequencyInflection item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_brushfrequencyrate") [ set ecol_brushFrequencyRate item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_grassfrequencyinflection") [ set ecol_grassFrequencyInflection item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_grassfrequencyrate") [ set ecol_grassFrequencyRate item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_woodfrequencyinflection") [ set ecol_woodFrequencyInflection item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_woodfrequencyrate") [ set ecol_woodFrequencyRate item globalIndex globalValues ]
          ;;; add new global variables here
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
            set p_soil_hydrologicSoilGroup read-from-string item 16 thisLine
            set p_soil_coverTreatmentAndHydrologicCondition read-from-string item 17 thisLine
            set p_soil_runOffCurveNumber item 18 thisLine
            set p_soil_saturation item 19 thisLine
            set p_soil_fieldCapacity item 20 thisLine
            set p_soil_waterHoldingCapacity item 21 thisLine
            set p_soil_wiltingPoint item 22 thisLine
            set p_soil_deepDrainageCoefficient item 23 thisLine
            set p_ecol_%grass item 24 thisLine
            set p_ecol_%brush item 25 thisLine
            set p_ecol_%wood item 26 thisLine
            set p_ecol_coverType read-from-string item 27 thisLine
            ;;; add new patch variables here
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
;;;;;;;;;;;; movie generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to generate-animation

  setup
  vid:start-recorder
  repeat end-simulation-in-tick [ go vid:record-view ]
  vid:save-recording  (word "run_" behaviorspace-run-number ".mov")
  vid:reset-recorder

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; numeric generic functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-annual-sinusoid-with-fluctuation [ minValue maxValue meanFluctuation dayOfYear ]

  ;;; assuming north hemisphere, winter solstice in 21st December
  let angleAtLowestValue (360 * (31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 21) / yearLengthInDays) - 90
  ;;; assuming south hemisphere, winter solstice in 21st June
  if (southHemisphere?)
  [ set angleAtLowestValue (360 * (31 + 28 + 31 + 30 + 31 + 21) / yearLengthInDays) - 90 ]

  report max (list 0 random-normal (get-annual-sinusoid minValue maxValue dayOfYear angleAtLowestValue) meanFluctuation)

end

to-report get-annual-sinusoid [ minValue maxValue dayOfYear angleAtLowestValue ]

  let amplitude (maxValue - minValue) / 2

  report minValue + amplitude * (1 + sin (angleAtLowestValue + 360 * dayOfYear / yearLengthInDays))

  ; NOTE: sin function in NetLogo needs angle in degrees. 270º equivalent to 3 * pi / 2 and 360º equivalent to 2 * pi

end

to-report get-double-logistic-curve [ nPoints plateauValue inflection1 rate1 inflection2 rate2 ]

  let curve (list)

  foreach n-values nPoints [j -> j]
  [
    pointIndex ->
    set curve lput (get-point-in-double-logistic pointIndex plateauValue inflection1 rate1 inflection2 rate2) curve
  ]

  report curve

end

to-report get-point-in-double-logistic [ pointIndex plateauValue inflection1 rate1 inflection2 rate2 ]

  report (plateauValue / (1 + exp((inflection1 - pointIndex) * rate1))) + ((1 - plateauValue) / (1 + exp((inflection2 - pointIndex) * rate2)))

end

to-report clamp01 [ value ]
  report min (list 1 (max (list 0 value)))
end

to-report clampMin0 [ value ]
  report (max (list 0 value))
end

to-report clampMinMax [ value minValue maxValue ]
  report min (list maxValue (max (list minValue value)))
end
@#$#@#$#@
GRAPHICS-WINDOW
711
90
1187
567
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
1229
83
1596
569
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

INPUTBOX
58
252
164
312
terrainRandomSeed
0.0
1
0
Number

MONITOR
496
25
597
70
NIL
landRatio
4
1
11

SLIDER
222
228
500
261
par_elev_seaLevelReferenceShift
par_elev_seaLevelReferenceShift
-1000
round max (list maxElevation elev_rangeHeight)
0.0
1
1
m
HORIZONTAL

MONITOR
332
71
430
116
sdElevation
precision sdElevation 4
4
1
11

MONITOR
429
71
511
116
minElevation
precision minElevation 4
4
1
11

MONITOR
505
71
592
116
maxElevation
precision maxElevation 4
4
1
11

BUTTON
344
263
552
296
refresh after changing sea level
refresh-view-after-seaLevel-change
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
334
25
426
70
NIL
count patches
0
1
11

MONITOR
424
25
489
70
maxDist
precision maxDist 4
4
1
11

PLOT
769
680
1125
800
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
"pen-1" 1.0 1 -2674135 true "" "histogram n-values plot-y-max [j -> 0]"

CHOOSER
33
319
199
364
elev_algorithm-style
elev_algorithm-style
"NetLogo" "C#"
1

SWITCH
969
19
1089
52
show-flows
show-flows
0
1
-1000

PLOT
691
561
1187
681
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
681
86
714
564
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
708
59
1194
92
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
1186
83
1346
568
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
1435
595
1551
628
update transects
update-transects
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
1225
623
1376
656
show-transects
show-transects
1
1
-1000

SWITCH
46
368
193
401
flow_do-fill-sinks
flow_do-fill-sinks
0
1
-1000

INPUTBOX
209
339
394
399
par_riverWaterPerFlowAccumulation
0.0
1
0
Number

CHOOSER
48
125
182
170
type-of-experiment
type-of-experiment
"random" "user-defined" "defined by expNumber"
0

BUTTON
11
10
70
43
NIL
setup
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

BUTTON
39
181
191
214
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

TEXTBOX
84
229
149
247
TERRAIN
14
0.0
1

INPUTBOX
25
58
99
118
randomSeed
0.0
1
0
Number

BUTTON
192
10
247
43
NIL
go
T
1
T
OBSERVER
NIL
4
NIL
NIL
1

MONITOR
391
594
573
631
NIL
temperature_annualMinAt2m
2
1
9

BUTTON
74
11
129
44
NIL
go
NIL
1
T
OBSERVER
NIL
2
NIL
NIL
1

MONITOR
391
560
576
597
NIL
temperature_annualMaxAt2m
2
1
9

INPUTBOX
102
58
215
118
end-simulation-in-tick
0.0
1
0
Number

SLIDER
22
636
393
669
temperature_mean-daily-fluctuation
temperature_mean-daily-fluctuation
0
20
0.0
0.1
1
ºC  (default: 5)
HORIZONTAL

SLIDER
22
671
389
704
temperature_daily-lower-deviation
temperature_daily-lower-deviation
0
20
0.0
0.1
1
ºC  (default: 5)
HORIZONTAL

SLIDER
23
704
390
737
temperature_daily-upper-deviation
temperature_daily-upper-deviation
0
20
0.0
0.1
1
ºC  (default: 5)
HORIZONTAL

SLIDER
20
562
391
595
temperature_annual-max-at-2m
temperature_annual-max-at-2m
temperature_annual-min-at-2m
50
0.0
0.1
1
ºC  (default: 40)
HORIZONTAL

SLIDER
22
599
386
632
temperature_annual-min-at-2m
temperature_annual-min-at-2m
-10
temperature_annual-max-at-2m
15.0
0.1
1
ºC  (default: 15)
HORIZONTAL

MONITOR
394
634
574
671
NIL
temperature_meanDailyFluctuation
2
1
9

MONITOR
390
670
564
707
NIL
temperature_dailyLowerDeviation
2
1
9

MONITOR
392
706
566
743
NIL
temperature_dailyUpperDeviation
2
1
9

PLOT
1597
185
2147
369
Temperature
days
ºC
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" "plot temperature"
"min" 1.0 0 -13345367 true "" "plot minTemperature"
"max" 1.0 0 -2674135 true "" "plot maxTemperature"

SLIDER
24
798
419
831
solar_annual-max
solar_annual-max
solar_annual-min
7
0.0
0.001
1
kWh/m2 (default: 7)
HORIZONTAL

SLIDER
24
759
421
792
solar_annual-min
solar_annual-min
2
solar_annual-max
3.0
0.001
1
kWh/m2 (default: 3)
HORIZONTAL

SLIDER
25
836
418
869
solar_mean-daily-fluctuation
solar_mean-daily-fluctuation
0
4
0.0
0.001
1
kWh/m2 (default: 1)
HORIZONTAL

PLOT
1598
372
2096
514
Solar radiation
days
KWh/m2
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range (precision (solar_annualMin - solar_meanDailyFluctuation - 0.1) 2) (precision (solar_annualMax + solar_meanDailyFluctuation + 0.1) 2)" "set-plot-y-range (precision (solar_annualMin - solar_meanDailyFluctuation - 0.1) 2) (precision (solar_annualMax + solar_meanDailyFluctuation + 0.1) 2)"
PENS
"default" 1.0 0 -16777216 true "" "plot solarRadiation / 3.6"

MONITOR
424
755
536
792
NIL
solar_annualMin
3
1
9

MONITOR
422
793
536
830
NIL
solar_annualMax
3
1
9

MONITOR
421
833
579
870
NIL
solar_meanDailyFluctuation
3
1
9

MONITOR
679
10
758
55
NIL
currentYear
0
1
11

MONITOR
771
10
885
55
NIL
currentDayOfYear
0
1
11

CHOOSER
1249
20
1522
65
display-mode
display-mode
"elevation and surface water depth (m)" "elevation (m)" "surface water depth (mm)" "soil formative erosion" "soil depth (mm)" "soil texture" "soil texture types" "soil run off curve number" "soil water wilting point" "soil water holding capacity" "soil water field capacity" "soil water saturation" "soil deep drainage coefficient" "ecological community composition" "cover type" "albedo" "reference evapotranspiration (ETr) (mm)" "runoff (mm)" "root zone depth (mm)" "soil water content (ratio)" "ARID coefficient"
0

BUTTON
1117
20
1241
53
refresh display
refresh-to-display-mode
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
131
10
191
43
+ year
repeat 365 [ go ]
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
632
815
1033
848
precipitation_yearly-mean
precipitation_yearly-mean
0
1000
0.0
1.0
1
mm/year (default: 400)
HORIZONTAL

SLIDER
631
847
1033
880
precipitation_yearly-sd
precipitation_yearly-sd
0
250
0.0
1.0
1
mm/year (default: 130)
HORIZONTAL

SLIDER
22
889
430
922
precipitation_daily-cum_n-samples
precipitation_daily-cum_n-samples
0
300
0.0
1.0
1
(default: 200)
HORIZONTAL

SLIDER
22
926
430
959
precipitation_daily-cum_max-sample-size
precipitation_daily-cum_max-sample-size
1
20
0.0
1.0
1
(default: 10)
HORIZONTAL

SLIDER
22
966
572
999
precipitation_daily-cum_plateau-value_yearly-mean
precipitation_daily-cum_plateau-value_yearly-mean
0
0.9
0.0
0.01
1
winter (mm)/summer (mm) (default: 0.1)
HORIZONTAL

SLIDER
22
998
572
1031
precipitation_daily-cum_plateau-value_yearly-sd
precipitation_daily-cum_plateau-value_yearly-sd
0
0.2
0.0
0.001
1
(default: 0.05)
HORIZONTAL

SLIDER
633
892
1112
925
precipitation_daily-cum_inflection1_yearly-mean
precipitation_daily-cum_inflection1_yearly-mean
1
150
0.0
1.0
1
day of year (default: 40)
HORIZONTAL

SLIDER
711
928
1114
961
precipitation_daily-cum_inflection1_yearly-sd
precipitation_daily-cum_inflection1_yearly-sd
0
50
0.0
1.0
1
days (default: 20)
HORIZONTAL

SLIDER
711
966
1116
999
precipitation_daily-cum_rate1_yearly-mean
precipitation_daily-cum_rate1_yearly-mean
0
0.5
0.0
0.01
1
(default: 0.15)
HORIZONTAL

SLIDER
711
1003
1114
1036
precipitation_daily-cum_rate1_yearly-sd
precipitation_daily-cum_rate1_yearly-sd
0
0.1
0.0
0.01
1
(default: 0.02)
HORIZONTAL

SLIDER
1273
895
1691
928
precipitation_daily-cum_inflection2_yearly-mean
precipitation_daily-cum_inflection2_yearly-mean
150
366
0.0
1.0
1
day of year (default: 200)
HORIZONTAL

SLIDER
1274
933
1677
966
precipitation_daily-cum_inflection2_yearly-sd
precipitation_daily-cum_inflection2_yearly-sd
0
40
0.0
1
1
days (default: 20)
HORIZONTAL

SLIDER
1275
970
1680
1003
precipitation_daily-cum_rate2_yearly-mean
precipitation_daily-cum_rate2_yearly-mean
0
0.5
0.0
0.01
1
(default: 0.05)
HORIZONTAL

SLIDER
1275
1007
1678
1040
precipitation_daily-cum_rate2_yearly-sd
precipitation_daily-cum_rate2_yearly-sd
0
0.1
0.0
0.01
1
(default: 0.01)
HORIZONTAL

MONITOR
1032
814
1160
851
NIL
precipitation_yearlyMean
2
1
9

MONITOR
1033
849
1171
886
NIL
precipitation_yearlySd
2
1
9

MONITOR
429
887
602
924
NIL
precipitation_dailyCum_nSamples
2
1
9

MONITOR
428
924
582
961
NIL
precipitation_dailyCum_maxSampleSize
2
1
9

MONITOR
571
965
699
1002
NIL
precipitation_dailyCum_plateauValue_yearlyMean
2
1
9

MONITOR
572
1000
710
1037
NIL
precipitation_dailyCum_plateauValue_yearlySd
2
1
9

MONITOR
1113
892
1269
929
NIL
precipitation_dailyCum_inflection1_yearlyMean
2
1
9

MONITOR
1116
931
1270
968
NIL
precipitation_dailyCum_inflection1_yearlySd
2
1
9

MONITOR
1113
967
1269
1004
NIL
precipitation_dailyCum_rate1_yearlyMean
2
1
9

MONITOR
1116
1006
1270
1043
NIL
precipitation_dailyCum_rate1_yearlySd
2
1
9

MONITOR
1691
895
1847
932
NIL
precipitation_dailyCum_inflection2_yearlyMean
2
1
9

MONITOR
1679
936
1833
973
NIL
precipitation_dailyCum_inflection2_yearlySd
2
1
9

MONITOR
1677
971
1833
1008
NIL
precipitation_dailyCum_rate2_yearlyMean
2
1
9

MONITOR
1680
1010
1834
1047
NIL
precipitation_dailyCum_rate2_yearlySd
2
1
9

PLOT
1594
514
2185
672
precipitation
days
mm
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"precipitation" 1.0 1 -16777216 true "" "plot precipitation"
"mean ETr" 1.0 0 -2674135 true "" "plot mean[p_ETr] of patches"

PLOT
1827
673
2084
793
cumulative year precipitation
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range -0.1 1.1\nset-plot-x-range 0 (yearLengthInDays + 1)" "if (currentDayOfYear = 1) [ clear-plot set-plot-y-range -0.1 1.1 set-plot-x-range 0 (yearLengthInDays + 1) ]"
PENS
"default" 1.0 0 -16777216 true "" "plot-cumPrecipitation-table"

PLOT
1594
673
1827
793
year preciptation
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 (yearLengthInDays + 1)" ""
PENS
"default" 1.0 1 -16777216 true "" "plot-precipitation-table"

PLOT
1597
10
2260
185
Soil water content & ARID
days
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-y-range -0.1 1.1" ""
PENS
"mean ARID" 1.0 0 -16777216 true "" "plot mean [p_soil_ARID] of patches"
"mean water content ratio" 1.0 0 -13345367 true "" "plot mean [p_soil_waterContentRatio] of patches"

SLIDER
22
438
264
471
par_ecol_minRootZoneDepth
par_ecol_minRootZoneDepth
0
par_ecol_maxRootZoneDepth
200.0
1
1
mm3/mm3
HORIZONTAL

MONITOR
514
436
679
473
root zone depth range
(word \"min = \" (precision ecol_minRootZoneDepth 4) \", max = \" (precision ecol_maxRootZoneDepth 4))
2
1
9

SLIDER
23
471
264
504
par_ecol_minAlbedo
par_ecol_minAlbedo
0
par_ecol_maxAlbedo
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
513
474
678
511
albedo range
(word \"min = \" (precision ecol_minAlbedo 4) \", max = \" (precision ecol_maxAlbedo 4))
2
1
9

SLIDER
263
471
513
504
par_ecol_maxAlbedo
par_ecol_maxAlbedo
par_ecol_minAlbedo
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
266
438
513
471
par_ecol_maxRootZoneDepth
par_ecol_maxRootZoneDepth
par_ecol_minRootZoneDepth
3000
0.0
1
1
mm3/mm3
HORIZONTAL

TEXTBOX
29
511
681
529
=============================WEATHER=============================
14
0.0
1

TEXTBOX
27
412
670
430
========================ECOLOGICAL COMMUNITY========================
14
0.0
1

TEXTBOX
285
308
572
340
========= RIVER =========
14
0.0
1

MONITOR
402
349
555
386
NIL
riverWaterPerFlowAccumulation
7
1
9

MONITOR
501
227
651
264
NIL
elev_seaLevelReferenceShift
2
1
9

SWITCH
1205
591
1411
624
show-seaLevel-in-transects
show-seaLevel-in-transects
1
1
-1000

MONITOR
549
345
670
390
land units with river
landWithRiver
0
1
11

MONITOR
224
124
364
169
NIL
mostCommonTextureType
0
1
11

MONITOR
365
124
515
169
NIL
meanRunOffCurveNumber
2
1
11

MONITOR
224
169
364
214
NIL
meanWaterHoldingCapacity
4
1
11

MONITOR
363
169
520
214
NIL
meanDeepDrainageCoefficient
4
1
11

MONITOR
532
146
668
191
NIL
mostCommonCoverType
0
1
11

TEXTBOX
1433
631
1566
653
(transects plots will be updated in the following tick)
9
0.0
1

SWITCH
22
531
161
564
southHemisphere?
southHemisphere?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

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
