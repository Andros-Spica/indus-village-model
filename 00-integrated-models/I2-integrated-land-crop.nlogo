;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Integrated Land Crop model (I2)
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
  patchWidth
  maxDist

  yearLengthInDays

  ;;;; Soil Water Balance model global
  soil_rootWaterUptakeCoefficient        ; (root) Water Uptake coefficient (mm^3.mm^-3) (MUF)

  ;;;; Crop model
  crop_f_Solar_max ; fSolar_max is the maximum fraction of radiation interception that a crop can reach
  ; Zhao et al. 2019 note: fSolar_max is considered as a management parameter, not a crop parameter, to account for different plant spacings. For most high-density crops, this value is set at 0.95.

  ; randomSeed (GUI): seed of random number generator used for setting parameters and general stocahstic processes.

  ;*****************************************************************************************************************
  ;;; imported table inputs
  ;;;;; hydrologic Soil Groups table
  soil_textureTypes                           ; Types of soil according to % of sand, silt and clay (ternary diagram) established by USDA
  soil_hydrologicSoilGroups                   ; USDA classification of soils according to water infiltration (A, B, C, and D) per each texture type

  ;;;;; run off curve number table
  soil_runOffCurveNumberTable                 ; table (list of lists) with run off curve numbers of Hydrologic Soil Group (columns) combination of cover type-treatment-hydrologic condition

  ;;;;; Field Capacity and Water Holding capacity table
  soil_fieldCapacity                   ; field capacity (fraction of soil volume) per texture type
  soil_saturation                      ; saturation (fraction of soil volume) per texture type
  soil_intakeRate                      ; intake rate (mm/hour) per texture type
  soil_minWaterHoldingCapacity         ; minimum and maximum water holding capacity (in/ft) per texture type (not currently used)
  soil_maxWaterHoldingCapacity

  ;;;;; albedo table
  ecol_albedoTable                            ; table (list of lists) with min/max abedo by latitude range (columns) and broadband and cover type (and description)

  ;;;;; ecological component table
  ecol_ecologicalComponents      ; ecological component names (order)
  ecol_maxRootDepth              ; maximum root depth (mm) of each vegetation component (to be used as root zone depth)
  ecol_biomass                   ; biomass (g/m^2) of each vegetation component (above ground biomass (AGB))
  ecol_recoveryLag               ; recovery lag (days) of each vegetation component
  ecol_waterStressSensitivity    ; water stress sensitivity (%maxAffected/%total*day) of each vegetation component

  ;;;;; crop table
  crop_typesOfCrops                   ; common name of crop
  ;;;; Species-specific
  crop_RUE                            ; Radiation use efficiency (above ground only and without respiration) (g MJ−1 m-2)
  crop_T_base                         ; Base temperature for phenology development and growth (ºC)
  crop_T_opt                          ; Optimal temperature for biomass growth (ºC)
  crop_I_50maxH                       ; The maximum daily reduction in I50B due to heat stress (ºC d)
  crop_I_50maxW                       ; The maximum daily reduction in I50B due to drought stress (ºC d)
  crop_T_heat                         ; Threshold temperature to start accelerating senescence from heat stress (ºC). In the Zhao et al. 2019, named as T_max
  crop_T_extreme                      ; The extreme temperature threshold when RUE becomes 0 due to heat stress (ºC)
  crop_S_Water                        ; sensitivity of crop RUE to the ARID index (representing water shortage; see below)
  ;;;; Cultivar-specific
  crop_T_sum                          ; Cumulative temperature requirement from sowing to maturity (ºC d)
  crop_HI                             ; Potential harvest index
  crop_I_50A                          ; Cumulative temperature requirement for leaf area development to intercept 50% of radiation (ºC d)
  crop_I_50B                          ; Cumulative temperature till maturity to reach 50% radiation interception due to leaf senescence (ºC d)
  ;;;; management
  crop_sugSowingDay                   ; sowing day (day of year)
  crop_sugHarvestingDay               ; harvesting day (day of year)
  ;;;; root
  crop_rootZoneDepth                  ; root zone depth (mm)

  ;*****************************************************************************************************************
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

  ;;;; ecological community
  ecol_grassFrequencyInflection          ; flow accumulation required for having 50% of grass coverage (inflection point of the logistic curve)
  ecol_grassFrequencyRate                ; rate of increase in percentage of grass coverage, depending on flow_accumulation (rate or slope parameter of the logistic curve)
  ecol_brushFrequencyInflection          ; flow accumulation required for having 50% of brush coverage (inflection point of the logistic curve)
  ecol_brushFrequencyRate                ; rate of increase in percentage of brush coverage, depending on flow_accumulation (rate or slope parameter of the logistic curve)
  ecol_woodFrequencyInflection           ; flow accumulation required for having 50% of wood coverage (inflection point of the logistic curve)
  ecol_woodFrequencyRate                 ; rate of increase in percentage of wood coverage, depending on flow_accumulation (rate or slope parameter of the logistic curve)

  ;;;; derived measures
  landRatio                            ; the ratio of land units above seaLevel.
  minElevation                         ; statistics on the elevation of land units.
  sdElevation
  maxElevation

  landWithRiver                        ; count of land units with passing river
  maxFlowAccumulation

  mostCommonTextureType       ; the most common of texture type, see soil_textureTypes

  ;*****************************************************************************************************************

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

  ;;;; Solar radiation (MJ/m2)
  solar_annualMax
  solar_annualMin
  solar_meanDailyFluctuation

  ;;; RIVER -------------------------------------------------------------------------------
  riverWaterPerFlowAccumulation ; average river stage increment per flow accumulation at the river's starting land unit ( mm (height) / m^2 (area) ).
                                ; Because there are many factors subtracting river flow (assuming that the catchment area is large enough,
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

  ;;; Inundation algorithm
  errorToleranceThreshold       ; in metres
                                ; NOTE: this is an arbitrary limit to the precision of the inundation algorithm.
                                ; Differences in height (i.e. elevation + water depth) that amount to less than this value will be ignored.
                                ; It also serves as the step used to redistribute the water depth among patches of a neighborhood.

  ;;; CROP ---------------------------------------------------------------------------------
  crop_intensity                 ; crop cover as percentage of patch area
  crop_selection                 ; list of crop common names to be cultivated in every patch
 ; NOTE: these are a temporary setting exclusive to this version and to be replaced with agent decision-making in I3

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

  crop_sowingDay
  crop_harvestingDay

  ;;;; counters and final measures

  ;;;;; ecological communities and cover
  mostCommonCoverType        ; the most common of cover type, see p_ecol_coverType

  ;;;;; soil water properties
  meanRunOffCurveNumber       ; mean runoff curve number of land units
  meanWaterHoldingCapacity    ; mean water holding capacity of land units (fraction of soil volume)
  meanDeepDrainageCoefficient ; mean deep drainage coefficient (1/day)
]

patches-own
[
  ;========= LAND model ======================================================================
  ;;; keep all LAND model variables in place and in order or remember to update "import-terrain" procedure
  ;;; topography
  elevation             ; average elevation above reference of the land unit (metres).
                        ; The reference is an arbitrary elevation from which all
                        ; algorithms will sculpt the terrain.
  elevation_original    ; elevation imported from the terrain file before recentrering

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

  ;;; initial ecological communities
  p_initEcol_%grass                 ; percentage of grass vegetation (surface cover) in ecological community
  p_initEcol_%brush                 ; percentage of brush/shrub vegetation (surface cover) in ecological community
  p_initEcol_%wood                  ; percentage of wood vegetation (surface cover) in ecological community

  ;========= SOIL WATER BALANCE model ======================================================================

  ;;; surface water
  p_water                           ; surface water ( mm (height) / m^2 (area) )
  p_runoff                          ; Daily runoff ( mm (height) / m^2 (area) )

  ;;; soil water
  p_ecol_rootZoneDepth              ; root zone depth (mm).
  p_soil_waterContent               ; Water content in the soil profile for the rooting depth ( mm (height) / m^2 (area) )
  p_soil_waterContentRatio          ; Volumetric Soil Water content (fraction : mm.mm-1). calculated as WAT/z

  ;;; transpiration
  p_netSolarRadiation               ; net solar radiation discount reflection or albedo (MJ m-2)
  p_ETr                             ; reference evapotranspiration ( mm (height) / m^2 (area) )

  p_soil_ARID                       ; ARID index after Woli et al. 2012, ranging form 0 (no water shortage) to 1 (extreme water shortage)
  p_soil_ARID_yearSeries            ; registers daily values of ARID of the current year (used to export data)
  p_soil_ARID_yearSeries_lastYear   ; saves daily values of ARID of the last year (used to export data)

  ;======= I1 variables ======================================================================================

  ;;; soil water properties
  p_soil_hydrologicSoilGroup  ; USDA simplification of soil texture types into four categories

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

  ;;; ecological communities and soil cover
  p_ecol_%grass                     ; percentage of grass vegetation (surface cover) in ecological community
  p_ecol_%brush                     ; percentage of brush/shrub vegetation (surface cover) in ecological community
  p_ecol_%wood                      ; percentage of wood vegetation (surface cover) in ecological community

  p_ecol_%crop                      ; percentage of crop cultivation in ecological community

  p_ecol_%water                     ; percentage of water (surface cover) in ecological community

  p_ecol_coverType                  ; cover type summarising the composition of vegetation types in ecological community.
                                    ; Namely, they are: "desert", "grassland", "wood-grass", "shrubland", and "woodland"
                                    ; see "03-land-model/ternaryPlots/coverTypePerEcologicalCommunity.png"

  p_ecol_albedo                     ; albedo or percentage of solar radiation reflected by soil or soil cover

  p_ecol_biomass                    ; total biomass (g/m^2) of ecological communities (vegetation as proxy)

  ;========= I2 variables ======================================================================

  ;;; main variables
  p_crop_frequency                  ; frequency in % of p_ecol_%crop of each crop in typesOfCrop
  p_crop_TT                         ; cumulative mean temperature (ºC day)
  p_crop_biomass                    ; crop biomass (g)
  p_crop_totalBiomass               ; total crop biomass (g)
  p_crop_yield                      ; crop biomass harvested (g)
  p_crop_totalYield                 ; total crop biomass harvested (g)

  ;;; auxiliar variables
  p_crop_biomass_rate               ; daily change in plant biomass (g)
  p_crop_f_solar                    ; the fraction of solar ra- diation intercepted by a crop canopy
  p_crop_I_50Blocal                 ; The cumulative temperature required to reach 50% of radiation interception during canopy senescence (I50B) (value affected by heat and drought stress)
  p_crop_f_temp                     ; temperature impact
  p_crop_f_heat                     ; heat stress
  p_crop_f_water                    ; drought stress
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  ; --- import terrain -----------------------

  import-terrain

  ; --- import tables ------------------------

  load-hydrologic-soil-groups-table

  load-runoff-curve-number-table

  load-soil-water-table

  load-albedo-table

  load-ecological-component-table

  load-crops-table

  ; --- loading/testing parameters -----------

  set-constants

  set-parameters

  ; --- core procedures ----------------------

  set currentDayOfYear 1

  setup-patches

  update-weather

  update-water

  update-crop-extension

  rescale-ecological-communities

  update-ecological-communities

  update-soil-cover

  ; --- output handling ------------------------

  set-terrain-output-stats

  update-output-stats

  setup-patch-coordinates-labels "bottom" "left"

  setup-transect

  refresh-to-display-mode

  print-crop-table

  setup-plot-crop

  update-plot-crop

  ; -- time -------------------------------------

  reset-ticks

end

to set-constants

  ; "constants" are variables that will not be explored as parameters
  ; and may be used during a simulation.

  ; land units are 100m x 100m or 1 ha = 10,000 m^2
  set patchWidth 100
  set patchArea patchWidth * patchWidth

  set yearLengthInDays 365

  ;;; Ordered list of soil texture types used for visualisation
  ;;; this order corresponds to an approximation to the soil texture palette (red: sand, green: silt, blue: clay)
  set soil_textureTypes_display (list
    "Sand"             "Loamy sand"        "Sandy loam"     ; red         orange  brown
    "Loam"             "Silt loam"         "Silt"           ; yellow      green   lime
    "Silty clay loam"  "Silty clay"        "Clay"           ; turquoise   cyan    sky
    "Clay loam"        "Sandy clay"        "Sandy clay loam"; blue        violet  magenta
  )

  ; MUF : Water Uptake coefficient (mm^3 mm^-3)
  set soil_rootWaterUptakeCoefficient 0.096

  ; inundation algorithm
  set errorToleranceThreshold 1

  ; maximum fraction of radiation interception
  set crop_f_Solar_max 0.95

end

to set-parameters

  ; set random seed
  random-seed randomSeed

  set maxDist (sqrt (( (max-pxcor - min-pxcor) ^ 2) + ((max-pycor - min-pycor) ^ 2)) / 2)

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

    ;;; River water (dependent on flow_riverAccumulationAtStart, which is set by Land model)
    set riverWaterPerFlowAccumulation par_riverWaterPerFlowAccumulation
  ]
  if (type-of-experiment = "random")
  [
    ;;; sometimes use values from user interface as a maximum for random uniform distributions

    ;;; set sea level (no more just a display issue; it is relevant for ETr and limiting coastlines)
    set elev_seaLevelReferenceShift -1 * random 1000

    ;;; weather generation
    set temperature_annualMaxAt2m 15 + random-float 25
    set temperature_annualMinAt2m -15 + random-float 30
    set temperature_meanDailyFluctuation random-float 5
    set temperature_dailyLowerDeviation random-float 10
    set temperature_dailyUpperDeviation random-float 10

    set solar_annualMin 1.5 + random-float 15
    set solar_annualMax 20 + random-float 10
    set solar_meanDailyFluctuation 3 + random-float 3

    set precipitation_yearlyMean 200 + random-float 800
    set precipitation_yearlySd random-float 200
    set precipitation_dailyCum_nSamples 100 + random 200
    set precipitation_dailyCum_maxSampleSize 5 + random 20
    set precipitation_dailyCum_plateauValue_yearlyMean 0.2 + random-float 0.6
    set precipitation_dailyCum_plateauValue_yearlySd random-float 0.4
    set precipitation_dailyCum_inflection1_yearlyMean 40 + random 140
    set precipitation_dailyCum_inflection1_yearlySd 20 + random 80
    set precipitation_dailyCum_rate1_yearlyMean 0.01 + random-float 0.07
    set precipitation_dailyCum_rate1_yearlySd 0.004 + random-float 0.02
    set precipitation_dailyCum_inflection2_yearlyMean 180 + random 140
    set precipitation_dailyCum_inflection2_yearlySd 20 + random 80
    set precipitation_dailyCum_rate2_yearlyMean 0.01 + random-float 0.07
    set precipitation_dailyCum_rate2_yearlySd 0.004 + random-float 0.02

    ;;; River water (effect on p_water depends on flow_riverAccumulationAtStart, which is set by the Land model)
    set riverWaterPerFlowAccumulation 1E-4 + random-float 0.00099 ; range between 1E-4 and 1E-5

    ;;; NOTES about calibration:
    ;;; Global Horizontal Irradiation can vary from about 2 to 7 KWh/m-2 per day.
    ;;; (conversion kWh/m2 to MJ/m2 is 1 : 3.6)
    ;;; See approx. values in https://globalsolaratlas.info/
    ;;; and https://www.researchgate.net/publication/271722280_Solmap_Project_In_India%27s_Solar_Resource_Assessment
    ;;; see general info in http://www.physicalgeography.net/fundamentals/6i.html
  ]
  if (type-of-experiment = "precipitation-variation")
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

    set precipitation_yearlyMean 50 + random-float 950
    set precipitation_yearlySd precipitation_yearly-sd
    set precipitation_dailyCum_nSamples precipitation_daily-cum_n-samples
    set precipitation_dailyCum_maxSampleSize precipitation_daily-cum_max-sample-size
    set precipitation_dailyCum_plateauValue_yearlyMean 0.2 + random-float 0.6
    set precipitation_dailyCum_plateauValue_yearlySd precipitation_daily-cum_plateau-value_yearly-sd
    set precipitation_dailyCum_inflection1_yearlyMean precipitation_daily-cum_inflection1_yearly-mean
    set precipitation_dailyCum_inflection1_yearlySd precipitation_daily-cum_inflection1_yearly-sd
    set precipitation_dailyCum_rate1_yearlyMean precipitation_daily-cum_rate1_yearly-mean
    set precipitation_dailyCum_rate1_yearlySd precipitation_daily-cum_rate1_yearly-sd
    set precipitation_dailyCum_inflection2_yearlyMean precipitation_daily-cum_inflection2_yearly-mean
    set precipitation_dailyCum_inflection2_yearlySd precipitation_daily-cum_inflection2_yearly-sd
    set precipitation_dailyCum_rate2_yearlyMean precipitation_daily-cum_rate2_yearly-mean
    set precipitation_dailyCum_rate2_yearlySd precipitation_daily-cum_rate2_yearly-sd

    ;;; River water (dependent on flow_riverAccumulationAtStart, which is set by Land model)
    set riverWaterPerFlowAccumulation par_riverWaterPerFlowAccumulation

    set randomise-crop-frequencies? false
  ]
  if (type-of-experiment = "river-variation")
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

    ;;; River water (dependent on flow_riverAccumulationAtStart, which is set by Land model)
    set riverWaterPerFlowAccumulation 0.00001 + random-float 0.00099

    set randomise-crop-frequencies? false
  ]
  if (type-of-experiment = "water-variation")
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

    set precipitation_yearlyMean 50 + random-float 950
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

    ;;; River water (dependent on flow_riverAccumulationAtStart, which is set by Land model)
    set riverWaterPerFlowAccumulation 0.00001 + random-float 0.00099

    set randomise-crop-frequencies? false
  ]
  if (type-of-experiment = "randomised-crop-frequencies")
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

    ;;; River water (dependent on flow_riverAccumulationAtStart, which is set by Land model)
    set riverWaterPerFlowAccumulation par_riverWaterPerFlowAccumulation

    set randomise-crop-frequencies? true
  ]

  ;;; scenario settings:

  ;;; precipitation
  set precipitation_yearlyMean precipitation_yearlyMean + precipitation_yearly-mean-bias

  ;;; crop management parameters (never not randomised)
  ;;; crop intensity on every patch
  set crop_intensity crop-intensity

  ;;; convert crops-selected from string to list
  set crop_selection read-from-string crop-selection

  ;;; sowing/harvest dates are initialised as the ones suggested in cropTable.csv
  set crop_sowingDay crop_sugSowingDay
  set crop_harvestingDay crop_sugHarvestingDay

end

to parameters-check

  ;;; check if values were reset to 0 (NetLogo does that from time to time...!)
  ;;; and set default values (assuming they are not 0)

  ;;; sea level shift
  if (par_elev_seaLevelReferenceShift = 0)                       [ set par_elev_seaLevelReferenceShift                       -1000 ]


  ;;; the default values of weather parameters aim to broadly represent conditions in Haryana, NW India.

  if (temperature_annual-max-at-2m = 0)                          [ set temperature_annual-max-at-2m                             37 ]
  if (temperature_annual-min-at-2m = 0)                          [ set temperature_annual-min-at-2m                             12.8 ]
  if (temperature_mean-daily-fluctuation = 0)                    [ set temperature_mean-daily-fluctuation                        2.2 ]
  if (temperature_daily-lower-deviation = 0)                     [ set temperature_daily-lower-deviation                         6.8 ]
  if (temperature_daily-upper-deviation = 0)                     [ set temperature_daily-upper-deviation                         7.9 ]

  if (solar_annual-max = 0)                                      [ set solar_annual-max                                          24.2 ]
  if (solar_annual-min = 0)                                      [ set solar_annual-min                                          9.2 ]
  if (solar_mean-daily-fluctuation = 0)                          [ set solar_mean-daily-fluctuation                              3.3 ]

  if (precipitation_yearly-mean = 0)                             [ set precipitation_yearly-mean                               489 ]
  if (precipitation_yearly-sd = 0)                               [ set precipitation_yearly-sd                                 142.2 ]
  if (precipitation_daily-cum_n-samples = 0)                      [ set precipitation_daily-cum_n-samples                      200 ]
  if (precipitation_daily-cum_max-sample-size = 0)               [ set precipitation_daily-cum_max-sample-size                  10 ]
  if (precipitation_daily-cum_plateau-value_yearly-mean = 0)     [ set precipitation_daily-cum_plateau-value_yearly-mean         0.25 ]
  if (precipitation_daily-cum_plateau-value_yearly-sd = 0)       [ set precipitation_daily-cum_plateau-value_yearly-sd           0.1 ]
  if (precipitation_daily-cum_inflection1_yearly-mean = 0)       [ set precipitation_daily-cum_inflection1_yearly-mean           40 ]
  if (precipitation_daily-cum_inflection1_yearly-sd = 0)         [ set precipitation_daily-cum_inflection1_yearly-sd             5 ]
  if (precipitation_daily-cum_rate1_yearly-mean = 0)             [ set precipitation_daily-cum_rate1_yearly-mean                 0.07 ]
  if (precipitation_daily-cum_rate1_yearly-sd = 0)               [ set precipitation_daily-cum_rate1_yearly-sd                   0.02 ]
  if (precipitation_daily-cum_inflection2_yearly-mean = 0)       [ set precipitation_daily-cum_inflection2_yearly-mean           240 ]
  if (precipitation_daily-cum_inflection2_yearly-sd = 0)         [ set precipitation_daily-cum_inflection2_yearly-sd             20 ]
  if (precipitation_daily-cum_rate2_yearly-mean = 0)             [ set precipitation_daily-cum_rate2_yearly-mean                 0.08 ]
  if (precipitation_daily-cum_rate2_yearly-sd = 0)               [ set precipitation_daily-cum_rate2_yearly-sd                   0.02 ]

  if (par_riverWaterPerFlowAccumulation = 0)                     [ set par_riverWaterPerFlowAccumulation                        1E-4 ]

  if (crop-intensity = 0)                                        [ set crop-intensity                                            50 ]
  if (crop-selection = "" or crop-selection = "0" or crop-selection = 0) [ set crop-selection     (word (map [i -> (word "\"" i "\"")] crop_typesOfCrops)) ]
  if (crop-to-display = "" or crop-to-display = "0" or crop-to-display = 0) [ set crop-to-display          (first crop_typesOfCrops) ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set par_elev_seaLevelReferenceShift                       -1000

  set temperature_annual-max-at-2m                             37
  set temperature_annual-min-at-2m                             12.8
  set temperature_mean-daily-fluctuation                        2.2
  set temperature_daily-lower-deviation                         6.8
  set temperature_daily-upper-deviation                         7.9

  set solar_annual-max                                          24.2
  set solar_annual-min                                          9.2
  set solar_mean-daily-fluctuation                              3.3

  set precipitation_yearly-mean                               489
  set precipitation_yearly-sd                                 142.2
  set precipitation_daily-cum_n-samples                       200
  set precipitation_daily-cum_max-sample-size                  10
  set precipitation_daily-cum_plateau-value_yearly-mean         0.25
  set precipitation_daily-cum_plateau-value_yearly-sd           0.1
  set precipitation_daily-cum_inflection1_yearly-mean           40
  set precipitation_daily-cum_inflection1_yearly-sd             5
  set precipitation_daily-cum_rate1_yearly-mean                 0.07
  set precipitation_daily-cum_rate1_yearly-sd                   0.02
  set precipitation_daily-cum_inflection2_yearly-mean           240
  set precipitation_daily-cum_inflection2_yearly-sd             20
  set precipitation_daily-cum_rate2_yearly-mean                 0.08
  set precipitation_daily-cum_rate2_yearly-sd                   0.02

  set par_riverWaterPerFlowAccumulation                        1E-4

  set crop-intensity                                            50
  set randomise-crop-frequencies?                               false
  set crop-selection                   (word (map [i -> (word "\"" i "\"")] crop_typesOfCrops))
  set crop-to-display                               (first crop_typesOfCrops)

end

to setup-patches

  ask patches
  [
    ;;; recentre elevation so negative values are only below sea level
    set elevation get-recentred-elevation

    ;;; use table input data to set up soil water properties
    setup-soil-soilWaterProperties

    setup-crops

    ;;; root zone depth (mm) given initial ecological communities
    set-ecological-community-root-zone-depth

    ;;; Initial water content (mm) given initial ecological communities
    set p_soil_waterContent  p_ecol_rootZoneDepth * p_soil_fieldCapacity
  ]

  setup-river-water

end

to-report get-recentred-elevation

  ;;; get re-centre value of elevation in relation to new seaLevel
  report elevation_original - elev_seaLevelReferenceShift

end

to setup-soil-soilWaterProperties

  set p_soil_hydrologicSoilGroup item (position p_soil_textureType soil_textureTypes) soil_hydrologicSoilGroups

  set p_soil_runOffCurveNumber (get-runOffCurveNumber
    p_ecol_%grass
    p_ecol_%brush
    p_ecol_%wood
    p_ecol_%water
    p_crop_frequency
    p_soil_hydrologicSoilGroup
    )

  set p_soil_fieldCapacity get-fieldCapacity p_soil_textureType

  set p_soil_wiltingPoint get-wiltingPoint

  set p_soil_saturation get-saturation

  set p_soil_waterHoldingCapacity get-waterHoldingCapacity ;p_soil_textureType

  set p_soil_deepDrainageCoefficient get-deepDrainageCoefficient p_soil_textureType

end

to-report get-fieldCapacity [ textureType ]

  report item (position textureType soil_textureTypes) soil_fieldCapacity

end

to-report get-wiltingPoint

  ; using linear estimation
  ; See "SecondaryDocs/linearEstimationOfSoilWaterHorizons.Rmd"

  report max (list (-0.0105 + 0.0042 * p_soil_%clay) 0)

end

to-report get-saturation

  ; using linear estimation
  ; See "SecondaryDocs/linearEstimationOfSoilWaterHorizons.Rmd"

  report 0.3916 + 0.0045 * p_soil_%clay

end

to-report get-waterHoldingCapacity ;[ textureType ]

  report (p_soil_fieldCapacity - p_soil_wiltingPoint)

  ; alternative using input data water holding capacity x soil texture type
  ;let minWHC (item (position textureType soil_textureTypes) soil_minWaterHoldingCapacity)
  ;let maxWHC (item (position textureType soil_textureTypes) soil_maxWaterHoldingCapacity)

  ;report (minWHC + random-float (maxWHC - minWHC)) * 2.54 / 30.48 ; converted from in/ft to cm/cm

end

to-report get-deepDrainageCoefficient [ textureType ]

  ; get intake rate (mm/hour) of the given texture type
  let intakeRate item (position textureType soil_textureTypes) soil_intakeRate

  ; return daily intake rate divided by the volume of soil above field capacity (intake/drainage rate at saturation) as approximation of deep drainage coefficient
  ; TO-DO: ideally, data on deep drainage coefficient should be used instead.
  let soilAboveFieldCapacity (1 - p_soil_fieldCapacity) * p_soil_depth

  ifelse (soilAboveFieldCapacity < 1E-17)
  [ report 1 ] ; to avoid error when p_soil_depth = 0
  [ report min (list 1 (24 * intakeRate / soilAboveFieldCapacity)) ] ; deep drainage cannot be greater than one

end

to update-crop-extension

  ask patches
  [
    ;;; assumes that water takes preference over crops
    set p_ecol_%crop min (list (100 - p_ecol_%water) crop_intensity)
  ]

end

to rescale-ecological-communities

  ;;; assumes that water and crops takes preference over other natural ecological communities

  ask patches ;with [ p_ecol_%water > 0 or p_ecol_%crop > 0 ]
  [
    ;;; scale the final percentage to account for acquatic ecological communities and crops
    let newTotal p_ecol_%wood + p_ecol_%brush + p_ecol_%grass + p_ecol_%water + p_ecol_%crop

    if (newTotal > 100)
    [
      set p_ecol_%wood p_ecol_%wood * (100 - p_ecol_%water - p_ecol_%crop) / 100
      set p_ecol_%brush p_ecol_%brush * (100 - p_ecol_%water - p_ecol_%crop) / 100
      set p_ecol_%grass p_ecol_%grass * (100 - p_ecol_%water - p_ecol_%crop) / 100
    ]
  ]

end

to setup-river-water

  ask patches with [flow_accumulation > flow_riverAccumulationAtStart] ; patches containing the river
  [
    set p_water flow_accumulation * riverWaterPerFlowAccumulation
  ]

end

to setup-crops

  ;;; crop assignment
  ;;; NOTE: this is a temporary aspect to be replaced by agent decision-making

  set p_crop_frequency []

  foreach crop_typesOfCrops
  [
    cropName ->

    ifelse (member? cropName crop_typesOfCrops)
    [
      ifelse (randomise-crop-frequencies?)
      [
        ;;; patches have a random proportion of each crops selected
        set p_crop_frequency lput (random 100) p_crop_frequency
      ]
      [
        ;;; assign equal frequencies to each crops selected
        set p_crop_frequency lput 100 p_crop_frequency
      ]
    ]
    [
      set p_crop_frequency lput 0 p_crop_frequency
    ]
  ]

  ;;; rescale values
  let cropFrequencyTotal sum p_crop_frequency
  set p_crop_frequency map [i -> 100 * i / cropFrequencyTotal] p_crop_frequency

  ;;; initialise all crop related variables as list where items correspond to crops
  set p_crop_TT n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_biomass n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_totalBiomass n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_yield n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_totalYield n-values (length crop_typesOfCrops) [ j -> 0 ]

  set p_crop_biomass_rate n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_f_solar n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_I_50Blocal n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_f_temp n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_f_heat n-values (length crop_typesOfCrops) [ j -> 0 ]
  set p_crop_f_water n-values (length crop_typesOfCrops) [ j -> 0 ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ; --- core procedures -------------------------

  update-weather

  update-water

  update-crop-extension

  update-ecological-communities

  update-crops

  update-soil-cover

  ; --- output handling ------------------------

  update-output-stats

  refresh-to-display-mode

  update-plot-crop

  ; -- time -------------------------------------

  advance-time

  tick

  ; --- stop conditions -------------------------

  if (ticks = (end-simulation-in-year * yearLengthInDays)) [stop]

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
    set p_netSolarRadiation (1 - p_ecol_albedo / 100) * solarRadiation
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

  report (get-annual-sinusoid-with-fluctuation
    temperature_annualMinAt2m
    temperature_annualMaxAt2m
    temperature_meanDailyFluctuation
    dayOfYear
    southHemisphere?
  )

end

to update-precipitation [ dayOfYear ]

  if (dayOfYear = 1) [ set-precipitation-of-year ]

  set precipitation item (dayOfYear - 1) precipitation_yearSeries

end

to set-precipitation-of-year

  ;;; Initialisation ===================================================================

  ;;; get randomised values for parameters of the double logistic curve
  let plateauValue clamp01 (random-normal precipitation_dailyCum_plateauValue_yearlyMean precipitation_dailyCum_plateauValue_yearlySd)
  let inflection1 clampMinMax (random-normal precipitation_dailyCum_inflection1_yearlyMean precipitation_dailyCum_inflection1_yearlySd) 1 yearLengthInDays
  let rate1 clampMin0 (random-normal precipitation_dailyCum_rate1_yearlyMean precipitation_dailyCum_rate1_yearlySd)
  let inflection2 clampMinMax (random-normal precipitation_dailyCum_inflection2_yearlyMean precipitation_dailyCum_inflection2_yearlySd) 1 yearLengthInDays
  let rate2 clampMin0 (random-normal precipitation_dailyCum_rate2_yearlyMean precipitation_dailyCum_rate2_yearlySd)
  ;print (word "plateauValue = " plateauValue ", inflection1 = " inflection1 ", rate1 = " rate1 ", inflection2 = " inflection2 ", rate2 = " rate2)

  ;;; get randomised total precipitation of current year
  let totalYearPrecipitation clampMin0 (random-normal precipitation_yearlyMean precipitation_yearlySd)

  ;;; ==================================================================================

  ;;; Simulate *cumulative proportion of year precipitation*
  ;;; NOTE: double logistic curve as a proxy of the year series of daily cumulative precipitation
  set precipitation_cumYearSeries (get-cumulative-curve
    ; parameters for creatin a double logistic curve
    plateauValue inflection1 rate1 inflection2 rate2
    ; length of curve. NOTE: one more point besides lenghtOfCurve to account for the initial derivative
    (yearLengthInDays + 1)
    ; parameters for stochastically breaking down the curve into steps
    precipitation_dailyCum_nSamples precipitation_dailyCum_maxSampleSize
  )

  ;;; Derivate *daily proportion of year precipitation* from simulated *cumulative proportion of year precipitation*.
  ;;; These are the difference between day i and day i - 1
  let precipitation_propYearSeries get-incremets-from-curve precipitation_cumYearSeries
  ;;; exclude the first element (which is the extra theoretical day used for derivative calculation)
  set precipitation_propYearSeries but-first precipitation_propYearSeries

  ;;; Calculate *daily precipitation* values by multipling *daily proportions of year precipitation* by the *year total precipitation*
  set precipitation_yearSeries map [ i -> i * totalYearPrecipitation ] precipitation_propYearSeries

end

to-report get-solar-radiation [ dayOfYear ]

  ;;; get solar radiation for the current day (MJ/m2)

  report clampMin0 (get-annual-sinusoid-with-fluctuation
    solar_annualMin
    solar_annualMax
    solar_meanDailyFluctuation
    dayOfYear
    southHemisphere?
  )
  ;;; NOTE: it might be possible to decrease solar radiation depending on the current day precipitation. Additional info on precipitation effect on solar radiation is needed.

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

  reset-sea-water

  add-river-water

  add-precipitation-water

  solve-runoff-exchange

  solve-inundation-exchange

  drain-soil-water

  ask patches [ set p_ecol_%water (get-%water-surface p_water) ]

end

to reset-sea-water

  ;;; add water to fill below sea level elevation (depth)
  ask patches with [elevation < 0]
  [
    set p_water -1 * elevation * 1000
  ]

end

to add-river-water

  ask patches with [flow_accumulation > flow_riverAccumulationAtStart] ; patches containing the river or only starting point: = flow_riverAccumulationAtStart + 1]
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

  ; add incoming river water at river start to replace runoff
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
  ]
  [
    ; soil absorbes all water not running off
    set soilWaterChange potentialSoilWaterChange
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
      ;print (word p_water " + " thisPatch_h2 " * 1000 = " p_water + thisPatch_h2 * 1000)
      set p_water p_water + thisPatch_h2 * 1000 ; in mm
    ]
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

to solve-inundation-exchange

  ;;; case 3 in try-send-runoff-to can make water depth plus elevation to be uneven between neighbouring patches.
  ;;; particularly when the value of riverWaterPerFlowAccumulation is relatively high (>>1E-4)

  ;;; get the (initial) set of patches with excess water depth
  let patchesWithExcess patches with [has-excess-water-depth]

  let maxIterations 10000 ; just as a safety measure, to avoid infinite loop
  ;;; iteratively solve each patch with excess water depth
  while [any? patchesWithExcess and maxIterations > 0]
  [
    ask one-of patchesWithExcess
    [
      ;;; get the entire neighborhood (neighbors + this patch)
      let neighborhood (patch-set self neighbors)

      ;;; get the sum of surface water depth (in m) for the entire neighborhood
      let sumNeighborhoodWaterDepth sum [p_water / 1000] of neighborhood

      ;;; set all water in neighborhood to zero
      ask neighborhood [ set p_water 0 ]

      ;;; iterate n times, where n is the neighborhood sum divided by errorToleranceThreshold (rounded value)
      repeat round (sumNeighborhoodWaterDepth / errorToleranceThreshold)
      [
        ;;; get the lowest patch in the neighborhood
        ;;; NOTE: height is elevation (m) + surfaceWater (mm) / 1000
        ask min-one-of neighborhood [get-height]
        [
          ;;; add errorToleranceThreshold * 1000 to the amount of surface water depth (mm)
          set p_water p_water + errorToleranceThreshold * 1000
        ]
      ]

      ;;; ask neighbors and neighbors of neighbors (excluding this patch) to update patchesWithExcess
      ;;; NOTE: this piece of code is effective and much faster than asking all patches to update patchesWithExcess

      ;;; exclude this patch from patchesWithExcess
      set patchesWithExcess other patchesWithExcess

      ;;; get extended neighborhood
      let extendedNeighborhood other (patch-set neighbors ([neighbors] of neighbors))

      ask extendedNeighborhood
      [
        ;;; check if this patch has excess water depth,
        ;;; than add or exclude it from patchesWithExcess
        ifelse (has-excess-water-depth)
        [
          set patchesWithExcess (patch-set self patchesWithExcess)
        ]
        [
          set patchesWithExcess other patchesWithExcess
        ]
      ]
    ]

    set maxIterations maxIterations - 1
  ]

end

to-report get-height

  report elevation + p_water / 1000

end

to-report has-excess-water-depth

  let sourceHeight get-height

  report p_water > 0 and any? neighbors with [sourceHeight - get-height > errorToleranceThreshold]

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
    if (p_soil_waterContent < 0)  ;;; this happens if p_soil_waterContent is less than WATfc or WATwp
    [
      ;print self print p_soil_waterContent
      set p_soil_waterContent 0
    ]
    ifelse (p_ecol_rootZoneDepth > 0)
    [ set p_soil_waterContentRatio p_soil_waterContent / p_ecol_rootZoneDepth ]
    [ set p_soil_waterContentRatio 0]

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

;=======================================================================================================
;;; END of water flow and soil water algorithms (combined)
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

to-report get-%water-surface [ water ]

  ;;; get an estimation of the percentage of patch surface covered by free water.
  ;;; The bankfull surface width / mean bankfull depth ratio criteria is used as an approaximation.
  ;;; See: https://cfpub.epa.gov/watertrain/moduleFrame.cfm?parent_object_id=1262#:~:text=The%20width%2Fdepth%20(W%2F,the%20channel%20to%20move%20sediment
  ;;; It is assumed that
  ;;; 1. all patches have a stream shaped relief with W/D = 12 (the most common value empirically) when filled,
  ;;; 2. the bankfull surface width is smaller or equal to patchWidth
  ;;; Therefore,
  ;;; ratio = width (m) / (p_water (mm) / 1000)
  ;;; width = ratio * p_water / 1000
  ;;; and
  ;;; water% = min (list (100 * width / patchWidth) 100)

  let waterWidth 12 * water / 1000

  report clampMinMax (100 * waterWidth / patchWidth) 0 100

end

to update-ecological-communities

  ;;; update the composition of ecological communities in patches

  ask patches
  [
    apply-inundation-and-crop-effect

    apply-ecological-recolonisation

    advance-ecological-succession

    set-ecological-communities-biomass

    set-ecological-community-root-zone-depth
  ]

end

to apply-inundation-and-crop-effect

  ;;; inundation (the expansion of water surface) and crop cultivation will conquer the ecological components' area.
  ;;; The effect is assumed to affect ecological components evenly.

  let sum% p_ecol_%wood + p_ecol_%brush + p_ecol_%grass

  if (p_ecol_%water + p_ecol_%crop > 0 and sum% > 0)
  [
    set p_ecol_%wood 100 * (p_ecol_%wood / sum%) * (1 - (p_ecol_%water + p_ecol_%crop) / 100)
    set p_ecol_%brush 100 * (p_ecol_%brush / sum%) * (1 - (p_ecol_%water + p_ecol_%crop) / 100)
    set p_ecol_%grass 100 * (p_ecol_%grass / sum%) * (1 - (p_ecol_%water + p_ecol_%crop) / 100)
  ]

end

to apply-ecological-recolonisation

  ;;; if this patch has 0 of a component,
  ;;; a very small amount of that component will be added in order to activate the logistic growth

  if (p_ecol_%water + p_ecol_%crop < 100) ;;; not completely covered by water or crops
  [
    ;;; wood
    if (p_ecol_%wood = 0)
    [ set p_ecol_%wood 1E-6 ]

    ;;; brush
    if (p_ecol_%brush = 0)
    [ set p_ecol_%brush 1E-6 ]

    ;;; grass
    if (p_ecol_%grass = 0)
    [ set p_ecol_%grass 1E-6 ]
  ]

end

to advance-ecological-succession

  ;;; advance the ecological succession in this patch
  ;;; all ecological components (based on vegetation) are assumed to grow towards the initial ecological community configuration, minus the influence of water stress.
  ;;; The logistic growth model is used, where the reproductive rate (growth slope) is regulated by the frequency of the component and its proportion to a carrying capacity (here, the initial value)

  let proportionOfAvailableArea (1 - (p_ecol_%water + p_ecol_%crop) / 100)

  let recoveryRate_%wood 1 / (get-recovery-lag-of-ecological-component "wood")
  let recoveryRate_%brush 1 / (get-recovery-lag-of-ecological-component "brush")
  let recoveryRate_%grass 1 / (get-recovery-lag-of-ecological-component "grass")

  set p_ecol_%wood (p_ecol_%wood +
    recoveryRate_%wood *
    ((p_initEcol_%wood * (1 - p_soil_ARID * (get-water-stress-sensitivity-of-ecological-component "wood")) *
      proportionOfAvailableArea) - p_ecol_%wood
    )
  )

  set p_ecol_%brush (p_ecol_%brush +
    recoveryRate_%brush *
    ((p_initEcol_%brush * (1 - p_soil_ARID * (get-water-stress-sensitivity-of-ecological-component "brush")) *
      proportionOfAvailableArea) - p_ecol_%brush
    )
  )

  set p_ecol_%grass (p_ecol_%grass +
    recoveryRate_%grass *
    ((p_initEcol_%grass * (1 - p_soil_ARID * (get-water-stress-sensitivity-of-ecological-component "grass")) *
      proportionOfAvailableArea) - p_ecol_%grass
    )
  )

end

to set-ecological-communities-biomass

  set p_ecol_biomass get-ecological-communities-biomass p_ecol_%wood p_ecol_%brush p_ecol_%grass

end

to-report get-ecological-communities-biomass [ %wood %brush %grass ]

  report
  (%wood / 100) * (get-biomass-of-ecological-component "wood") +
  (%brush / 100) * (get-biomass-of-ecological-component "brush") +
  (%grass / 100) * (get-biomass-of-ecological-component "grass")

end

to set-ecological-community-root-zone-depth

  ;;; root zone depth is set to be the weighted mean of the maximum root zone depth of each crop and ecological component (water ecological community is ignored)

  let cropsRootZoneDepth (p_ecol_%crop / 100) * get-mean-max-root-depth-of-crops p_crop_frequency

  set p_ecol_rootZoneDepth cropsRootZoneDepth + get-ecological-community-root-zone-depth p_ecol_%crop p_ecol_%wood p_ecol_%brush p_ecol_%grass

  set p_ecol_rootZoneDepth min (list p_soil_depth p_ecol_rootZoneDepth) ;;; it cannot be deeper than the soil layer
  ;;; root zone depth will be 0 if there is no active (terrestrial) ecological community (100% bare soil or water)

end

to-report get-mean-max-root-depth-of-crops [ cropFrequencies ]

  report sum (map [ [i j] -> (i / 100) * j ] cropFrequencies crop_rootZoneDepth)

end

to-report get-ecological-community-root-zone-depth [ %crop %wood %brush %grass ]

  report
  (%wood / 100) * (get-max-root-depth-of-ecological-component "wood") +
  (%brush / 100) * (get-max-root-depth-of-ecological-component "brush") +
  (%grass / 100) * (get-max-root-depth-of-ecological-component "grass")

end

to-report get-max-root-depth-of-ecological-component [ ecologicalComponentName ]

  ;;; get max root depth value corresponding to ecologicalComponentName
  report item (position ecologicalComponentName ecol_ecologicalComponents) ecol_maxRootDepth

end

to-report get-biomass-of-ecological-component [ ecologicalComponentName ]

  ;;; get biomass value corresponding to ecologicalComponentName
  report item (position ecologicalComponentName ecol_ecologicalComponents) ecol_biomass

end

to-report get-recovery-lag-of-ecological-component [ ecologicalComponentName ]

  ;;; get recovery lag value corresponding to ecologicalComponentName
  report item (position ecologicalComponentName ecol_ecologicalComponents) ecol_recoveryLag

end

to-report get-water-stress-sensitivity-of-ecological-component [ ecologicalComponentName ]

  ;;; get water stress sensitivity value corresponding to ecologicalComponentName
  report item (position ecologicalComponentName ecol_ecologicalComponents) ecol_waterStressSensitivity

end

;=======================================================================================================
;;; START of SIMPLE crop model algorithms
;;; Zhao C, Liu B, Xiao L, Hoogenboom G, Boote K J, Kassie B T,
;;; Pavan W, Shelia V, Kim K S, Hernandez-Ochoa I M, Wallach D,
;;; Porter C H, Stockle C O, Zhu Y and Asseng S (2019)
;;; A SIMPLE crop model Eur. J. Agron. 104 97–106
;;; Online: https://doi.org/10.1016/j.eja.2019.01.009
;;; See also: "04-crop-model" directory within "indus-village-model".
;=======================================================================================================

to update-crops

  ask patches
  [
    foreach crop_selection
    [
      crop ->

      let cropIndex position crop crop_typesOfCrops

      if ( is-growing cropIndex )
      [
        update-biomass cropIndex

        set p_crop_totalBiomass replace-item cropIndex p_crop_totalBiomass ((item cropIndex p_crop_biomass) * (p_ecol_%crop / 100) * patchArea * (item cropIndex p_crop_frequency) / 100)
      ]

      if ( is-ripe cropIndex )
      [
        ;;; calculate harvest yield
        ifelse (item cropIndex p_crop_TT >= item cropIndex crop_T_sum)
        [
          set p_crop_yield replace-item cropIndex p_crop_yield (item cropIndex p_crop_biomass * item cropIndex crop_HI)
          set p_crop_totalYield replace-item cropIndex p_crop_totalYield ((item cropIndex p_crop_yield) * (p_ecol_%crop / 100) * patchArea * (item cropIndex p_crop_frequency) / 100)
        ]
        [
          set p_crop_yield replace-item cropIndex p_crop_yield 0

          set p_crop_totalYield replace-item cropIndex p_crop_totalYield 0
        ]

        ;;; reset biomass and auxiliary variables
        reset-crop-variables cropIndex
      ]
    ]
  ]

end

;;; PATCHES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-crop-variables [ cropIndex ]

  set p_crop_TT replace-item cropIndex p_crop_TT 0
  set p_crop_biomass replace-item cropIndex p_crop_biomass 0

  set p_crop_biomass_rate replace-item cropIndex p_crop_biomass_rate 0
  set p_crop_f_solar replace-item cropIndex p_crop_f_solar 0
  set p_crop_I_50Blocal replace-item cropIndex p_crop_I_50Blocal 0
  set p_crop_f_temp replace-item cropIndex p_crop_f_temp 0
  set p_crop_f_heat replace-item cropIndex p_crop_f_heat 0
  set p_crop_f_water replace-item cropIndex p_crop_f_water 0

end

to-report is-growing [ cropIndex ]

  let myCropSowingDay (item cropIndex crop_sowingDay)
  let myCropHarvestingDay (item cropIndex crop_harvestingDay)

  ifelse (myCropSowingDay < myCropHarvestingDay)
  [
    ; summer crop (sowing day comes before harvesting day in the Jan-Dec calendar)
    report (currentDayOfYear >= myCropSowingDay) and (currentDayOfYear < myCropHarvestingDay)
  ]
  [
    ; winter crop (harvesting day comes before sowing day in the Jan-Dec calendar; ignore first year harvest)
    report (currentDayOfYear >= myCropSowingDay) or (currentYear > 0 and currentDayOfYear < myCropHarvestingDay)
  ]

end

to-report is-ripe [ cropIndex ]

  report (currentDayOfYear = item cropIndex crop_harvestingDay)

end

to update-biomass [ cropIndex ]

  update-TT cropIndex

  update-f_Temp cropIndex

  update-f_Heat cropIndex

  update-f_Water cropIndex

  set p_crop_I_50Blocal replace-item cropIndex p_crop_I_50Blocal ((item cropIndex crop_I_50B) + (item cropIndex crop_I_50maxW) * (1 - item cropIndex p_crop_f_Water) + (item cropIndex crop_I_50maxH) * (1 - item cropIndex p_crop_f_Heat))

  update-f_Solar cropIndex

  set p_crop_biomass_rate replace-item cropIndex p_crop_biomass_rate (solarRadiation * (item cropIndex crop_RUE) * item cropIndex p_crop_f_Solar * item cropIndex p_crop_f_Temp * (clampMin0 (min (list (item cropIndex p_crop_f_Heat) (item cropIndex p_crop_f_Water)))))

  set p_crop_biomass replace-item cropIndex p_crop_biomass (item cropIndex p_crop_biomass + item cropIndex p_crop_biomass_rate)

end

to update-TT [ cropIndex ]

  let deltaTT 0

  ifelse ( temperature > item cropIndex crop_T_base )
  [
    set deltaTT temperature - item cropIndex crop_T_base
  ]
  [
    set deltaTT 0
  ]

  set p_crop_TT replace-item cropIndex p_crop_TT (item cropIndex p_crop_TT + deltaTT)

end

to update-f_Temp [ cropIndex ]

  ifelse ( temperature < item cropIndex crop_T_base )
  [
    set p_crop_f_Temp replace-item cropIndex p_crop_f_Temp 0
  ]
  [
    ifelse ( temperature >= item cropIndex crop_T_opt )
    [
      set p_crop_f_Temp replace-item cropIndex p_crop_f_Temp 1
    ]
    [
      set p_crop_f_Temp replace-item cropIndex p_crop_f_Temp ((temperature - item cropIndex crop_T_base) / (item cropIndex crop_T_opt - item cropIndex crop_T_base))
    ]
  ]

end

to update-f_Heat [ cropIndex ]

  ifelse ( maxTemperature <= item cropIndex crop_T_heat )
  [
    set p_crop_f_Heat replace-item cropIndex p_crop_f_Heat 1
  ]
  [
    ifelse ( maxTemperature > item cropIndex crop_T_extreme )
    [
      set p_crop_f_Heat replace-item cropIndex p_crop_f_Heat 0
    ]
    [
      set p_crop_f_Heat replace-item cropIndex p_crop_f_Heat ((maxTemperature - item cropIndex crop_T_heat) / (item cropIndex crop_T_extreme - item cropIndex crop_T_heat))
    ]
  ]

end

to update-f_Water [ cropIndex ]

  set p_crop_f_Water replace-item cropIndex p_crop_f_Water (1 - (item cropIndex crop_S_Water) * p_soil_ARID)

end

to update-f_Solar [ cropIndex ]

  let f_Solar_early (crop_f_Solar_max / (1 + e ^ (-0.01 * (item cropIndex p_crop_TT - item cropIndex crop_I_50A))))

  let f_Solar_late (crop_f_Solar_max / (1 + e ^ (-0.01 * (item cropIndex p_crop_TT - item cropIndex p_crop_I_50Blocal))))

  set p_crop_f_Solar replace-item cropIndex p_crop_f_Solar min (list f_Solar_early f_Solar_late)

  ;;; drought effect
  if (item cropIndex p_crop_f_Water < 0.1)
  [
    set p_crop_f_Solar replace-item cropIndex p_crop_f_Solar (item cropIndex p_crop_f_Solar * (0.9 + item cropIndex p_crop_f_Water))
  ]

end

;=======================================================================================================
;;; END of SIMPLE crop model algorithms
;;; Zhao C, Liu B, Xiao L, Hoogenboom G, Boote K J, Kassie B T,
;;; Pavan W, Shelia V, Kim K S, Hernandez-Ochoa I M, Wallach D,
;;; Porter C H, Stockle C O, Zhu Y and Asseng S (2019)
;;; A SIMPLE crop model Eur. J. Agron. 104 97–106
;;; Online: https://doi.org/10.1016/j.eja.2019.01.009
;;; See also: "04-crop-model" directory within "indus-village-model".
;=======================================================================================================

to update-soil-cover

  ask patches
  [
    set p_ecol_coverType (get-cover-type
      p_ecol_%grass
      p_ecol_%brush
      p_ecol_%wood
      p_ecol_%water
      p_ecol_%crop
      )

    set p_soil_runOffCurveNumber (get-runOffCurveNumber
      p_ecol_%grass
      p_ecol_%brush
      p_ecol_%wood
      p_ecol_%water
      p_ecol_%crop
      p_soil_hydrologicSoilGroup
      )

    set p_ecol_albedo (get-albedo
      p_ecol_%grass
      p_ecol_%brush
      p_ecol_%wood
      p_ecol_%water
      p_ecol_%crop
      p_soil_%sand
      p_soil_waterContentRatio
     )
  ]

end

to-report get-cover-type [ %grass %brush %wood %water %crop ]

  ;;; set cover type according to percentages
  ;;; The criteria used for separating cover types (to select runoff curve number) attempts to approach the one used in:
  ;;; Table 2.2 in: Cronshey R G 1986 Urban Hydrology for Small Watersheds, Technical Release 55 (TR-55).
  ;;; United States Department of Agriculture, Soil Conservation Service, Engineering Division
  ;;; See also ternary diagram "ternaryPlots/coverTypePerEcologicalCommunity.png", generated in R
  ;;; An extra category for free water surfaces was added.
  ;;; NOTE: desert (%bareSoil > 50), cropland (%crop > 50) and water (%water > 50) definitions are arbitrary.
  ;;; Pending to find information on these thresholds, if any standards exist (keep in mind they affect runoff curve number and albedo).

  if (%water > 50) [ report "free water" ]

  if (%crop > 50) [ report "cropland" ]

  let %bareSoil 100 - %grass - %brush - %wood - %water - %crop
  if (%bareSoil > 50) [ report "desert" ] ;;; if percentages are too low

  if (%grass >= 60) [ report "grassland" ]
  if (%wood >= 50 and %brush <= 50 and %grass < 40) [ report "woodland" ]
  if (%brush >= 50 and %wood < 50 and %grass < 40) [ report "shrubland" ]
  if (%brush < 60 and %wood < 60 and %grass < 60) [ report "wood-grass" ]

end

to-report get-runOffCurveNumber [ %grass %brush %wood %water %crop hydrologicSoilGroup ]

  ; cropland should be broken down once crop model is integrated (I2 model)

  let treatment "" ; defaults to no specific treatment
  let condition "good" ; defaults to "good"

  let %bareSoil (100 - %wood - %brush - %grass - %water - %crop)

  report (
  ((get-runOffCurveNumber-of-cover-and-soil "free water" condition treatment hydrologicSoilGroup) * %water / 100) +
  ((get-runOffCurveNumber-of-cover-and-soil "woodland" condition treatment hydrologicSoilGroup) * %wood / 100) +
  ((get-runOffCurveNumber-of-cover-and-soil "shrubland" condition treatment hydrologicSoilGroup) * %brush / 100) +
  ((get-runOffCurveNumber-of-cover-and-soil "grassland" condition treatment hydrologicSoilGroup) * %grass / 100) +
  ((get-runOffCurveNumber-of-cover-and-soil "cropland" condition treatment hydrologicSoilGroup) * %crop / 100) +
  ((get-runOffCurveNumber-of-cover-and-soil "desert" condition treatment hydrologicSoilGroup) * %bareSoil / 100)
  )

end

to-report get-runOffCurveNumber-of-cover-and-soil [ coverType condition treatment hydrologicSoilGroup ]

  let coverTreatmentAndHydrologicCondition get-coverTreatmentAndHydrologicCondition coverType condition treatment

  report (
    item
    (position coverTreatmentAndHydrologicCondition (item 0 soil_runOffCurveNumberTable))            ; selecting row
    (item (1 + position hydrologicSoilGroup (list "A" "B" "C" "D")) soil_runOffCurveNumberTable)    ; selecting column (skip column with coverTreatmentAndHydrologicCondition)
    )

end

to-report get-coverTreatmentAndHydrologicCondition [ coverType condition treatment ]

  ;;; correspond cover type with cover/treatment/hydrologic condition as registred in runOffCurveNumberTable
  ;;; That table, created by the USDA, holds a classification of cover conditions in the US;
  ;;; future versions should aim to calibrate this data to the cover types within the region of interest.

  ;;; NOTE: this version ignores condition and treatment and selects a single default per each coverType
  ;;; NOTE2: cropland should be broken down once crop model is integrated (I2 model)

  if (coverType = "free water")
  [
    report (word "free water | surface covered with free water | null" )
  ]
  if (coverType = "desert")
  [
    report (word "desert shrub | major plants include saltbush-greasewood-creosotebush-blackbrush-bursage-palo verde-mesquite-cactus | good")
  ]
  if (coverType = "grassland")
  [
    report (word "pasture or grassland or range | continuous forage for grazing | good")
  ]
  if (coverType = "shrubland")
  [
    report (word "brush | brush-weed-grass mixture with brush the major element | good")
  ]
  if (coverType = "woodland")
  [
    report (word "woods | used and managed but not planted | good")
  ]
  if (coverType = "wood-grass")
  [
    report (word "woods-grass combination or tree farm | lightly or only occasionally grazed | good")
  ]
  if (coverType = "cropland")
  [
    report (word "small grain crop | straight row | good")
  ]

  report ""

end

to-report get-albedo [ %grass %brush %wood %water %crop %sand waterContentRatio]

  let %bareSoil (100 - %wood - %brush - %grass - %water - %crop)

  report
  ((get-albedo-of-cover "inland water") * %water / 100) +
  ((get-albedo-of-cover "woodlands") * %wood / 100) +
  ((get-albedo-of-cover "shrublands") * %brush / 100) +
  ((get-albedo-of-cover "grasslands") * %grass / 100) +
  ((get-albedo-of-cover "croplands") * %crop / 100) +
  ((get-albedo-of-cover "wet bare not sandy soil") * (%bareSoil / 100) * (1 - %sand / 100) * waterContentRatio) +
  ((get-albedo-of-cover "dry bare not sandy soil") * (%bareSoil / 100) * (1 - %sand / 100) * (1 - waterContentRatio)) +
  ((get-albedo-of-cover "wet bare sandy soil") * (%bareSoil / 100) * (%sand / 100) * waterContentRatio) +
  ((get-albedo-of-cover "dry bare sandy soil") * (%bareSoil / 100) * (%sand / 100) * (1 - waterContentRatio))

end

to-report get-albedo-of-cover [ coverName ]

  ;;; get albedo value corresponding to coverName in ecol_albedoTable
  report item (position coverName (item 0 ecol_albedoTable)) (item 1 ecol_albedoTable)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT STATS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-terrain-output-stats

  set minElevation min [elevation] of patches

  set maxElevation max [elevation] of patches

  set sdElevation standard-deviation [elevation] of patches

  set landRatio count patches with [elevation >= 0] / count patches

  set landWithRiver count patches with [flow_accumulation >= flow_riverAccumulationAtStart]

  ;;; soil water properties (dependent only on soil texture)
  set meanWaterHoldingCapacity mean [p_soil_waterHoldingCapacity] of patches
  set meanDeepDrainageCoefficient mean [p_soil_deepDrainageCoefficient] of patches

end

to update-output-stats

  set meanRunOffCurveNumber mean [p_soil_runOffCurveNumber] of patches

  set mostCommonCoverType modes [p_ecol_coverType] of patches

  ask patches
  [
    update-ARID_yearSeries
  ]

end

to update-ARID_yearSeries

  ; if starting a new year
  if (currentDayOfYear = 1)
  [
    ; save current year as last year
    set p_soil_ARID_yearSeries_lastYear p_soil_ARID_yearSeries
    ; reset p_soil_ARID_yearSeries if starting a new year
    set p_soil_ARID_yearSeries (list)
  ]
  ; append this day ARID to ARID_yearSeries
  set p_soil_ARID_yearSeries lput p_soil_ARID p_soil_ARID_yearSeries

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to refresh-view-after-seaLevel-change

  ;;; this procedure will recentre patch elevations according to elev_seaLevelReferenceShift
  ;;; and update landRatio and display

  set elev_seaLevelReferenceShift par_elev_seaLevelReferenceShift

  ask patches
  [
    ;;; recentre elevation so negative values are only below sea level
    set elevation get-recentred-elevation
  ]

  reset-sea-water

  set-terrain-output-stats

  refresh-to-display-mode

end

to refresh-to-display-mode

  ;;; several soil properties must be rescaled to enhance visualisation
  ;;; (the parametric max and min values of some of these are never realised for various reasons)

  set-current-plot "Legend"

  clear-plot

  if (display-mode = "elevation and surface water depth (m)")
  [
    ifelse (any? patches with [p_ecol_coverType = "free water"])
    [
      let minWater min [p_water] of patches with [p_ecol_coverType = "free water"]
      let maxWater max [p_water] of patches with [p_ecol_coverType = "free water"]

      let rangeWater maxWater - minWater
      if (rangeWater = 0) [ set rangeWater 1 ]

      ask patches
      [
        ; paint elevation
        set pcolor get-elevation-color elevation

        ; paint water depth
        if (p_ecol_coverType = "free water")
        [ set pcolor 98 - 4 * (p_water - minWater) / rangeWater ]
      ]

      set-legend-elevation-and-water (maxWater / 1000) (minWater / 1000) 94 98 6 true ; in metres
    ]
    [
      ;;; if there  is no "free water" patch, behave like display-mode = "elevation (m)"
      ask patches
      [
        set pcolor get-elevation-color elevation
      ]
      set-legend-elevation 12
    ]
  ]
  if (display-mode = "elevation (m)")
  [
    ask patches
    [
      set pcolor get-elevation-color elevation
    ]
    set-legend-elevation 12
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
  if (display-mode = "surface water width (%)")
  [
    let minWater min [get-%water-surface p_water] of patches with [p_water > 0]
    let maxWater max [get-%water-surface p_water] of patches with [p_water > 0]

    let rangeWater maxWater - minWater
    if (rangeWater = 0) [ set rangeWater 1 ]

    ask patches
    [
      ifelse (p_water > 0)
      [ set pcolor 94 - 4 * ((get-%water-surface p_water) - minWater) / rangeWater ]
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
  if (display-mode = "ecological community composition")
  [
    ask patches
    [
      ;;; red: grass, green: brush, blue: wood, black: bare soil and water
      set pcolor get-ecologicalCommunityComposition-color p_ecol_%grass p_ecol_%brush p_ecol_%wood
    ]
    set-legend-ecologicalCommunityComposition
  ]
  if (display-mode = "total ecological community biomass (Kg)")
  [
    let minBiomass 0.001 * patchArea * min [p_ecol_biomass] of patches
    let maxBiomass 0.001 * patchArea * max [p_ecol_biomass] of patches

    ask patches
    [
      ifelse (p_ecol_biomass > 0)
      [ set pcolor 52 + 6 * (1 - ((0.001 * patchArea * p_ecol_biomass) - minBiomass) / (maxBiomass + 1E-6 - minBiomass)) ]
      [ set pcolor 59 ]
    ]
    set-legend-continuous-range maxBiomass minBiomass 52 59 7 true
  ]
  if (display-mode = "cover type")
  [
    ask patches
    [
      set pcolor get-coverType-color p_ecol_coverType
    ]
    set-legend-coverType
  ]
  if (display-mode = "albedo (%)")
  [
    let minAlbedo min [p_ecol_albedo] of patches
    let maxAlbedo max [p_ecol_albedo] of patches

    let rangeAlbedo maxAlbedo - minAlbedo
    if (rangeAlbedo = 0) [ set rangeAlbedo 1 ]

    ask patches
    [
      set pcolor 2 + 6 * (p_ecol_albedo - minAlbedo) / rangeAlbedo
    ]
    set-legend-continuous-range maxAlbedo minAlbedo 2 8 6 false
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
  if (display-mode = "crop-to-display frequency (%)")
  [
    ask patches
    [
      set pcolor 2 + 6 * (item (position crop-to-display crop_typesOfCrops) p_crop_frequency) / 100
    ]
    set-legend-continuous-range 100 0 8 2 6 true
  ]
  if (display-mode = "total crop biomass (Kg/patch)")
  [
    let minBiomass 0.001 * min [sum p_crop_totalBiomass] of patches
    let maxBiomass 0.001 * max [sum p_crop_totalBiomass] of patches

    ask patches
    [
      ifelse (sum p_crop_totalBiomass > 0)
      [ set pcolor 52 + 6 * (1 - ((0.001 * sum p_crop_totalBiomass) - minBiomass) / (maxBiomass + 1E-6 - minBiomass)) ]
      [ set pcolor 59 ]
    ]
    set-legend-continuous-range maxBiomass minBiomass 59 52 7 true
  ]
  if (display-mode = "total crop yield (Kg/patch)")
  [
    let minMeanYield 0
    carefully [ set minMeanYield 0.001 * min [sum p_crop_totalYield] of patches ] [ set minMeanYield 0 ]
    let maxMeanYield 0
    carefully [ set maxMeanYield 0.001 * max [sum p_crop_totalYield] of patches ] [ set maxMeanYield 1E-6 ]

    ask patches
    [
      carefully
      [ set pcolor 42 + 6 * (1 - (0.001 * sum p_crop_totalYield - minMeanYield) / (maxMeanYield + 1E-6 - minMeanYield)) ]
      [ set pcolor 49 ]
    ]
    set-legend-continuous-range maxMeanYield minMeanYield 49 42 7 true
  ]

  ;;; other modes of display can be added here

  display-flows

end

to-report get-elevation-color [ elevationValue ]

  let elevationGradient 0

;  ifelse (elevationValue < 0)
;  [
;    let normSubElevation (-1) * (elevationValue)
;    let normSubMinElevation (-1) * (minElevation) + 1E-6
;    set elevationGradient 20 + (200 * (1 - normSubElevation / normSubMinElevation))
;    report rgb 0 0 elevationGradient
;  ]
;  [
    ;;; this fragment was adapted to also represent land that is far above sea level
    let normSupElevation elevationValue - minElevation
    let normSupMaxElevation maxElevation - minElevation + 1E-6
    set elevationGradient 100 + (155 * (normSupElevation / normSupMaxElevation))
    report rgb (elevationGradient - 100) elevationGradient 0
;  ]

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

to-report get-crop-color [ cropName ]

  ; for a maximum of 13 crops
  report (16 + 9 * 10 * (position cropName crop_typesOfCrops)) mod 140

end

to-report get-coverType-color [ coverTypeName ]

  ;;; blue: free water, orange: desert, brown: grassland, yellow: wood-grass, green: shrubland, green: woodland
  let col pink ; color to mark patches with any bugs

  if (coverTypeName = "free water") [ set col 104 ]
  if (coverTypeName = "desert") [ set col 26 ]
  if (coverTypeName = "cropland") [ set col 67 ]
  if (coverTypeName = "grassland") [ set col 36 ]
  if (coverTypeName = "wood-grass") [ set col 44 ]
  if (coverTypeName = "shrubland") [ set col 53 ]
  if (coverTypeName = "woodland") [ set col 74 ]

  report col

end

to set-legend-elevation-and-water [ maximum minimum maxShade minShade numberOfKeys ascendingOrder? ]

  ; numberOfKeys is doubled (applies to elevation and then water)
  ; all other arguments refer to water

  set-legend-elevation numberOfKeys

  set-legend-continuous-range maximum minimum maxShade minShade numberOfKeys ascendingOrder?

end

to set-legend-elevation [ numberOfKeys ]

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

  set maximum precision maximum 4
  set minimum precision minimum 4

  let rangeValues maximum - minimum

  let step precision (rangeValues / numberOfKeys) 4

  if (maximum = minimum or step = 0) [ set maximum maximum + 1 set step 2 set rangeValues 1 ] ; this makes that at least one legend key is drawn when maximum = minimum or step is 0

  ifelse (ascendingOrder?)
  [
    let value minimum

    while [ value < maximum ]
    [
      create-temporary-plot-pen (word "" (precision value 4) "")
      set-plot-pen-color minShade + (maxShade - minShade) * (value - minimum) / rangeValues
      set value value + step
    ]
  ]
  [
    let value maximum

    while [ value > minimum ]
    [
      create-temporary-plot-pen (word "" (precision value 4) "")
      set-plot-pen-color maxShade - (maxShade - minShade) * (value - minimum) / rangeValues
      set value value - step
    ]
  ]

end

to set-legend-soil-textureType

  foreach soil_textureTypes_display
  [
    textureTypeName ->
    create-temporary-plot-pen textureTypeName
    set-plot-pen-color get-textureType-color textureTypeName
  ]

end

to set-legend-soil-texture [ %sandRange %siltRange %clayRange ]

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

  ;;; red: grass, green: brush, blue: wood
  create-temporary-plot-pen "100% grass"
  set-plot-pen-color red
  create-temporary-plot-pen "100% brush"
  set-plot-pen-color green
  create-temporary-plot-pen "100% wood"
  set-plot-pen-color blue
  create-temporary-plot-pen "100% bare soil and water"
  set-plot-pen-color black

end

to set-legend-coverType

  foreach (list "free water" "desert" "cropland" "grassland" "wood-grass" "shrubland" "woodland")
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

to print-crop-table

  output-print (word " | typesOfCrops | T_sum | HI | I_50A | I_50B | T_base | T_opt | RUE | I_50maxH | I_50maxW | T_heat | T_ext | S_water | sugSowingDay | sugHarvestingDay | rootZoneDepth |")

  foreach n-values (length crop_typesOfCrops) [j -> j]
  [
    cropIndex ->
    output-print (word
      " | " (item cropIndex crop_typesOfCrops)
      " | " (item cropIndex crop_T_sum)
      " | " (item cropIndex crop_HI)
      " | " (item cropIndex crop_I_50A)
      " | " (item cropIndex crop_I_50B)
      " | " (item cropIndex crop_T_base)
      " | " (item cropIndex crop_T_opt)
      " | " (item cropIndex crop_RUE)
      " | " (item cropIndex crop_I_50maxH)
      " | " (item cropIndex crop_I_50maxW)
      " | " (item cropIndex crop_T_heat)
      " | " (item cropIndex crop_T_extreme)
      " | " (item cropIndex crop_S_water)
      " | " (item cropIndex crop_sugSowingDay)
      " | " (item cropIndex crop_sugHarvestingDay)
      " | " (item cropIndex crop_rootZoneDepth)
      " | ")
  ]

end

to setup-plot-crop

  set-current-plot "Crops biomass (patch mean)"

  foreach crop_typesOfCrops
  [
    cropName ->
    create-temporary-plot-pen cropName
    set-plot-pen-color get-crop-color cropName
  ]

  set-current-plot "Crops yield (patch mean) and annual total precipitation"

  create-temporary-plot-pen "annual RAIN"
  set-plot-pen-mode 1
  set-plot-pen-color 0

  foreach crop_typesOfCrops
  [
    cropName ->
    create-temporary-plot-pen cropName
    set-plot-pen-mode 0
    set-plot-pen-color get-crop-color cropName
  ]

end

to update-plot-crop

  set-current-plot "Crops biomass (patch mean)"

  foreach crop_typesOfCrops
  [
    cropName ->
    set-current-plot-pen cropName
    plot mean [item (position cropName crop_typesOfCrops) p_crop_biomass] of patches
  ]

  set-current-plot "Crops yield (patch mean) and annual total precipitation"

  if (currentDayOfYear = 2) ; skips plotting on setup
  [
    set-current-plot-pen "annual RAIN"
    plot sum precipitation_yearSeries
  ]

  foreach crop_typesOfCrops
  [
    cropName ->
    set-current-plot-pen cropName
    if (is-ripe position cropName crop_typesOfCrops)
    [
      plotxy (currentyear + (currentDayOfYear / yearLengthInDays)) (mean [item (position cropName crop_typesOfCrops) p_crop_yield] of patches)
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE HANDLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EXPORT YIELD PERFORMANCES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to experiment-3

  let numberOfBatches 5

  set experiment-name "exp3"

  set type-of-experiment "user-defined"

  parameters-to-default

  set experiment-numberOfRuns 2

  set experiment-initRandomSeed 0

  set end-simulation-in-year 5

  repeat numberOfBatches
  [
    run-yield-performance-experiment-batch

    set experiment-initRandomSeed randomSeed
  ]

end

to experiment-4

  let numberOfBatches 5

  set experiment-name "exp4"

  set type-of-experiment "river-variation"

  parameters-to-default

  set experiment-numberOfRuns 2

  set experiment-initRandomSeed 0

  set end-simulation-in-year 5

  repeat numberOfBatches
  [
    run-yield-performance-experiment-batch

    set experiment-initRandomSeed randomSeed
  ]

end

to experiment-5

  let numberOfBatches 5

  set experiment-name "exp5"

  set type-of-experiment "randomised-crop-frequencies"

  parameters-to-default

  set experiment-numberOfRuns 2

  set experiment-initRandomSeed 0

  set end-simulation-in-year 5

  repeat numberOfBatches
  [
    run-yield-performance-experiment-batch

    set experiment-initRandomSeed randomSeed
  ]

end

to run-yield-performance-experiment-batch

  ;;; Every yield experiment batch can create up to five files:
  ;;; - crop table (if not already created),
  ;;; - terrain data (if not already created),
  ;;; - parameters per run (if already created, parameters are added as new rows), and
  ;;; - variables per crop/patch/year/run (one file created per batch)
  ;;; - weather variables per day (one file created per batch)

  ;;; a first setup is required to be able to export cropTable and terrain data
  setup

  ;;; the same cropTable applies to all runs in a batch
  ;;; so cropTable data are exported in a separate file
  ;;; NOTE: data can be retraced using crops names, initial random seed, and terrain random seed
  export-cropTable-of-yield-experiment

  ;;; a batch runs multiple random seeds on the same terrain
  ;;; so terrain data are exported as a separate table to minimise file size
  ;;; NOTE: data can be retraced using the terrainRandomSeed (assuming that the terrain parameters not controled by random seed are
  ;;; not varying; i.e. which could produce multiple terrains using the same terrain seed)
  export-terrain-of-yield-experiment

  setup-yield-performance-data-file

  setup-weather-data-file

  set randomSeed experiment-initRandomSeed

  repeat experiment-numberOfRuns
  [
    setup

    ;;; a run has a single configuration of parameters
    ;;; so parameters are exported as a separate table to minimise file size
    ;;; NOTE: data can be retraced using the randomSeed
    export-parameters-of-yield-experiment

    export-weather-of-yield-experiment

    repeat end-simulation-in-year
    [
      repeat yearLengthInDays
      [
        go

        export-weather-of-yield-experiment

        if (currentDayOfYear = 365) [ export-yield-performance ]
      ]
    ]

    set randomSeed randomSeed + 1
  ]

end

to setup-yield-performance-data-file

  ;;; parameters file
  ;;; build a unique file name according to the user setting
  let filePath (word "output//I2_yield-exp_pars_terrainRandomSeed=" terrainRandomSeed "_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name ".csv")

  ;;; do not repeat the setup if file already exists
  if (not file-exists? filePath)
  [
    file-open filePath

    file-print (word
      "terrainRandomSeed,randomSeed,"
      "temperature_annualMaxAt2m,temperature_annualMinAt2m,temperature_meanDailyFluctuation,temperature_dailyLowerDeviation,temperature_dailyUpperDeviation,"
      "solar_annualMax,solar_annualMin,solar_meanDailyFluctuation,"
      "precipitation_yearlyMean,precipitation_yearlySd,precipitation_dailyCum_nSamples,precipitation_dailyCum_maxSampleSize,"
      "precipitation_dailyCum_plateauValue_yearlyMean,precipitation_dailyCum_plateauValue_yearlySd,"
      "precipitation_dailyCum_inflection1_yearlyMean,precipitation_dailyCum_inflection1_yearlySd,precipitation_dailyCum_rate1_yearlyMean,precipitation_dailyCum_rate1_yearlySd,"
      "precipitation_dailyCum_inflection2_yearlyMean,precipitation_dailyCum_inflection2_yearlySd,precipitation_dailyCum_rate2_yearlyMean,precipitation_dailyCum_rate2_yearlySd,"
      "elev_seaLevelReferenceShift,riverWaterPerFlowAccumulation,errorToleranceThreshold,"
      "crop_selection,crop_intensity"
    )

    file-close
  ]

  ;;; yield and other variables file
  ;;; build a unique file name according to the user setting
  set filePath (word "output//I2_yield-exp_terrainRandomSeed=" terrainRandomSeed "_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  ;;; check that filePath does not exceed 100 (not common in this context)
  ;if (length filePath > 120) [ print "WARNING: file path may be too long, depending on your current directory. Decrease length of file name or increase the limit." set filePath substring filePath 0 120 ]
;print filePath

  ;;; do not repeat the setup if file already exists
  if (not file-exists? filePath)
  [
    file-open filePath

    file-print (word
      "terrainRandomSeed,randomSeed,"
      "currentYear,currentDayOfYear,"
      "precipitation_yearTotal,p_soil_meanARID,"
      "x,y,"
      "p_water,p_ecol_rootZoneDepth,p_soil_runOffCurveNumber,p_ecol_albedo,p_ecol_biomass,"
      "p_ecol_%water,p_ecol_%crop,p_ecol_%wood,p_ecol_%brush,p_ecol_%grass,"
      "crop,p_crop_frequency,p_soil_meanARID_grow,p_crop_yield,p_crop_totalYield"
    )

    file-close
  ]

end

to export-yield-performance

  ;;; recover the unique file name according to the user setting
  let filePath (word "output//I2_yield-exp_terrainRandomSeed=" terrainRandomSeed "_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  file-open filePath

  foreach sort patches
  [
    aPatch ->
    ask aPatch
    [
      foreach crop_selection
      [
        aCrop ->

        let cropIndex position aCrop crop_typesOfCrops

        ;;; terrainRandomSeed,randomSeed,
        file-type terrainRandomSeed file-type ","
        file-type randomSeed file-type ","
        ;;; currentYear, currentDayOfYear,
        file-type currentYear file-type ","
        file-type currentDayOfYear file-type ","
        ;;; year total of precipitation
        file-type (sum precipitation_yearSeries) file-type ","
        ;;; mean ARID in current year
        file-type (mean p_soil_ARID_yearSeries) file-type ","
        ;;; x, y,
        file-type pxcor file-type ","
        file-type pycor file-type ","
        ;;; p_water, p_ecol_rootZoneDepth, p_soil_runOffCurveNumber, p_ecol_albedo, p_ecol_biomass,
        file-type p_water file-type ","
        file-type p_ecol_rootZoneDepth file-type ","
        file-type p_soil_runOffCurveNumber file-type ","
        file-type p_ecol_albedo file-type ","
        file-type p_ecol_biomass file-type ","
        ;;; p_ecol_%water, p_ecol_%crop, p_ecol_%wood, p_ecol_%brush, p_ecol_%grass,
        file-type p_ecol_%water file-type ","
        file-type p_ecol_%crop file-type ","
        file-type p_ecol_%wood file-type ","
        file-type p_ecol_%brush file-type ","
        file-type p_ecol_%grass file-type ","
        ;;; crop, p_crop_frequency
        file-type aCrop file-type ","
        file-type (item cropIndex p_crop_frequency) file-type ","
        ;;; mean ARID during grow season in year
        ifelse ((item cropIndex crop_sowingDay) < (item cropIndex crop_harvestingDay))
        [
          ; growing season fits the current calendar year
          file-type (mean sublist p_soil_ARID_yearSeries (item cropIndex crop_sowingDay) (item cropIndex crop_harvestingDay)) file-type "," ]
        [
          ; growing season spans also into last year
          ifelse (currentYear = 0)
          [
            ; there is no last year
            file-type "," ; these NA will be signaling the rows that should be ignored in analysis
          ]
          [
            file-type (mean sentence (sublist p_soil_ARID_yearSeries 1 (item cropIndex crop_harvestingDay)) (sublist p_soil_ARID_yearSeries_lastYear (item cropIndex crop_sowingDay) yearLengthInDays)) file-type ","
          ]
        ]
        ;;; yield
        file-type (item cropIndex p_crop_yield) file-type ","
        file-type (item cropIndex p_crop_totalYield)
        file-print ""
      ]
    ]
  ]

  file-close

end

to export-parameters-of-yield-experiment

  let filePath (word "output//I2_yield-exp_pars_terrainRandomSeed=" terrainRandomSeed "_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name ".csv")

  file-open filePath

  ;;; terrainRandomSeed,randomSeed,
  file-type terrainRandomSeed file-type ","
  file-type randomSeed file-type ","
  ;;; temperature parameters
  file-type temperature_annualMaxAt2m file-type ","
  file-type temperature_annualMinAt2m file-type ","
  file-type temperature_meanDailyFluctuation file-type ","
  file-type temperature_dailyLowerDeviation file-type ","
  file-type temperature_dailyUpperDeviation file-type ","
  ;;; solar radiation parameters
  file-type solar_annualMax file-type ","
  file-type solar_annualMin file-type ","
  file-type solar_meanDailyFluctuation file-type ","
  ;;; precipitation parameters
  file-type precipitation_yearlyMean file-type ","
  file-type precipitation_yearlySd file-type ","
  file-type precipitation_dailyCum_nSamples file-type ","
  file-type precipitation_dailyCum_maxSampleSize file-type ","
  file-type precipitation_dailyCum_plateauValue_yearlyMean file-type ","
  file-type precipitation_dailyCum_plateauValue_yearlySd file-type ","
  file-type precipitation_dailyCum_inflection1_yearlyMean file-type ","
  file-type precipitation_dailyCum_inflection1_yearlySd file-type ","
  file-type precipitation_dailyCum_rate1_yearlyMean file-type ","
  file-type precipitation_dailyCum_rate1_yearlySd file-type ","
  file-type precipitation_dailyCum_inflection2_yearlyMean file-type ","
  file-type precipitation_dailyCum_inflection2_yearlySd file-type ","
  file-type precipitation_dailyCum_rate2_yearlyMean file-type ","
  file-type precipitation_dailyCum_rate2_yearlySd file-type ","
  ;;; terrain and hydrology parameters
  file-type elev_seaLevelReferenceShift file-type ","
  file-type riverWaterPerFlowAccumulation file-type ","
  file-type errorToleranceThreshold file-type ","
  ;;; cropping parameters
  file-type (map [i -> (word "'" i "'")] crop_selection) file-type ","
  file-type crop_intensity
  file-print ""

  file-close

end

to export-cropTable-of-yield-experiment

  ;;; build a unique file name according to the user setting
  let filePath (word "output//I2_yield-exp_cropTable.csv")

  ;;; do not repeat the export if file already exists
  if (not file-exists? filePath)
  [
    file-open filePath

    file-print (word
      "terrainRandomSeed,initRandomSeed,"
      "crop,T_sum,HI,I_50A,I_50B,T_base,T_opt,RUE,I_50maxH,I_50maxW,T_heat,T_extreme,S_water,sowingDay,harvestDay,rootZoneDepth"
    )

    foreach crop_selection
    [
      aCrop ->

      let cropIndex position aCrop crop_typesOfCrops

      ;;; terrainRandomSeed,randomSeed,
      file-type terrainRandomSeed file-type ","
      file-type randomSeed file-type "," ;;; this will be equivalent to experiment-initRandomSeed
      ;;; crop, sowingDay, harvestDay
      file-type (word aCrop) file-type ","
      file-type (item cropIndex crop_T_sum) file-type ","
      file-type (item cropIndex crop_HI) file-type ","
      file-type (item cropIndex crop_I_50A) file-type ","
      file-type (item cropIndex crop_I_50B) file-type ","
      file-type (item cropIndex crop_T_base) file-type ","
      file-type (item cropIndex crop_T_opt) file-type ","
      file-type (item cropIndex crop_RUE) file-type ","
      file-type (item cropIndex crop_I_50maxH) file-type ","
      file-type (item cropIndex crop_I_50maxW) file-type ","
      file-type (item cropIndex crop_T_heat) file-type ","
      file-type (item cropIndex crop_T_extreme) file-type ","
      file-type (item cropIndex crop_S_water) file-type ","
      file-type (item cropIndex crop_sowingDay) file-type ","
      file-type (item cropIndex crop_harvestingDay) file-type ","
      file-type (item cropIndex crop_rootZoneDepth)
      file-print ""
    ]

    file-close
  ]

end

to export-terrain-of-yield-experiment

  ;;; this function extracts patch information to a csv file that can be easily accessed when analysing yield experiments in R

  ;;; build another unique file name to be created at output/yield/ directory, together with the yield experiment data
  let filePath (word "output//I2_yield-exp_terrainRandomSeed=" terrainRandomSeed ".csv")

  ;;; do not repeat the export if file already exists
  if (not file-exists? filePath)
  [
    file-open filePath

    ;;; header
    file-print (word
      "terrainRandomSeed,"
      "x,y,elevation,flow_direction,flow_receive,flow_accumulation,"
      "p_soil_formativeErosion,p_soil_depth,p_soil_%sand,p_soil_%silt,p_soil_%clay,p_soil_textureType,"
      "p_initEcol_%grass,p_initEcol_%brush,p_initEcol_%wood,p_ecol_coverType,"
      "p_soil_saturation,p_soil_fieldCapacity,p_soil_waterHoldingCapacity,p_soil_wiltingPoint,p_soil_deepDrainageCoefficient"
    )

    foreach sort patches
    [
      aPatch ->
      ask aPatch
      [
        file-type terrainRandomSeed file-type ","
        file-type pxcor file-type ","
        file-type pycor file-type ","
        file-type elevation file-type ","
        file-type flow_direction file-type ","
        file-type flow_receive file-type ","
        file-type flow_accumulation file-type ","
        file-type p_soil_formativeErosion file-type ","
        file-type p_soil_depth file-type ","
        file-type p_soil_%sand file-type ","
        file-type p_soil_%silt file-type ","
        file-type p_soil_%clay file-type ","
        file-type p_soil_textureType file-type ","
        file-type p_initEcol_%grass file-type ","
        file-type p_initEcol_%brush file-type ","
        file-type p_initEcol_%wood file-type ","
        file-type p_ecol_coverType file-type ","
        file-type p_soil_saturation file-type ","
        file-type p_soil_fieldCapacity file-type ","
        file-type p_soil_waterHoldingCapacity file-type ","
        file-type p_soil_wiltingPoint file-type ","
        file-type p_soil_deepDrainageCoefficient
      ]
      file-print ""
    ]

    file-close
  ]

end

to setup-weather-data-file

  ;;; build a unique file name according to the user setting
  let filePath (word "output//I2_yield-exp_weather_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  ;;; check that filePath does not exceed 100 (not common in this context)
  ;if (length filePath > 120) [ print "WARNING: file path may be too long, depending on your current directory. Decrease length of file name or increase the limit." set filePath substring filePath 0 120 ]
;print filePath
  file-open filePath

  file-print (word
    "randomSeed,"
    "currentYear,currentDayOfYear,"
    "temperature,maxTemperature,minTemperature,"
    "solarRadiation,"
    "precipitation"
  )

  file-close

end

to export-weather-of-yield-experiment

  ;;; recover the unique file name according to the user setting
  let filePath (word "output//I2_yield-exp_weather_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  file-open filePath

  ;;; randomSeed,
  file-type randomSeed file-type ","
  ;;; currentYear, currentDayOfYear,
  file-type currentYear file-type ","
  file-type currentDayOfYear file-type ","
  ;;; tempetature,temperature_max,temperature_min,
  file-type temperature file-type ","
  file-type maxTemperature file-type ","
  file-type minTemperature file-type ","
  ;;; solarRadiation,
  file-type solarRadiation file-type ","
  ;;; RAIN
  file-type precipitation

  file-print ""

  file-close

end

;;; IMPORT TERRAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to import-terrain

  clear-all

  ;;; load a terrain from the "terrains" folder
  ;;; corresponding to the random seed given as a parameter in the interface

  ;;; build a unique file name according to the user setting
  let filePath (word "terrains//terrain_random_w=" world-width "_h=" world-height "_a=" elev_algorithm-style "_fill-sinks=" flow_do-fill-sinks "_seed=" terrainRandomSeed)

  ;;; check that filePath does not exceed 100 (not common in this context)
  ;if (length filePath > 100) [ print "WARNING: file path may be too long, depending on your current directory. Decrease length of file name or increase the limit." set filePath substring filePath 0 100 ]

  set filePath (word filePath ".csv")

  ifelse (not file-exists? filePath)
  [ print (word "WARNING: could not find '" filePath "'") stop ] ;;; unfortunately the stop command doesn't stop the setup procedure
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

          if (item globalIndex globalNames = "ecol_brushfrequencyinflection") [ set ecol_brushFrequencyInflection item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_brushfrequencyrate") [ set ecol_brushFrequencyRate item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_grassfrequencyinflection") [ set ecol_grassFrequencyInflection item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_grassfrequencyrate") [ set ecol_grassFrequencyRate item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_woodfrequencyinflection") [ set ecol_woodFrequencyInflection item globalIndex globalValues ]
          if (item globalIndex globalNames = "ecol_woodfrequencyrate") [ set ecol_woodFrequencyRate item globalIndex globalValues ]
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

            set elevation_original item 5 thisLine
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
            set p_ecol_%grass item 16 thisLine
            set p_initEcol_%grass item 16 thisLine
            set p_ecol_%brush item 17 thisLine
            set p_initEcol_%brush item 17 thisLine
            set p_ecol_%wood item 18 thisLine
            set p_initEcol_%wood item 18 thisLine
            set p_ecol_coverType read-from-string item 19 thisLine
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

  set-terrain-output-stats

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

;;; IMPORT TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to load-hydrologic-soil-groups-table

  ;;; SOURCE: table in page A-1,
  ;;; Cronshey R G 1986 Urban Hydrology for Small Watersheds, Technical Release 55 (TR-55).
  ;;; United States Department of Agriculture, Soil Conservation Service, Engineering Division.

  ;;; this procedure loads the values of the hydrologic soil groups table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping the different types of data, if more than one
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let hydrologicSoilGroupTable csv:from-file "hydrologicSoilGroupTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes
  let textureTypesRowRange (list ((item 1 (item 2 hydrologicSoilGroupTable)) - 1) ((item 3 (item 2 hydrologicSoilGroupTable)) - 1))

  ;;; line 4 (= index 3), row indexes
  ;;; Types of soil according to % of sand, silt and clay (ternary diagram) established by USDA
  let textureTypeColumn (item 1 (item 3 hydrologicSoilGroupTable)) - 1

  ;;; USDA classification of soils according to water infiltration (A, B, C, and D; see reference in csv file)
  let HydrologycSoilGroupsColumn (item 3 (item 3 hydrologicSoilGroupTable)) - 1

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; read variables (list of lists, matrix: texture types x hydrologic soil groups)
  let hydrologicSoilGroupsData sublist hydrologicSoilGroupTable (item 0 textureTypesRowRange) (item 1 textureTypesRowRange + 1)

  ;;; extract type of texture
  set soil_textureTypes map [row -> item textureTypeColumn row ] hydrologicSoilGroupsData

  ;;; extract hydrologic soil group
  set soil_hydrologicSoilGroups map [row -> item HydrologycSoilGroupsColumn row ] hydrologicSoilGroupsData

end

to load-runoff-curve-number-table

  ;;; SOURCE: table 2.2,
  ;;; Cronshey R G 1986 Urban Hydrology for Small Watersheds, Technical Release 55 (TR-55).
  ;;; United States Department of Agriculture, Soil Conservation Service, Engineering Division.

  ;;; this procedure loads the values of the run off curve number table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping the different types of data, if more than one
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let runOffCurveNumberTable csv:from-file "runOffCurveNumberTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes
  let typesOfCoverRowRange (list ((item 1 (item 2 runOffCurveNumberTable)) - 1) ((item 3 (item 2 runOffCurveNumberTable)) - 1))

  ;;; line 4 (= index 3), row indexes
  ;;; types of soil cover
  let coverTypeColumn (item 1 (item 3 runOffCurveNumberTable)) - 1

  ;;; types of soil treatment (if applies)
  let TreatmentColumn (item 3 (item 3 runOffCurveNumberTable)) - 1

  ;;; types of soil hydrologic condition (if applies)
  let HydrologicConditionColumn (item 5 (item 3 runOffCurveNumberTable)) - 1

  ;;; Columns holding data for the four Hydrologic soil groups: value 8 and 10 (=item 7 and 9)
  let HydrologycSoilGroupsColumns (list ((item 7 (item 3 runOffCurveNumberTable)) - 1) ((item 9 (item 3 runOffCurveNumberTable)) - 1) )

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; read variables (list of lists, matrix: cover types-treatment-condition x hydrologic soil groups)
  let runOffCurveNumberData sublist runOffCurveNumberTable (item 0 typesOfCoverRowRange) (item 1 typesOfCoverRowRange + 1) ; select only those rows corresponding to data on types of cover

  ;;; extract cover, treatment and hydrologic condition
  let coverTreatmentAndHydrologicCondition (
    map [row -> (word (item coverTypeColumn row) " | " (item TreatmentColumn row) " | " (item HydrologicConditionColumn row) ) ] runOffCurveNumberData
    )

  ;;; extract curve number table
  set soil_runOffCurveNumberTable extract-subtable runOffCurveNumberData (item 0 HydrologycSoilGroupsColumns) (item 1 HydrologycSoilGroupsColumns)

  ;;; combine with cover-treatment-hydrologic condition
  set soil_runOffCurveNumberTable fput coverTreatmentAndHydrologicCondition soil_runOffCurveNumberTable

end

to load-soil-water-table

  ;;; SOURCE (TO-DO: FIND BETTER SOURCES!):
  ;;; 1. Plant & Soil Sciences eLibrary, Lesson: Soils - Part 2: Physical Properties
  ;;;    of Soil and Soil Water, page 10 (Soil Water), Table 2.6.
  ;;;    https://passel2.unl.edu/view/lesson/0cff7943f577/10
  ;;;    Conservation Service, Engineering Division
  ;;; 2. Rain Machine support documentation, "Zones", "Soil Types", Table.
  ;;;    https://support.rainmachine.com/hc/en-us/articles/228001248-Soil-Types
  ;;; 3. SWAT theoretical documentation 2009, p. 148, Table 2:3-1,
  ;;;    https://swat.tamu.edu/media/99192/swat2009-theory.pdf

  ;;; this procedure loads the values of the soil water table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping the different types of data, if more than one
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let soilWaterTable csv:from-file "soilWaterTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes
  let textureTypesRowRange (list ((item 1 (item 2 soilWaterTable)) - 1) ((item 3 (item 2 soilWaterTable)) - 1))

  ;;; line 4 (= index 3), row indexes
  ;;; Types of soil according to % of sand, silt and clay (ternary diagram) established by USDA
  let textureTypeColumn (item 1 (item 3 soilWaterTable)) - 1

  ;;; values of field capacity (fraction of soil volume) per texture type
  let fieldCapacityColumn (item 3 (item 3 soilWaterTable)) - 1

  ;;; values of saturation (fraction of soil volume) per texture type
  let saturationColumn (item 5 (item 3 soilWaterTable)) - 1

  ;;; values of intake rate (mm/hour) per texture type
  let intakeRateColumn (item 7 (item 3 soilWaterTable)) - 1

  ;;; values of minimum and maximum water holding capacity (in/ft) per texture type
  let minWaterHoldingCapacityColumn (item 9 (item 3 soilWaterTable)) - 1
  let maxWaterHoldingCapacityColumn (item 11 (item 3 soilWaterTable)) - 1

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; read variables (list of lists, matrix: texture types x soil water variables)
  let soilWaterData sublist soilWaterTable (item 0 textureTypesRowRange) (item 1 textureTypesRowRange + 1)

  ;;; types of texture must be exactly the same that is extracted from the Hydrologic Soil Group table

  ;;; extract field capacity
  set soil_fieldCapacity map [row -> item fieldCapacityColumn row ] soilWaterData

  ;;; extract saturation
  set soil_saturation map [row -> item saturationColumn row ] soilWaterData

  ;;; extract intake rate
  set soil_intakeRate map [row -> item intakeRateColumn row ] soilWaterData

  ;;; extract water holding capacity
  set soil_minWaterHoldingCapacity map [row -> item minWaterHoldingCapacityColumn row ] soilWaterData
  set soil_maxWaterHoldingCapacity map [row -> item maxWaterHoldingCapacityColumn row ] soilWaterData

end

to load-albedo-table

  ;;; SOURCE:
  ;;; Values are informed on (not derived from) the following sources:

  ;;; Houldcroft C J, Grey W M F, Barnsley M, Taylor C M, Los S O and North P R J (2009).
  ;;; New vegetation Albedo parameters and global fields of soil background Albedo derived from MODIS for use in a climate model,
  ;;; J. Hydrometeorol., 10: 183–98.
  ;;; https://doi.org/10.1175/2008JHM1021.1

  ;;; Table 2 in:
  ;;; Gao F, Schaaf C B, Strahler A H, Roesch A, Lucht W and Dickinson R (2005).
  ;;; MODIS bidirectional reflectance distribution function and albedo Climate Modeling
  ;;; Grid products and the variability of albedo major global vegetation types,
  ;;; Journal of Geophysical Research D: Atmospheres, 110(1): 1–13.
  ;;; https://doi.org/10.1029/2004JD005190

  ;;; also https://en.wikipedia.org/wiki/Albedo

  ;;; this procedure loads the values of the albedo table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping the different types of data, if more than one
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let albedoTable csv:from-file "albedoTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes
  let typesOfCoverRowRange (list ((item 1 (item 2 albedoTable)) - 1) ((item 3 (item 2 albedoTable)) - 1))

  ;;; line 4 (= index 3), row indexes
  ;;; broadband range
  ;;; types of soil cover
  let coverTypeColumn (item 1 (item 3 albedoTable)) - 1

  ;;; albedo (%)
  let albedoColumn (item 3 (item 3 albedoTable)) - 1

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; read variables (list of lists, matrix: cover types x albedo)
  let albedoData sublist albedoTable (item 0 typesOfCoverRowRange) (item 1 typesOfCoverRowRange + 1) ; select only those rows corresponding to data on types of cover

  ;;; extract cover types
  let coverType map [row -> item coverTypeColumn row ] albedoData

  ;;; extract albedo (%)
  let albedo map [row -> item albedoColumn row ] albedoData

  ;;; combine with cover types with albedo values
  set ecol_albedoTable (list coverType albedo)

end

to load-ecological-component-table

  ;;; SOURCE:
  ;;; Values are informed on (not derived from) the following sources:

  ;;; "source: on root depth:
  ;;; Foxx T S, Tierney G D and Williams J M (1984).
  ;;; Rooting depths of plants relative to biological and environmental factors 26
  ;;; Online: http://permalink.lanl.gov/object/tr?what=info:lanl-repo/lareport/LA-10254-MS 00318770.pdf

  ;;; on biomass:
  ;;; Wu Z, Dye D, Vogel J and Middleton B (2016).
  ;;; Estimating forest and woodland aboveground biomass using active and passive remote sensing Photogramm.
  ;;; Eng. Remote Sensing 82 271–81.
  ;;; Online: https://doi.org/10.14358/PERS.82.4.271

  ;;; Li A, Dhakal S, Glenn N, Spaete L, Shinneman D, Pilliod D, Arkle R and McIlroy S (2017).
  ;;; Lidar Aboveground Vegetation Biomass Estimates in Shrublands: Prediction, Uncertainties and Application to Coarser Scales
  ;;; Remote Sens. 9 903
  ;;; Online: http://www.mdpi.com/2072-4292/9/9/903

  ;;; Navarro Cerrillo R M and Blanco Oyonarte P (2006).
  ;;; Estimation of above-ground biomass in shrubland ecosystems of southern Spain
  ;;; Investig. Agrar. Sist. y Recur. For. 15 197.
  ;;; Online: https://www.researchgate.net/publication/28126747"

  ;;; this procedure loads the values of the ecological component table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping the different types of data, if more than one
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let ecologicalCommunityTable csv:from-file "ecologicalCommunityTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes
  let typesOfEcologicalComponentRowRange (list ((item 1 (item 2 ecologicalCommunityTable)) - 1) ((item 3 (item 2 ecologicalCommunityTable)) - 1))

  ;;; line 4 (= index 3), row indexes
  ;;; broadband range
  ;;; types of soil cover
  let ecologicalComponentTypeColumn (item 1 (item 3 ecologicalCommunityTable)) - 1

  ;;; maximum root depth (mm)
  let maxRootDepthColumn (item 3 (item 3 ecologicalCommunityTable)) - 1

  ;;; biomass (g/m^2)
  let biomassColumn (item 5 (item 3 ecologicalCommunityTable)) - 1

  ;;; recovery lag (days)
  let recoveryLagColumn (item 7 (item 3 ecologicalCommunityTable)) - 1

  ;;; water stress sensitivity (%maxAffected/%total*day)
  let waterStressSensitivityColumn (item 9 (item 3 ecologicalCommunityTable)) - 1

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; read variables (list of lists, matrix: ecological component types x max root depth-biomass-recovery lag-water stress sensitivity)
  let ecologicalComponentData sublist ecologicalCommunityTable (item 0 typesOfEcologicalComponentRowRange) (item 1 typesOfEcologicalComponentRowRange + 1) ; select only those rows corresponding to data on types of cover

  ;;; extract ecological component names
  set ecol_ecologicalComponents map [row -> item ecologicalComponentTypeColumn row ] ecologicalComponentData

  ;;; extract maximum root depth
  set ecol_maxRootDepth map [row -> item maxRootDepthColumn row ] ecologicalComponentData

  ;;; extract biomass
  set ecol_biomass map [row -> item biomassColumn row ] ecologicalComponentData

  ;;; extract recovery lag
  set ecol_recoveryLag map [row -> item recoveryLagColumn row ] ecologicalComponentData

  ;;; extract water stress sensitivity
  set ecol_waterStressSensitivity map [row -> item waterStressSensitivityColumn row ] ecologicalComponentData

end

to load-crops-table

  ;;; this procedure loads the values of the crops table
  ;;; the table contains:
  ;;;   1. two lines of headers with comments (metadata, to be ignored)
  ;;;   2. two lines with statements mapping the different types of data, if more than one
  ;;;   3. the header of the table with the names of variables
  ;;;   4. remaining rows containing row name and values

  let cropsTable csv:from-file "cropsTable.csv"

  ;;;==================================================================================================================
  ;;; mapping coordinates (row or columns) in lines 3 and 4 (= index 2 and 3) -----------------------------------------
  ;;; NOTE: always correct raw mapping coordinates (start at 1) into list indexes (start at 0)

  ;;; line 3 (= index 2), row indexes

  ;;; Types of crops rows: value 2 and 4 (= index 1 and 3)
  let typesOfCropsRowRange (list ((item 1 (item 2 cropsTable)) - 1) ((item 3 (item 2 cropsTable)) - 1))

  ;;; line 4 (= index 3), column indexes

  let T_sumColumn (item 7 (item 3 cropsTable)) - 1

  let HIColumn (item 9 (item 3 cropsTable)) - 1

  let I_50AColumn (item 11 (item 3 cropsTable)) - 1

  let I_50BColumn (item 13 (item 3 cropsTable)) - 1

  let T_baseColumn (item 15 (item 3 cropsTable)) - 1

  let T_optColumn (item 17 (item 3 cropsTable)) - 1

  let RUEColumn (item 19 (item 3 cropsTable)) - 1

  let I_50maxHColumn (item 21 (item 3 cropsTable)) - 1

  let I_50maxWColumn (item 23 (item 3 cropsTable)) - 1

  let T_heatColumn (item 25 (item 3 cropsTable)) - 1

  let T_extColumn (item 27 (item 3 cropsTable)) - 1

  ;let S_CO2Column (item 29 (item 3 cropsTable)) - 1

  let S_waterColumn (item 31 (item 3 cropsTable)) - 1

  let sugSowingDayColumn (item 33 (item 3 cropsTable)) - 1

  let sugHarvestingDayColumn (item 35 (item 3 cropsTable)) - 1

  let rootZoneDepthColumn (item 37 (item 3 cropsTable)) - 1

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; read variables per crop type (list of lists, matrix: crop types x variables)
  let cropsData sublist cropsTable (item 0 typesOfCropsRowRange) (item 1 typesOfCropsRowRange + 1) ; select only those row corresponding to types of crops, if there is anything else

  ;;; extract types of crops from the first column
  set crop_typesOfCrops map [row -> item 0 row ] cropsData

  ;;; extract parameter values from the given column
  set crop_T_sum map [row -> item T_sumColumn row ] cropsData

  set crop_HI map [row -> item HIColumn row ] cropsData

  set crop_I_50A map [row -> item I_50AColumn row ] cropsData

  set crop_I_50B map [row -> item I_50BColumn row ] cropsData

  set crop_T_base map [row -> item T_baseColumn row ] cropsData

  set crop_T_opt map [row -> item T_optColumn row ] cropsData

  set crop_RUE map [row -> item RUEColumn row ] cropsData

  set crop_I_50maxH map [row -> item I_50maxHColumn row ] cropsData

  set crop_I_50maxW map [row -> item I_50maxWColumn row ] cropsData

  set crop_T_heat map [row -> item T_heatColumn row ] cropsData

  set crop_T_extreme map [row -> item T_extColumn row ] cropsData

  set crop_S_water map [row -> item S_waterColumn row ] cropsData

  set crop_sugSowingDay map [row -> item sugSowingDayColumn row ] cropsData

  set crop_sugHarvestingDay map [row -> item sugHarvestingDayColumn row ] cropsData

  set crop_rootZoneDepth map [row -> item rootZoneDepthColumn row ] cropsData

end

to-report extract-subtable [ table startColumnIndex endColumnIndex ]

  let subtable (list)
  let columnsCount ((endColumnIndex + 1) - startColumnIndex)
  foreach n-values columnsCount [ j -> j ]
  [
    i ->
    let columnIndex startColumnIndex + i
    set subtable lput (map [row -> item columnIndex row ] table) subtable
  ]
  report subtable

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; movie generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to generate-animation

  setup
  vid:start-recorder
  repeat (end-simulation-in-year * yearLengthInDays) [ go vid:record-view ]
  vid:save-recording  (word "run_" behaviorspace-run-number ".mov")
  vid:reset-recorder

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; numeric generic functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-annual-sinusoid-with-fluctuation [ minValue maxValue meanFluctuation dayOfYear southHemisphere ]

  report random-normal (get-annual-sinusoid minValue maxValue dayOfYear southHemisphere) meanFluctuation

end

to-report get-annual-sinusoid [ minValue maxValue dayOfYear southHemisphere ]

  let dayOfYearWithLowestValue get-dayOfYear-with-lowest-value southHemisphere

  let amplitude (maxValue - minValue) / 2

  report minValue + amplitude * (1 + sin ((360 * (dayOfYear - (dayOfYearWithLowestValue - yearLengthInDays)) / yearLengthInDays) - 90))

  ; NOTE: sin function in NetLogo needs angle in degrees. 360º equivalent to 2 * pi

end

to-report get-dayOfYear-with-lowest-value [ southHemisphere ]

let value -1

  ifelse (southHemisphere)
  [
    ;;; assuming southern hemisphere, winter solstice in 21st June (not leap year)
    set value (31 + 28 + 31 + 30 + 31 + 21)
  ]
  [
    ;;; assuming northern hemisphere, winter solstice in 21st December (not leap year)
    set value (31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 21)
  ]

  report value

end

to-report get-cumulative-curve [ plateauValue inflection1 rate1 inflection2 rate2 lengthOfCurve nSamples maxSampleSize ]

  ;;; get double logistic curve
  let cumulativeCurve (get-double-logistic-curve
    lengthOfCurve
    plateauValue inflection1 rate1 inflection2 rate2
  )

  ;;; modify the curve breaking the continuous pattern by randomly averaging neighborhoods of values
  set cumulativeCurve (escalonate-curve cumulativeCurve nSamples maxSampleSize)

  ;;; NOTE: in some cases, the curve at this point might be too horizontal and fail to reach 1.
  ;;; This means that reaching 1 at the end of the curve (i.e. cumulative curve) takes precedence over the shape parameters
  ;if ((last cumulativeCurve) < 1) [ print (word "Warning (precipitation): failed to generate a cumulative curve without re-scaling: " (last cumulativeCurve) " < 1" )]

  ;;; re-scale the curve so it fits within 0 and 1
  set cumulativeCurve rescale-curve cumulativeCurve

  report cumulativeCurve

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

to-report escalonate-curve [ curve nSamples maxSampleSize ]

  ;;; Break curve slope into several random steps, each consisting of an increase with maximum slope followed by a plateau

  foreach n-values nSamples [j -> j + 1] ; do not iterate for the first (0) element
  [
    sampleIndex ->

    ; get a decreasing sample size proportionally to sample index
    let thisSampleSize ceiling (maxSampleSize * sampleIndex / nSamples)

    ; get random day of year to have rain (we exclude 0, which is the extra day or the last day of previous year)
    let plateauMiddlePoint 1 + random (length curve)

    ; set sample limits
    let earliestNeighbour max (list 1 (plateauMiddlePoint - thisSampleSize))
    let latestNeighbour min (list (length curve) (plateauMiddlePoint + thisSampleSize))

    ; get mean of neighbourhood
    let meanNeighbourhood mean (sublist curve earliestNeighbour latestNeighbour)
    ;print (word "thisSampleSize = " thisSampleSize ", plateauMiddlePoint = " plateauMiddlePoint ", earliestNeighbour = " earliestNeighbour ", latestNeighbour = " latestNeighbour)
    ;print meanNeighbourhood

    ; assign mean to all days in neighbourhood
    foreach n-values (latestNeighbour - earliestNeighbour) [k -> earliestNeighbour + k]
    [
      i ->
      set curve replace-item i curve meanNeighbourhood
    ]
  ]

  report curve

end

to-report rescale-curve [ curve ]

  ;;; Rescale curve to the 0-1 interval

  ;;; cover special case where the curve is a horizontal line (first = last)
  ;;; solution: interpolate 0-1 with a line
  if ((last curve) = (item 0 curve)) [ set curve n-values (length curve) [ j -> j * 1 / (length curve) ] ]

  let newCurve curve

  foreach n-values (length curve) [j -> j]
  [
    i ->
    set newCurve replace-item i newCurve (((item i curve) - (item 0 curve)) / ((last curve) - (item 0 curve)))
  ]

  report newCurve

end

to-report get-incremets-from-curve [ curve ]

  ;;; Calculate the increments or derivatives corresponding to a given curve

  let incrementsCurve curve

  foreach n-values (length curve) [j -> j]
  [
    i ->
    if (i > 0) ; do not iterate for the first (0) element
    [
      set incrementsCurve replace-item i incrementsCurve (max (list 0 ((item i curve) - (item (i - 1) curve))))
    ]
  ]

  report incrementsCurve

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

to-report get-logistic-growth-delta [ X maxX r ]

  ;;; because maxX is dynamic, do preliminary check to avoid division by zero
  if (maxX = 0) [ report -1 * X ] ;;; this sets the maximum decrease rate

  report r * X * (1 - X / maxX)

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
1189
82
1594
568
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
48
325
154
385
terrainRandomSeed
0.0
1
0
Number

MONITOR
423
12
524
57
NIL
landRatio
4
1
11

SLIDER
218
333
496
366
par_elev_seaLevelReferenceShift
par_elev_seaLevelReferenceShift
-1000
round max (list maxElevation elev_rangeHeight)
-1000.0
1
1
m
HORIZONTAL

MONITOR
264
58
362
103
sdElevation
precision sdElevation 4
4
1
11

MONITOR
361
58
443
103
minElevation
precision minElevation 4
4
1
11

MONITOR
437
58
524
103
maxElevation
precision maxElevation 4
4
1
11

BUTTON
340
368
548
401
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
266
12
358
57
NIL
count patches
0
1
11

MONITOR
356
12
421
57
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
23
392
189
437
elev_algorithm-style
elev_algorithm-style
"NetLogo" "C#"
1

SWITCH
765
14
885
47
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
36
441
183
474
flow_do-fill-sinks
flow_do-fill-sinks
0
1
-1000

INPUTBOX
203
425
388
485
par_riverWaterPerFlowAccumulation
1.0E-4
1
0
Number

CHOOSER
24
125
234
170
type-of-experiment
type-of-experiment
"random" "user-defined" "precipitation-variation" "river-variation" "water-variation" "randomised-crop-frequencies"
3

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
93
298
567
317
==================TERRAIN==================
14
0.0
1

INPUTBOX
25
58
99
118
randomSeed
5.0
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
384
593
518
630
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
383
556
519
593
NIL
temperature_annualMaxAt2m
2
1
9

INPUTBOX
102
58
228
118
end-simulation-in-year
5.0
1
0
Number

SLIDER
14
629
385
662
temperature_mean-daily-fluctuation
temperature_mean-daily-fluctuation
0
5
2.2
0.1
1
ºC  (default: 2.2)
HORIZONTAL

SLIDER
18
668
385
701
temperature_daily-lower-deviation
temperature_daily-lower-deviation
0
10
6.8
0.1
1
ºC  (default: 6.8)
HORIZONTAL

SLIDER
18
707
385
740
temperature_daily-upper-deviation
temperature_daily-upper-deviation
0
10
7.9
0.1
1
ºC  (default: 7.9)
HORIZONTAL

SLIDER
12
558
383
591
temperature_annual-max-at-2m
temperature_annual-max-at-2m
15
40
37.0
0.1
1
ºC  (default: 37)
HORIZONTAL

SLIDER
13
594
384
627
temperature_annual-min-at-2m
temperature_annual-min-at-2m
-15
15
12.8
0.1
1
ºC  (default: 12.8)
HORIZONTAL

MONITOR
386
627
543
664
NIL
temperature_meanDailyFluctuation
2
1
9

MONITOR
386
667
538
704
NIL
temperature_dailyLowerDeviation
2
1
9

MONITOR
387
704
541
741
NIL
temperature_dailyUpperDeviation
2
1
9

PLOT
1566
513
2116
633
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
19
799
418
832
solar_annual-max
solar_annual-max
solar_annual-min
30
24.2
0.01
1
MJ/m2 (default: 24.2)
HORIZONTAL

SLIDER
19
764
416
797
solar_annual-min
solar_annual-min
1
solar_annual-max
9.2
0.01
1
MJ/m2 (default: 9.2)
HORIZONTAL

SLIDER
21
835
418
868
solar_mean-daily-fluctuation
solar_mean-daily-fluctuation
0
6
3.3
0.01
1
MJ/m2 (default: 3.3)
HORIZONTAL

PLOT
1566
634
2064
754
Solar radiation
days
MJ/m2
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot solarRadiation"

MONITOR
419
760
502
797
NIL
solar_annualMin
3
1
9

MONITOR
418
798
500
835
NIL
solar_annualMax
3
1
9

MONITOR
419
832
550
869
NIL
solar_meanDailyFluctuation
3
1
9

MONITOR
553
13
632
58
NIL
currentYear
0
1
11

MONITOR
645
13
759
58
NIL
currentDayOfYear
0
1
11

CHOOSER
1032
10
1334
55
display-mode
display-mode
"elevation and surface water depth (m)" "elevation (m)" "surface water depth (mm)" "surface water width (%)" "soil formative erosion" "soil depth (mm)" "soil texture" "soil texture types" "soil run off curve number" "soil water wilting point" "soil water holding capacity" "soil water field capacity" "soil water saturation" "soil deep drainage coefficient" "soil water content (ratio)" "ecological community composition" "total ecological community biomass (Kg)" "cover type" "albedo (%)" "reference evapotranspiration (ETr) (mm)" "runoff (mm)" "root zone depth (mm)" "ARID coefficient" "crop-to-display frequency (%)" "total crop biomass (Kg/patch)" "total crop yield (Kg/patch)"
1

BUTTON
893
14
1017
47
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
21
933
422
966
precipitation_yearly-mean
precipitation_yearly-mean
0
1000
489.0
1.0
1
mm/year (default: 489)
HORIZONTAL

SLIDER
18
973
420
1006
precipitation_yearly-sd
precipitation_yearly-sd
0
250
142.2
0.1
1
mm/year (default: 142.2)
HORIZONTAL

SLIDER
18
1023
426
1056
precipitation_daily-cum_n-samples
precipitation_daily-cum_n-samples
0
300
200.0
1.0
1
(default: 200)
HORIZONTAL

SLIDER
18
1060
426
1093
precipitation_daily-cum_max-sample-size
precipitation_daily-cum_max-sample-size
1
20
10.0
1.0
1
(default: 10)
HORIZONTAL

SLIDER
12
1115
496
1148
precipitation_daily-cum_plateau-value_yearly-mean
precipitation_daily-cum_plateau-value_yearly-mean
0.2
0.8
0.25
0.01
1
winter (mm)/summer (mm) (default: 0.25)
HORIZONTAL

SLIDER
13
1147
496
1180
precipitation_daily-cum_plateau-value_yearly-sd
precipitation_daily-cum_plateau-value_yearly-sd
0
0.4
0.1
0.001
1
(default: 0.1)
HORIZONTAL

SLIDER
17
1193
496
1226
precipitation_daily-cum_inflection1_yearly-mean
precipitation_daily-cum_inflection1_yearly-mean
40
140
40.0
1.0
1
day of year (default: 40)
HORIZONTAL

SLIDER
20
1229
498
1262
precipitation_daily-cum_inflection1_yearly-sd
precipitation_daily-cum_inflection1_yearly-sd
20
100
5.0
1.0
1
days (default: 5)
HORIZONTAL

SLIDER
22
1267
500
1300
precipitation_daily-cum_rate1_yearly-mean
precipitation_daily-cum_rate1_yearly-mean
0.01
0.07
0.07
0.001
1
(default: 0.07)
HORIZONTAL

SLIDER
22
1304
498
1337
precipitation_daily-cum_rate1_yearly-sd
precipitation_daily-cum_rate1_yearly-sd
0.004
0.03
0.02
0.001
1
(default: 0.02)
HORIZONTAL

SLIDER
663
1195
1133
1228
precipitation_daily-cum_inflection2_yearly-mean
precipitation_daily-cum_inflection2_yearly-mean
180
366
240.0
1.0
1
day of year (default: 240)
HORIZONTAL

SLIDER
664
1233
1132
1266
precipitation_daily-cum_inflection2_yearly-sd
precipitation_daily-cum_inflection2_yearly-sd
20
100
20.0
1
1
days (default: 20)
HORIZONTAL

SLIDER
665
1270
1130
1303
precipitation_daily-cum_rate2_yearly-mean
precipitation_daily-cum_rate2_yearly-mean
0.01
0.08
0.08
0.001
1
(default: 0.08)
HORIZONTAL

SLIDER
665
1307
1138
1340
precipitation_daily-cum_rate2_yearly-sd
precipitation_daily-cum_rate2_yearly-sd
0.004
0.03
0.02
0.001
1
(default: 0.02)
HORIZONTAL

SLIDER
97
887
423
920
precipitation_yearly-mean-bias
precipitation_yearly-mean-bias
-500
500
0.0
1
1
mm/year
HORIZONTAL

MONITOR
421
932
549
969
NIL
precipitation_yearlyMean
2
1
9

MONITOR
420
970
558
1007
NIL
precipitation_yearlySd
2
1
9

MONITOR
425
1021
582
1058
NIL
precipitation_dailyCum_nSamples
2
1
9

MONITOR
424
1058
605
1095
NIL
precipitation_dailyCum_maxSampleSize
2
1
9

MONITOR
496
1112
720
1149
NIL
precipitation_dailyCum_plateauValue_yearlyMean
2
1
9

MONITOR
497
1147
719
1184
NIL
precipitation_dailyCum_plateauValue_yearlySd
2
1
9

MONITOR
497
1193
653
1230
NIL
precipitation_dailyCum_inflection1_yearlyMean
2
1
9

MONITOR
500
1232
654
1269
NIL
precipitation_dailyCum_inflection1_yearlySd
2
1
9

MONITOR
497
1268
653
1305
NIL
precipitation_dailyCum_rate1_yearlyMean
2
1
9

MONITOR
500
1307
654
1344
NIL
precipitation_dailyCum_rate1_yearlySd
2
1
9

MONITOR
1135
1194
1291
1231
NIL
precipitation_dailyCum_inflection2_yearlyMean
2
1
9

MONITOR
1137
1229
1291
1266
NIL
precipitation_dailyCum_inflection2_yearlySd
2
1
9

MONITOR
1135
1264
1291
1301
NIL
precipitation_dailyCum_rate2_yearlyMean
2
1
9

MONITOR
1138
1303
1292
1340
NIL
precipitation_dailyCum_rate2_yearlySd
2
1
9

PLOT
1566
755
2157
875
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
1799
876
2056
996
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
1566
876
1799
996
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

MONITOR
2059
911
2132
956
year total
sum precipitation_yearSeries
2
1
11

PLOT
1566
251
2187
393
Ecological communities
days
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-y-range -1 101" ""
PENS
"mean water (%)" 1.0 1 -14454117 true "" "plot 100"
"mean crop (%)" 1.0 1 -4079321 true "" "plot 100 - mean [p_ecol_%water] of patches"
"mean wood (%)" 1.0 1 -14835848 true "" "plot 100 - (mean [p_ecol_%crop] of patches + mean [p_ecol_%water] of patches)"
"mean brush (%)" 1.0 1 -6459832 true "" "plot 100 - (mean [p_ecol_%wood] of patches + mean [p_ecol_%crop] of patches + mean [p_ecol_%water] of patches)"
"mean grass (%)" 1.0 1 -13840069 true "" "plot 100 - (mean [p_ecol_%brush] of patches + mean [p_ecol_%wood] of patches + mean [p_ecol_%crop] of patches + mean [p_ecol_%water] of patches)"
"mean bare soil (%)" 1.0 1 -16777216 true "" "plot 100 - (mean [p_ecol_%grass] of patches + mean [p_ecol_%brush] of patches + mean [p_ecol_%wood] of patches + mean [p_ecol_%crop] of patches + mean [p_ecol_%water] of patches)"

PLOT
1566
393
2229
513
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

TEXTBOX
14
494
666
512
=============================WEATHER=============================
14
0.0
1

TEXTBOX
306
407
593
439
========= RIVER =========
14
0.0
1

MONITOR
396
435
549
472
NIL
riverWaterPerFlowAccumulation
7
1
9

MONITOR
497
332
647
369
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
543
431
664
476
land units with river
landWithRiver
0
1
11

MONITOR
229
119
369
164
NIL
mostCommonTextureType
0
1
11

MONITOR
370
119
520
164
NIL
meanRunOffCurveNumber
2
1
11

MONITOR
229
164
369
209
NIL
meanWaterHoldingCapacity
4
1
11

MONITOR
368
164
525
209
NIL
meanDeepDrainageCoefficient
4
1
11

MONITOR
533
169
669
214
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
13
520
178
553
southHemisphere?
southHemisphere?
1
1
-1000

MONITOR
521
120
680
165
mean total biomass (Kg/patch)
0.001 * mean [sum p_ecol_biomass + sum p_crop_totalBiomass] of patches
4
1
11

MONITOR
554
69
630
114
mean albedo
mean [p_ecol_albedo] of patches
2
1
11

OUTPUT
744
963
1530
1164
9

PLOT
1566
10
2260
130
Crops biomass (patch mean)
days
g/m2
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

PLOT
1566
130
2251
250
Crops yield (patch mean) and annual total precipitation
years
g/m2 | mm
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

MONITOR
1401
902
1556
947
NIL
crop_HarvestingDay
0
1
11

MONITOR
1264
902
1402
947
NIL
crop_sowingDay
0
1
11

TEXTBOX
816
867
1448
894
=============================CROPS=============================
14
0.0
1

INPUTBOX
1012
889
1255
949
crop-selection
[\"wheat 1\" \"wheat 2\" \"rice\" \"barley\" \"pearl millet\" \"proso millet\"]
1
0
String

INPUTBOX
1339
10
1558
70
crop-to-display
wheat 1
1
0
String

SLIDER
735
888
994
921
crop-intensity
crop-intensity
0
100
50.0
1
1
% of patch area
HORIZONTAL

PLOT
1148
681
1348
831
Biomass per patch
g
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

BUTTON
485
229
664
262
run yield experiment batch
run-yield-performance-experiment-batch
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
160
229
315
289
experiment-initRandomSeed
4.0
1
0
Number

INPUTBOX
319
229
474
289
experiment-numberOfRuns
2.0
1
0
Number

INPUTBOX
5
229
159
289
experiment-name
exp4
1
0
String

BUTTON
479
265
542
298
exp3
experiment-3
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
544
265
607
298
exp4
experiment-4
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
738
925
987
958
randomise-crop-frequencies?
randomise-crop-frequencies?
1
1
-1000

BUTTON
610
265
673
298
exp5
experiment-5
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

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
<experiments>
  <experiment name="noRiver_dry" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="type-of-experiment">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="elev_algorithm-style">
      <value value="&quot;C#&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow_do-fill-sinks">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="terrainRandomSeed" first="0" step="1" last="100"/>
    <enumeratedValueSet variable="par_elev_seaLevelReferenceShift">
      <value value="-1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="southHemisphere?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_yearly-mean">
      <value value="489"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_yearly-sd">
      <value value="142.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crop-intensity">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crop-selection">
      <value value="&quot;[\&quot;wheat\&quot; \&quot;rice\&quot; \&quot;barley\&quot; \&quot;pearl millet\&quot;]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-simulation-in-tick">
      <value value="3650"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
