;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  SIMPLE crop model (NetLogo implementation)
;;  Copyright (C) 2021 Andreas Angourakis (andros.spica@gmail.com)
;;  available at https://www.github.com/Andros-Spica/indus-village-model
;;  based on the model of Zhao et al. 2019 (https://doi.org/10.1016/j.eja.2019.01.009)
;;  and implementing the Soil Water Balance model from Wallach et al. 2006 'Working with dynamic crop models' (p. 24-28 and p. 138-144).
;;  available at https://www.github.com/Andros-Spica/SIMPLE-crop-model
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

extensions [csv vid]

;;;;;;;;;;;;;;;;;
;;;;; BREEDS ;;;;
;;;;;;;;;;;;;;;;;

; no breeds

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals
[
  ;;; default constants
  totalPatches
  maxDist
  yearLengthInDays

  typesOfCrops

  ;;;; Soil Water Balance model global parameters (conditions assumed to be locally homogeneous)
  MUF ; Water Uptake coefficient (mm^3.mm^-3)
  WP ; Water content at wilting Point (cm^3.cm^-3)

  ;;;; Crop management
  f_Solar_max ; fSolar_max is the maximum fraction of radiation interception that a crop can reach
  ; Zhao et al. 2019 note: fSolar_max is considered as a management parameter, not a crop parameter, to account for different plant spacings. For most high-density crops, this value is set at 0.95.

  ;;; modified parameters

  ;;;; Simulated weather input
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

  ;;;; CO2 (ppm)
  CO2_annualMin
  CO2_annualMax
  CO2_meanDailyFluctuation

  ;;;; Solar radiation (MJ/m2)
  solar_annualMax
  solar_annualMin
  solar_meanDailyFluctuation

  ;;;; ETr
  albedo ; canopy reflection or albedo of hypothetical grass reference crop (0.23). See http://www.fao.org/3/X0490E/x0490e07.htm
  elevation ; elevation above sea level [m]

  ;;;; Soil Water Balance model global parameters
  DC ;  Drainage coefficient (mm^3.mm^-3).
  z ; root zone depth (mm).
  CN ; Runoff curve number.
  FC ; Water content at field capacity (cm^3.cm^-3)
  WHC ; Water Holding Capacity of the soil (cm^3.cm^-3). Typical range from 0.05 to 0.25

  ;;;; Crop parameters (extracted from cropsTable.csv)
  ;;;; the above are lists of floats
  ;;;; Species-specific
  RUE ; Radiation use efficiency (above ground only and without respiration) (g MJ−1 m-2)
  T_base ; Base temperature for phenology development and growth (ºC)
  T_opt ; Optimal temperature for biomass growth (ºC)
  I_50maxH ; The maximum daily reduction in I50B due to heat stress (ºC d)
  I_50maxW ; The maximum daily reduction in I50B due to drought stress (ºC d)
  T_heat ; Threshold temperature to start accelerating senescence from heat stress (ºC). In the Zhao et al. 2019, named as T_max
  T_extreme ; The extreme temperature threshold when RUE becomes 0 due to heat stress (ºC)
  S_CO2 ; sensitivity of crop RUE (Relative increase in RUE) per ppm elevated CO2 above 350 ppm
  S_Water ; sensitivity of crop RUE to the ARID index (representing water shortage; see below)
  ;;;; Cultivar-specific
  T_sum ; Cumulative temperature requirement from sowing to maturity (ºC d)
  HI ; Potential harvest index
  I_50A ; Cumulative temperature requirement for leaf area development to intercept 50% of radiation (ºC d)
  I_50B ; Cumulative temperature till maturity to reach 50% radiation interception due to leaf senescence (ºC d)
  ;;;; management
  sugSowingDay ; sowing day (day of year)
  sugHarvestingDay ; harvesting day (day of year)

  ;;; variables
  ;;;; time tracking
  currentYear
  currentDayOfYear

  ;;;; main (these follow a seasonal pattern and apply for all patches)

  T ; average temperature of current day (ºC)
  T_max ; maximum temperature of current day (ºC)
  T_min ; minimum temperature of current day (ºC)

  CO2 ; average CO2 concentration of the current day (ppm)

  solarRadiation ; solar radiation of current day (MJ m-2)
  netSolarRadiation ; net solar radiation discount canopy reflection or albedo, assuming hypothetical grass reference crop (albedo = 0.23)

  ETr ; reference evapotranspiration

  RAIN ; precipitation of current day (mm)
  precipitation_yearSeries
  precipitation_cumYearSeries

  ARID ; ARID index after Woli et al. 2012, ranging form 0 (no water shortage) to 1 (extreme water shortage)
  WAT ; Water content in the soil profile for the rooting depth (mm)
  WATp ; Volumetric Soil Water content (fraction : mm.mm-1). calculated as WAT/z

  sowingDay
  harvestingDay

  TT ; cumulative mean temperature (ºC day)
  biomass ; crop biomass (g)
  yield ; crop biomass harvested (g)

  ;;; auxiliar variables
  biomass_rate ; daily change in plant biomass (g)
  f_solar ; the fraction of solar ra- diation intercepted by a crop canopy
  I_50Blocal ; The cumulative temperature required to reach 50% of radiation interception during canopy senescence (I50B) (value affected by heat and drought stress)
  f_CO2 ; CO2 impact
  f_temp ; temperature impact
  f_heat ; heat stress
  f_water ; drought stress

  ;;;; counters and final measures
  ARID_yearSeries ; registers daily values of ARID of the current year (used to export data)
  ARID_yearSeries_lastYear ; saves daily values of ARID of the last year (used to export data)
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  ; --- loading/testing parameters -----------

  set-constants

  load-crops-table

  set-parameters

  ; --- core procedures ----------------------

  set currentDayOfYear 1

  setup-crops

  update-weather

  ask patches [ update-WAT ]

  ; --- output handling ------------------------

  print-crop-table

  setup-plot-crop

  update-plot-crop

  reset-ticks

end

to set-constants

  ; "constants" are variables that will not be explored as parameters
  ; and may be used during a simulation.
  ; In this example, the constants depend on the size of the dimensions (x,y)
  set totalPatches count patches
  ; maximum distance
  set maxDist sqrt (((max-pxcor - min-pxcor) ^ 2) + ((max-pxcor - min-pxcor) ^ 2))

  set yearLengthInDays 365

  ; MUF : Water Uptake coefficient (mm^3 mm^-3)
  set MUF 0.096

  ; WP : Water content at wilting Point (cm^3.cm^-3)
  set WP 0.06

  ; maximum fraction of radiation interception
  set f_Solar_max 0.95

end

to set-parameters

  ; set random seed
  random-seed randomSeed

  ; check parameters values
  parameters-check

  ;;; setup parameters depending on the type of experiment
  if (type-of-experiment = "user-defined")
  [
    ;;; load parameters from user interface

    ;;; weather generation
    set temperature_annualMaxAt2m temperature_annual-max-at-2m
    set temperature_annualMinAt2m temperature_annual-min-at-2m
    set temperature_meanDailyFluctuation temperature_mean-daily-fluctuation
    set temperature_dailyLowerDeviation temperature_daily-lower-deviation
    set temperature_dailyUpperDeviation temperature_daily-upper-deviation

    set CO2_annualMin CO2-annual-min
    set CO2_annualMax CO2-annual-max
    set CO2_meanDailyFluctuation CO2-mean-daily-fluctuation

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

    set albedo par_albedo
    set elevation par_elevation

    ;;; Soil Water Balance model
    set elevation par_elevation
    set WHC water-holding-capacity
    set DC drainage-coefficient
    set z root-zone-depth
    set CN runoff-curve
  ]
  if (type-of-experiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set temperature_annualMinAt2m -15 + random-float 30
    set temperature_annualMaxAt2m temperature_annualMinAt2m + random-float 25
    set temperature_meanDailyFluctuation random-float 5
    set temperature_dailyLowerDeviation random-float 10
    set temperature_dailyUpperDeviation random-float 10

    set CO2_annualMin 250 + random 100
    set CO2_annualMax CO2_annualMin + random-float 10
    set CO2_meanDailyFluctuation max (list 0 random-normal 2.5 0.5)

    set solar_annualMin 1.5 + random-float 15
    set solar_annualMax solar_annualMin + random-float 10
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

    set albedo 1E-6 + random-float 0.6
    set elevation random-float 2000

    ;;; Soil Water Balance model
    set elevation random-float 2000
    set WHC 0.05 + random-float 0.2
    set DC 1E-6 + random-float 0.9
    set z 1E-6 + random-float 2500
    set CN random-float 90

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

    ;;; weather generation
    set temperature_annualMaxAt2m temperature_annual-max-at-2m
    set temperature_annualMinAt2m temperature_annual-min-at-2m
    set temperature_meanDailyFluctuation temperature_mean-daily-fluctuation
    set temperature_dailyLowerDeviation temperature_daily-lower-deviation
    set temperature_dailyUpperDeviation temperature_daily-upper-deviation

    set CO2_annualMin CO2-annual-min
    set CO2_annualMax CO2-annual-max
    set CO2_meanDailyFluctuation CO2-mean-daily-fluctuation

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

    set albedo par_albedo
    set elevation par_elevation

    ;;; Soil Water Balance model
    set elevation par_elevation
    set WHC water-holding-capacity
    set DC drainage-coefficient
    set z root-zone-depth
    set CN runoff-curve
  ]

  ; FC : Water content at field capacity (cm^3.cm^-3)
  set FC WP + WHC

  ; WAT0 : Initial Water content (mm)
  set WAT z * FC

  ;;; sowing/harvest dates are initialised as the ones suggested in cropTable.csv
  set sowingDay sugSowingDay
  set harvestingDay sugHarvestingDay

end

to parameters-check

  ;;; check if values were reset to 0 (NetLogo does that from time to time...!)
  ;;; and set default values (assuming they are not 0)

  ;;; the default values of weather parameters aim to broadly represent conditions in Haryana, NW India.

  if (temperature_annual-max-at-2m = 0)                          [ set temperature_annual-max-at-2m                             37 ]
  if (temperature_annual-min-at-2m = 0)                          [ set temperature_annual-min-at-2m                             12.8 ]
  if (temperature_mean-daily-fluctuation = 0)                    [ set temperature_mean-daily-fluctuation                        2.2 ]
  if (temperature_daily-lower-deviation = 0)                     [ set temperature_daily-lower-deviation                         6.8 ]
  if (temperature_daily-upper-deviation = 0)                     [ set temperature_daily-upper-deviation                         7.9 ]

  if (CO2-annual-min = 0)                                        [ set CO2-annual-min                               245 ]
  if (CO2-annual-max = 0)                                        [ set CO2-annual-max                               255 ]
  if (CO2-mean-daily-fluctuation = 0)                            [ set CO2-mean-daily-fluctuation                     1 ]

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

  if (par_albedo = 0)                                           [ set par_albedo                                  0.23 ]

  if (par_elevation = 0)                                         [ set par_elevation                                200 ]

  if (water-holding-capacity = 0)                                [ set water-holding-capacity                       0.15 ]
  if (drainage-coefficient = 0)                                  [ set drainage-coefficient                         0.55 ]
  if (root-zone-depth = 0)                                       [ set root-zone-depth                              400 ]
  if (runoff-curve = 0)                                          [ set runoff-curve                                 65 ]

end

to parameters-to-default

  ;;; set parameters to a default value
  ;set end-simulation-in-year                                    5

  set temperature_annual-max-at-2m                             37
  set temperature_annual-min-at-2m                             12.8
  set temperature_mean-daily-fluctuation                        2.2
  set temperature_daily-lower-deviation                         6.8
  set temperature_daily-upper-deviation                         7.9

  set CO2-annual-min                                          245
  set CO2-annual-max                                          255
  set CO2-mean-daily-fluctuation                                1

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

  set par_albedo                                  0.23

  set par_elevation                                200
  set water-holding-capacity                       0.15
  set drainage-coefficient                         0.55
  set root-zone-depth                              400
  set runoff-curve                                 65

end

to setup-crops

  ;;; initialise all crop related variables as list where items correspond to crops
  set TT n-values (length typesOfCrops) [ j -> 0 ]
  set biomass n-values (length typesOfCrops) [ j -> 0 ]
  set yield n-values (length typesOfCrops) [ j -> 0 ]

  set biomass_rate n-values (length typesOfCrops) [ j -> 0 ]
  set f_solar n-values (length typesOfCrops) [ j -> 0 ]
  set I_50Blocal n-values (length typesOfCrops) [ j -> 0 ]
  set f_CO2 n-values (length typesOfCrops) [ j -> 0 ]
  set f_temp n-values (length typesOfCrops) [ j -> 0 ]
  set f_heat n-values (length typesOfCrops) [ j -> 0 ]
  set f_water n-values (length typesOfCrops) [ j -> 0 ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ; --- core procedures -------------------------

  update-weather

  update-WAT

  update-crops

  ; --- output handling ------------------------

  update-output-stats

  update-plot-crop

  ; -- time -------------------------------------

  advance-time

  tick

  ; --- stop conditions -------------------------

  if (ticks = end-simulation-in-year * yearLengthInDays) [stop]

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

  set CO2 get-CO2 currentDayOfYear

  set solarRadiation get-solar-radiation currentDayOfYear

  set netSolarRadiation (1 - albedo) * solarRadiation
  set ETr get-ETr

end

to update-temperature [ dayOfYear ]

  set T get-temperature dayOfYear

  set T_min T - temperature_dailyLowerDeviation

  set T_max T + temperature_dailyUpperDeviation

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

  set RAIN item (dayOfYear - 1) precipitation_yearSeries

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

  report max (list 0 (get-annual-sinusoid-with-fluctuation
    solar_annualMin
    solar_annualMax
    solar_meanDailyFluctuation
    dayOfYear
    southHemisphere?
  ))
  ;;; NOTE: it might be possible to decrease solar radiation depending on the current day precipitation. Additional info on precipitation effect on solar radiation is needed.

end

to-report get-CO2 [ dayOfYear ]

  ;;; get CO2 atmospheric concentration for the current day (ppm)

  report (get-annual-sinusoid-with-fluctuation
    CO2_annualMin
    CO2_annualMax
    CO2_meanDailyFluctuation
    dayOfYear
    southHemisphere?
  )

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
  let e_s (get-vapour-pressure T_max + get-vapour-pressure T_min) / 2
  let e_a get-vapour-pressure T_min
  ; ... in absence of dew point temperature, as recommended by
  ; http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data
  ; however, possibly min temp > dew temp under arid conditions

  ;;; slope of  the  vapor  pressure-temperature  curve (kPa ºC−1)
  let DELTA 4098 * (get-vapour-pressure T) / (T + 237.3) ^ 2

  ;;; latent heat of vaporisation = 2.45 MJ.kg^-1
  let lambda 2.45

  ;;; specific heat at constant pressure, 1.013 10-3 [MJ kg-1 °C-1]
  let c_p 1.013 * 10 ^ -3
  ;;; ratio molecular weight of water vapour/dry air
  let epsilon 0.622
  ;;; atmospheric pressure (kPa)
  let P 101.3 * ((293 - 0.0065 * elevation) / 293) ^ 5.26
  ;;; psychometric constant (kPa ºC−1)
  let gamma c_p * P / (epsilon * lambda)

  ;;; Penman-Monteith equation from: fao.org/3/X0490E/x0490e0 ; and from: weap21.org/WebHelp/Mabia_Alg ETRef.htm

  ; 900 and 0.34 for the grass reference; 1600 and 0.38 for the alfalfa reference
  let C_n 900
  let C_d 0.34

  let ETr_temp (0.408 * DELTA * netSolarRadiation + gamma * (C_n / (T + 273)) * windSpeed * (e_s - e_a)) / (DELTA + gamma * (1 + C_d * windSpeed))

  report ETr_temp

end

to-report get-vapour-pressure [ temp ]

  report (0.6108 * exp(17.27 * temp / (temp + 237.3)))

end

to update-WAT

  ; Soil Water Balance model
  ; Using the approach of:
  ; 'Working with dynamic crop models: Methods, tools, and examples for agriculture and enviromnent'
  ;  Daniel Wallach, David Makowski, James W. Jones, François Brun (2006, 2014, 2019)
  ;  Model description in p. 24-28, R code example in p. 138-144.
  ;  see also https://github.com/cran/ZeBook/blob/master/R/watbal.model.r
  ; Some additional info about run off at: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
  ; and at: https://en.wikipedia.org/wiki/Runoff_curve_number

  ; Maximum abstraction (mm; for run off)
  let S 25400 / CN - 254
  ; Initial Abstraction (mm; for run off)
  let IA 0.2 * S
  ; WATfc : Maximum Water content at field capacity (mm)
  let WATfc FC * z
  ; WATwp : Water content at wilting Point (mm)
  let WATwp WP * z

  ; Change in Water Before Drainage (Precipitation - Runoff)
  let RO 0
  if (RAIN > IA)
  [ set RO ((RAIN - 0.2 * S) ^ 2) / (RAIN + 0.8 * S) ]
  ; Calculating the amount of deep drainage
  let DR 0
  if (WAT + RAIN - RO > WATfc)
  [ set DR DC * (WAT + RAIN - RO - WATfc) ]

  ; Calculate rate of change of state variable WAT
  ; Compute maximum water uptake by plant roots on a day, RWUM
  let RWUM MUF * (WAT + RAIN - RO - DR - WATwp)
  ; Calculate the amount of water lost through transpiration (TR)
  let TR min (list RWUM ETr)

  let dWAT RAIN - RO - DR - TR
  set WAT WAT + dWAT

  set WATp WAT / z

  set ARID 0
  if (TR < ETr)
  [ set ARID 1 - TR / ETr ]

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

  foreach typesOfCrops
  [
    crop ->

    let cropIndex position crop typesOfCrops

    if ( is-growing cropIndex )
    [
      update-biomass cropIndex
    ]

    if ( is-ripe cropIndex )
    [
      ;;; calculate harvest yield
      ifelse (item cropIndex TT >= item cropIndex T_sum)
      [ set yield replace-item cropIndex yield (item cropIndex biomass * item cropIndex HI) ]
      [ set yield replace-item cropIndex yield 0 ]
      ;;; reset biomass and auxiliary variables
      reset-crop-variables cropIndex
    ]
  ]

end

to reset-crop-variables [ cropIndex ]

  set TT replace-item cropIndex TT 0
  set biomass replace-item cropIndex biomass 0

  set biomass_rate replace-item cropIndex biomass_rate 0
  set f_solar replace-item cropIndex f_solar 0
  set I_50Blocal replace-item cropIndex I_50Blocal 0
  set f_temp replace-item cropIndex f_temp 0
  set f_CO2 replace-item cropIndex f_CO2 0
  set f_heat replace-item cropIndex f_heat 0
  set f_water replace-item cropIndex f_water 0

end

to-report is-growing [ cropIndex ]

  let myCropSowingDay (item cropIndex sowingDay)
  let myCropHarvestingDay (item cropIndex harvestingDay)

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

  report (currentDayOfYear = item cropIndex harvestingDay)

end

to update-biomass [ cropIndex ]

  update-TT cropIndex

  update-f_CO2 cropIndex

  update-f_Temp cropIndex

  update-f_Heat cropIndex

  update-f_Water cropIndex

  set I_50Blocal replace-item cropIndex I_50Blocal ((item cropIndex I_50B) + (item cropIndex I_50maxW) * (1 - item cropIndex f_Water) + (item cropIndex I_50maxH) * (1 - item cropIndex f_Heat))

  update-f_Solar cropIndex

  set biomass_rate replace-item cropIndex biomass_rate (solarRadiation * (item cropIndex RUE) * (item cropIndex f_Solar) * (item cropIndex f_CO2) * (item cropIndex f_Temp) * (clampMin0 (min (list (item cropIndex f_Heat) (item cropIndex f_Water)))))

  set biomass replace-item cropIndex biomass (item cropIndex biomass + item cropIndex biomass_rate)

end

to update-TT [ cropIndex ]

  let deltaTT 0

  ifelse ( T > item cropIndex T_base )
  [
    set deltaTT T - item cropIndex T_base
  ]
  [
    set deltaTT 0
  ]

  set TT replace-item cropIndex TT (item cropIndex TT + deltaTT)

end

to update-f_CO2 [ cropIndex ]

  ifelse ( CO2 <= 350 )
  [
    set f_CO2 replace-item cropIndex f_CO2 1 ; this is not specified in Zhao et al. 2019
  ]
  [
    ifelse ( CO2 > 700 )
    [
      set f_CO2 replace-item cropIndex f_CO2 (1 + (item cropIndex S_CO2) * 350)
    ]
    [
      set f_CO2 replace-item cropIndex f_CO2 (1 + (item cropIndex S_CO2) * (CO2 - 350))
    ]
  ]

end

to update-f_Temp [ cropIndex ]

  ifelse ( T < item cropIndex T_base )
  [
    set f_Temp replace-item cropIndex f_Temp 0
  ]
  [
    ifelse ( T >= item cropIndex T_opt )
    [
      set f_Temp replace-item cropIndex f_Temp 1
    ]
    [
      set f_Temp replace-item cropIndex f_Temp ((T - item cropIndex T_base) / (item cropIndex T_opt - item cropIndex T_base))
    ]
  ]

end

to update-f_Heat [ cropIndex ]

  ifelse ( T_max <= item cropIndex T_heat )
  [
    set f_Heat replace-item cropIndex f_Heat 1
  ]
  [
    ifelse ( T_max > item cropIndex T_extreme )
    [
      set f_Heat replace-item cropIndex f_Heat 0
    ]
    [
      set f_Heat replace-item cropIndex f_Heat ((T_max - item cropIndex T_heat) / (item cropIndex T_extreme - item cropIndex T_heat))
    ]
  ]

end

to update-f_Water [ cropIndex ]

  set f_Water replace-item cropIndex f_Water (1 - (item cropIndex S_Water) * ARID)

end

to update-f_Solar [ cropIndex ]

  let f_Solar_early (f_Solar_max / (1 + e ^ (-0.01 * (item cropIndex TT - item cropIndex I_50A))))

  let f_Solar_late (f_Solar_max / (1 + e ^ (-0.01 * (item cropIndex TT - item cropIndex I_50Blocal))))

  set f_Solar replace-item cropIndex f_Solar min (list f_Solar_early f_Solar_late)

  ;;; drought effect
  if (item cropIndex f_Water < 0.1)
  [
    set f_Solar replace-item cropIndex f_Solar (item cropIndex f_Solar * (0.9 + item cropIndex f_Water))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-output-stats

  update-ARID_yearSeries

end

to update-ARID_yearSeries

  ; if starting a new year
  if (currentDayOfYear = 1)
  [
    ; save current year as last year
    set ARID_yearSeries_lastYear ARID_yearSeries
    ; reset ARID_yearSeries if starting a new year
    set ARID_yearSeries (list)
  ]
  ; append this day ARID to ARID_yearSeries
  set ARID_yearSeries lput ARID ARID_yearSeries

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  output-print (word " | typesOfCrops | T_sum | HI | I_50A | I_50B | T_base | T_opt | RUE | I_50maxH | I_50maxW | T_heat | T_ext | S_water | sugSowingDay | sugHarvestingDay | ")

  foreach n-values (length typesOfCrops) [j -> j]
  [
    cropIndex ->
    output-print (word
      " | " (item cropIndex typesOfCrops)
      " | " (item cropIndex T_sum)
      " | " (item cropIndex HI)
      " | " (item cropIndex I_50A)
      " | " (item cropIndex I_50B)
      " | " (item cropIndex T_base)
      " | " (item cropIndex T_opt)
      " | " (item cropIndex RUE)
      " | " (item cropIndex I_50maxH)
      " | " (item cropIndex I_50maxW)
      " | " (item cropIndex T_heat)
      " | " (item cropIndex T_extreme)
      " | " (item cropIndex S_water)
      " | " (item cropIndex sugSowingDay)
      " | " (item cropIndex sugHarvestingDay)
      " | ")
  ]

end

to setup-plot-crop

  set-current-plot "Crops biomass (patch mean)"

  foreach typesOfCrops
  [
    cropName ->
    create-temporary-plot-pen cropName
    set-plot-pen-color get-crop-color cropName
  ]

  set-current-plot "Crops yield (patch mean) and annual total precipitation"

  create-temporary-plot-pen "annual RAIN"
  set-plot-pen-mode 1
  set-plot-pen-color 0

  foreach typesOfCrops
  [
    cropName ->
    create-temporary-plot-pen cropName
    set-plot-pen-mode 0
    set-plot-pen-color get-crop-color cropName
  ]

end

to update-plot-crop

  set-current-plot "Crops biomass (patch mean)"

  foreach typesOfCrops
  [
    cropName ->
    set-current-plot-pen cropName
    plot item (position cropName typesOfCrops) biomass
  ]

  set-current-plot "Crops yield (patch mean) and annual total precipitation"

  if (currentDayOfYear = 2) ; skips plotting on setup
  [
    set-current-plot-pen "annual RAIN"
    plot sum precipitation_yearSeries
  ]

  foreach typesOfCrops
  [
    cropName ->
    set-current-plot-pen cropName
    if (is-ripe position cropName typesOfCrops)
    [
      plotxy (currentyear + (currentDayOfYear / yearLengthInDays)) (item (position cropName typesOfCrops) yield)
    ]
  ]

end

to-report get-crop-color [ cropName ]

  ; for a maximum of 13 crops
  report (16 + 9 * 10 * (position cropName typesOfCrops)) mod 140

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPORT YIELD PERFORMANCES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to experiment-1

  let numberOfBatches 2

  set experiment-name "exp1"

  set type-of-experiment "user-defined"

  parameters-to-default

  set experiment-numberOfRuns 25

  set experiment-initRandomSeed 0

  set end-simulation-in-year 30

  repeat numberOfBatches
  [
    run-yield-performance-experiment-batch

    set experiment-initRandomSeed randomSeed
  ]

end

to experiment-2

  let numberOfBatches 2

  set experiment-name "exp2"

  set type-of-experiment "precipitation-variation"

  parameters-to-default

  set experiment-numberOfRuns 25

  set experiment-initRandomSeed 0

  set end-simulation-in-year 30

  repeat numberOfBatches
  [
    run-yield-performance-experiment-batch

    set experiment-initRandomSeed randomSeed
  ]

end

to run-yield-performance-experiment-batch

  setup-yield-performance-data-file

  setup-weather-data-file

  set randomSeed experiment-initRandomSeed

  repeat experiment-numberOfRuns
  [
    setup

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

  ;;; build a unique file name according to the user setting
  let filePath (word "output//SIMPLE-crop-model_yield-exp_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  ;;; check that filePath does not exceed 100 (not common in this context)
  ;if (length filePath > 120) [ print "WARNING: file path may be too long, depending on your current directory. Decrease length of file name or increase the limit." set filePath substring filePath 0 120 ]
;print filePath
  file-open filePath

  file-print (word
    "randomSeed,"
    "temperature_annualMaxAt2m,temperature_annualMinAt2m,temperature_meanDailyFluctuation,temperature_dailyLowerDeviation,temperature_dailyUpperDeviation,"
    "solar_annualMax,solar_annualMin,solar_meanDailyFluctuation,"
    "CO2_annualMin,CO2_annualMax,CO2_meanDailyFluctuation,"
    "precipitation_yearlyMean,precipitation_yearlySd,precipitation_dailyCum_nSamples,precipitation_dailyCum_maxSampleSize,"
    "precipitation_dailyCum_plateauValue_yearlyMean,precipitation_dailyCum_plateauValue_yearlySd,"
    "precipitation_dailyCum_inflection1_yearlyMean,precipitation_dailyCum_inflection1_yearlySd,precipitation_dailyCum_rate1_yearlyMean,precipitation_dailyCum_rate1_yearlySd,"
    "precipitation_dailyCum_inflection2_yearlyMean,precipitation_dailyCum_inflection2_yearlySd,precipitation_dailyCum_rate2_yearlyMean,precipitation_dailyCum_rate2_yearlySd,"
    "currentYear,currentDayOfYear,"
    "precipitation_yearTotal,meanARID,"
    "elevation,DC,z,CN,FC,WHC,WP,MUF,albedo,"
    "crop,T_sum,HI,I_50A,I_50B,T_base,T_opt,RUE,I_50maxH,I_50maxW,T_heat,T_extreme,S_CO2,S_water,sowingDay,harvestDay,"
    "meanARID_grow,yield"
  )

  file-close

end

to export-yield-performance

  ;;; recover the unique file name according to the user setting
  let filePath (word "output//SIMPLE-crop-model_yield-exp_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  file-open filePath

  foreach typesOfCrops
  [
    crop ->

    let cropIndex position crop typesOfCrops

    ;;; randomSeed,
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
    ;;; CO2 parameters
    file-type CO2_annualMin file-type ","
    file-type CO2_annualMax file-type ","
    file-type CO2_meanDailyFluctuation file-type ","
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
    ;;; currentYear, currentDayOfYear,
    file-type currentYear file-type ","
    file-type currentDayOfYear file-type ","
    ;;; year total of precipitation
    file-type (sum precipitation_yearSeries) file-type ","
    ;;; mean ARID in current year
    file-type (mean ARID_yearSeries) file-type ","
    ;;; elevation, DC, z, CN, FC, WHC, WP, MUF, albedo
    file-type elevation file-type ","
    file-type DC file-type ","
    file-type z file-type ","
    file-type CN file-type ","
    file-type FC file-type ","
    file-type WHC file-type ","
    file-type WP file-type ","
    file-type MUF file-type ","
    file-type albedo file-type ","
    ;;; crop, sowingDay, harvestDay,
    file-type crop file-type ","
    file-type (item cropIndex T_sum) file-type ","
    file-type (item cropIndex HI) file-type ","
    file-type (item cropIndex I_50A) file-type ","
    file-type (item cropIndex I_50B) file-type ","
    file-type (item cropIndex T_base) file-type ","
    file-type (item cropIndex T_opt) file-type ","
    file-type (item cropIndex RUE) file-type ","
    file-type (item cropIndex I_50maxH) file-type ","
    file-type (item cropIndex I_50maxW) file-type ","
    file-type (item cropIndex T_heat) file-type ","
    file-type (item cropIndex T_extreme) file-type ","
    file-type (item cropIndex S_CO2) file-type ","
    file-type (item cropIndex S_water) file-type ","
    file-type (item cropIndex sowingDay) file-type ","
    file-type (item cropIndex harvestingDay) file-type ","
    ;;; mean ARID during grow season in year
    ifelse ((item cropIndex sowingDay) < (item cropIndex harvestingDay))
    [
      ; growing season fits the current calendar year
      file-type (mean sublist ARID_yearSeries (item cropIndex sowingDay) (item cropIndex harvestingDay)) file-type "," ]
    [
      ; growing season spans also into last year
      ifelse (currentYear = 0)
      [
        ; there is no last year
        file-type "," ; these NA will be signaling the rows that should be ignored in analysis
      ]
      [
        file-type (mean sentence (sublist ARID_yearSeries 1 (item cropIndex harvestingDay)) (sublist ARID_yearSeries_lastYear (item cropIndex sowingDay) yearLengthInDays)) file-type ","
      ]
    ]
    ;;; yield
    file-type (item cropIndex yield)
    file-print ""
  ]

  file-close

end

to setup-weather-data-file

  ;;; build a unique file name according to the user setting
  let filePath (word "output//SIMPLE-crop-model_yield-exp_weather_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  ;;; check that filePath does not exceed 100 (not common in this context)
  ;if (length filePath > 120) [ print "WARNING: file path may be too long, depending on your current directory. Decrease length of file name or increase the limit." set filePath substring filePath 0 120 ]
;print filePath
  file-open filePath

  file-print (word
    "randomSeed,"
    "currentYear,currentDayOfYear,"
    "temperature,temperature_max,temperature_min,"
    "CO2,solarRadiation,netSolarRadiation,"
    "ETr,RAIN,ARID"
  )

  file-close

end

to export-weather-of-yield-experiment

  ;;; recover the unique file name according to the user setting
  let filePath (word "output//SIMPLE-crop-model_yield-exp_weather_type-of-experiment=" type-of-experiment "_experiment-name=" experiment-name "_initRandomSeed=" experiment-initRandomSeed ".csv")

  file-open filePath

  ;;; randomSeed,
  file-type randomSeed file-type ","
  ;;; currentYear, currentDayOfYear,
  file-type currentYear file-type ","
  file-type currentDayOfYear file-type ","
  ;;; tempetature,temperature_max,temperature_min,
  file-type T file-type ","
  file-type T_max file-type ","
  file-type T_min file-type ","
  ;;; CO2,
  file-type CO2 file-type ","
  ;;; solarRadiation,
  file-type solarRadiation file-type ","
  file-type netSolarRadiation file-type ","
  ;;; ETr
  file-type ETr file-type ","
  ;;; RAIN
  file-type RAIN file-type ","
  ;;; ARID
  file-type ARID

  file-print ""

  file-close

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD DATA FROM TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  let S_CO2Column (item 29 (item 3 cropsTable)) - 1

  let S_waterColumn (item 31 (item 3 cropsTable)) - 1

  let sugSowingDayColumn (item 33 (item 3 cropsTable)) - 1

  let sugHarvestingDayColumn (item 35 (item 3 cropsTable)) - 1

  ;;;==================================================================================================================
  ;;; extract data---------------------------------------------------------------------------------------

  ;;; read variables per crop type (list of lists, matrix: crop types x variables)
  let cropsData sublist cropsTable (item 0 typesOfCropsRowRange) (item 1 typesOfCropsRowRange + 1) ; select only those row corresponding to types of crops, if there is anything else

  ;;; extract types of crops from the first column
  set typesOfCrops map [row -> item 0 row ] cropsData

  ;;; extract parameter values from the given column
  set T_sum map [row -> item T_sumColumn row ] cropsData

  set HI map [row -> item HIColumn row ] cropsData

  set I_50A map [row -> item I_50AColumn row ] cropsData

  set I_50B map [row -> item I_50BColumn row ] cropsData

  set T_base map [row -> item T_baseColumn row ] cropsData

  set T_opt map [row -> item T_optColumn row ] cropsData

  set RUE map [row -> item RUEColumn row ] cropsData

  set I_50maxH map [row -> item I_50maxHColumn row ] cropsData

  set I_50maxW map [row -> item I_50maxWColumn row ] cropsData

  set T_heat map [row -> item T_heatColumn row ] cropsData

  set T_extreme map [row -> item T_extColumn row ] cropsData

  set S_CO2 map [row -> item S_CO2Column row ] cropsData

  set S_water map [row -> item S_waterColumn row ] cropsData

  set sugSowingDay map [row -> item sugSowingDayColumn row ] cropsData

  set sugHarvestingDay map [row -> item sugHarvestingDayColumn row ] cropsData

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
@#$#@#$#@
GRAPHICS-WINDOW
18
299
76
358
-1
-1
1.0
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

BUTTON
31
20
86
53
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
206
19
261
52
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

INPUTBOX
16
62
116
122
randomSeed
20.0
1
0
Number

CHOOSER
52
142
222
187
type-of-experiment
type-of-experiment
"user-defined" "random" "precipitation-variation"
2

MONITOR
1420
958
1602
995
NIL
temperature_annualMinAt2m
2
1
9

BUTTON
88
20
143
53
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
1420
924
1605
961
NIL
temperature_annualMaxAt2m
2
1
9

INPUTBOX
118
62
247
122
end-simulation-in-year
30.0
1
0
Number

SLIDER
1049
1000
1420
1033
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
1049
1035
1416
1068
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
1050
1068
1417
1101
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
1049
926
1420
959
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
1051
963
1415
996
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
1421
998
1601
1035
NIL
temperature_meanDailyFluctuation
2
1
9

MONITOR
1417
1034
1591
1071
NIL
temperature_dailyLowerDeviation
2
1
9

MONITOR
1419
1070
1593
1107
NIL
temperature_dailyUpperDeviation
2
1
9

PLOT
282
1020
938
1140
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
"mean" 1.0 0 -16777216 true "" "plot T"
"min" 1.0 0 -13345367 true "" "plot T_min"
"max" 1.0 0 -2674135 true "" "plot T_max"

MONITOR
31
201
110
246
NIL
currentYear
0
1
11

MONITOR
112
201
226
246
NIL
currentDayOfYear
0
1
11

BUTTON
145
19
205
52
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

PLOT
282
10
976
387
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

SLIDER
1033
1159
1428
1192
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
1033
1120
1430
1153
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
1034
1197
1427
1230
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
282
1137
895
1257
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
1433
1116
1545
1153
NIL
solar_annualMin
3
1
9

MONITOR
1431
1154
1545
1191
NIL
solar_annualMax
3
1
9

MONITOR
1430
1194
1588
1231
NIL
solar_meanDailyFluctuation
3
1
9

SLIDER
1036
451
1437
484
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
1035
483
1437
516
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
1028
371
1436
404
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
1031
408
1434
441
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
998
529
1550
562
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
1000
561
1550
594
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
999
604
1478
637
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
1077
640
1480
673
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
1077
678
1482
711
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
1077
715
1480
748
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
1073
755
1483
788
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
1074
793
1477
826
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
1075
830
1480
863
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
1075
867
1478
900
precipitation_daily-cum_rate2_yearly-sd
precipitation_daily-cum_rate2_yearly-sd
0.004
0.03
0.02
0.001
1
(default: 0.02)
HORIZONTAL

MONITOR
1436
450
1564
487
NIL
precipitation_yearlyMean
2
1
9

MONITOR
1437
485
1575
522
NIL
precipitation_yearlySd
2
1
9

MONITOR
1435
369
1608
406
NIL
precipitation_dailyCum_nSamples
2
1
9

MONITOR
1438
408
1592
445
NIL
precipitation_dailyCum_maxSampleSize
2
1
9

MONITOR
1549
528
1677
565
NIL
precipitation_dailyCum_plateauValue_yearlyMean
2
1
9

MONITOR
1550
563
1688
600
NIL
precipitation_dailyCum_plateauValue_yearlySd
2
1
9

MONITOR
1479
604
1635
641
NIL
precipitation_dailyCum_inflection1_yearlyMean
2
1
9

MONITOR
1482
643
1636
680
NIL
precipitation_dailyCum_inflection1_yearlySd
2
1
9

MONITOR
1479
679
1635
716
NIL
precipitation_dailyCum_rate1_yearlyMean
2
1
9

MONITOR
1482
718
1636
755
NIL
precipitation_dailyCum_rate1_yearlySd
2
1
9

MONITOR
1483
755
1639
792
NIL
precipitation_dailyCum_inflection2_yearlyMean
2
1
9

MONITOR
1479
796
1633
833
NIL
precipitation_dailyCum_inflection2_yearlySd
2
1
9

MONITOR
1477
831
1633
868
NIL
precipitation_dailyCum_rate2_yearlyMean
2
1
9

MONITOR
1480
870
1634
907
NIL
precipitation_dailyCum_rate2_yearlySd
2
1
9

PLOT
282
901
943
1021
precipitation
days
ppm
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"RAIN" 1.0 1 -16777216 true "" "plot RAIN"
"ETr" 1.0 0 -2674135 true "" "plot mean [ETr] of patches"

PLOT
27
952
260
1072
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
25
822
258
942
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
851
950
924
995
year total
sum precipitation_yearSeries
2
1
11

PLOT
282
783
967
903
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
"mean ARID" 1.0 0 -16777216 true "" "plot mean [ARID] of patches"
"mean WTp" 1.0 0 -13345367 true "" "plot mean [WATp] of patches"

SLIDER
7
703
198
736
par_elevation
par_elevation
0
2500
200.0
1
1
m a.s.l.
HORIZONTAL

SLIDER
-1
556
238
589
water-holding-capacity
water-holding-capacity
0.01
0.5
0.15
0.01
1
cm3/cm3
HORIZONTAL

SLIDER
-2
587
238
620
drainage-coefficient
drainage-coefficient
0
1
0.55
0.01
1
NIL
HORIZONTAL

SLIDER
-2
619
238
652
root-zone-depth
root-zone-depth
0
2000
400.0
1
1
mm
HORIZONTAL

SLIDER
-3
653
240
686
runoff-curve
runoff-curve
50
80
65.0
1
1
NIL
HORIZONTAL

MONITOR
201
702
271
739
NIL
elevation
2
1
9

MONITOR
238
555
288
592
NIL
WHC
2
1
9

MONITOR
238
587
288
624
NIL
DC
2
1
9

MONITOR
237
615
305
652
NIL
z
2
1
9

MONITOR
240
652
290
689
NIL
CN
2
1
9

BUTTON
10
516
164
549
NIL
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

SLIDER
8
741
200
774
par_albedo
par_albedo
0
0.7
0.23
0.01
1
NIL
HORIZONTAL

MONITOR
201
742
267
779
NIL
albedo
2
1
9

SWITCH
60
784
215
817
southHemisphere?
southHemisphere?
1
1
-1000

OUTPUT
977
10
1813
358
13

PLOT
282
385
978
783
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

SLIDER
1080
1253
1428
1286
CO2-annual-min
CO2-annual-min
200
CO2-annual-max
245.0
0.01
1
ppm (default: 245)
HORIZONTAL

SLIDER
1082
1288
1428
1321
CO2-annual-max
CO2-annual-max
CO2-annual-min
270
255.0
0.01
1
ppm (default: 255)
HORIZONTAL

SLIDER
1082
1321
1428
1354
CO2-mean-daily-fluctuation
CO2-mean-daily-fluctuation
0
5
1.0
0.01
1
ppm (default:1)
HORIZONTAL

MONITOR
1428
1322
1574
1359
NIL
CO2_meanDailyFluctuation
2
1
9

MONITOR
1430
1287
1523
1324
NIL
CO2_annualMax
2
1
9

MONITOR
1429
1252
1519
1289
NIL
CO2_annualMin
2
1
9

PLOT
284
1257
893
1377
CO2
days
ppm
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range (precision (CO2_annualMin - CO2_meanDailyFluctuation - 1) 2) (precision (CO2_annualMax + CO2_meanDailyFluctuation + 1) 2)" "set-plot-y-range (precision (CO2_annualMin - CO2_meanDailyFluctuation - 1) 2) (precision (CO2_annualMax + CO2_meanDailyFluctuation + 1) 2)"
PENS
"default" 1.0 0 -16777216 true "" "plot CO2"

BUTTON
9
468
188
501
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
94
269
249
329
experiment-initRandomSeed
0.0
1
0
Number

INPUTBOX
95
327
250
387
experiment-numberOfRuns
25.0
1
0
Number

INPUTBOX
32
394
251
454
experiment-name
exp2
1
0
String

BUTTON
204
468
267
501
exp1
experiment-1
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
204
508
267
541
exp2
experiment-2
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
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2190"/>
    <metric>T</metric>
    <metric>T_max</metric>
    <metric>T_min</metric>
    <metric>RAIN</metric>
    <metric>solarRadiation</metric>
    <metric>ET_0</metric>
    <metric>mean [WATp] of patches</metric>
    <metric>mean [ARID] of patches</metric>
    <metric>mean [biomass] of patches with [position crop typesOfCrops = 0]</metric>
    <metric>mean [biomass] of patches with [position crop typesOfCrops = 1]</metric>
    <metric>mean [yield] of patches with [position crop typesOfCrops = 0]</metric>
    <metric>mean [yield] of patches with [position crop typesOfCrops = 1]</metric>
    <enumeratedValueSet variable="precipitation_daily-cum_rate2_yearly-mean">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CO2-mean">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solar_annual-min">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_rate2_yearly-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CO2-daily-fluctuation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_yearly-sd">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solar_annual-max">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_plateau-value_yearly-sd">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_n-sample">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_inflection1_yearly-sd">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CO2-annual-deviation">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_max-sample-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_inflection2_yearly-sd">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_inflection2_yearly-mean">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temperature_mean-daily-fluctuation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temperature_annual-min-at-2m">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solar_mean-daily-fluctuation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temperature_daily-lower-deviation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temperature_annual-max-at-2m">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temperature_daily-upper-deviation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-simulation-in-tick">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_yearly-mean">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_inflection1_yearly-mean">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_rate1_yearly-mean">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="display-mode">
      <value value="&quot;crops&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_plateau-value_yearly-mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="precipitation_daily-cum_rate1_yearly-sd">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-experiment">
      <value value="&quot;user-defined&quot;"/>
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
