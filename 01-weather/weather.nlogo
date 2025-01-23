;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Weather model
;;  Copyright (C) 2025 Andreas Angourakis (andros.spica@gmail.com)
;;  available at https://www.github.com/Andros-Spica/indus-village-model
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
  yearLengthInDays

  albedo ; canopy reflection or albedo of hypothetical grass reference crop (0.23)
  elevation ; elevation above sea level [m] -- Convert to patch variable?

  ;;; modified parameters

  ;;;; temperature (ºC)
  temperature_annualMaxAt2m
  temperature_annualMinAt2m
  temperature_meanDailyFluctuation
  temperature_dailyLowerDeviation
  temperature_dailyUpperDeviation

  ;;;; precipitation (mm)
  precipitation_yearlyMean
  precipitation_yearlySd
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
  precipitation_dailyCum_nSamples_yearlyMean
  precipitation_dailyCum_nSamples_yearlySd
  precipitation_dailyCum_maxSampleSize_yearlyMean
  precipitation_dailyCum_maxSampleSize_yearlySd
  ;;; consult the study on modeling daily precipitation to understand role of these parameters:
  ;;; https://github.com/Andros-Spica/parModelPrecipitation
  ;;; Also see "01-weather/documentation"

  ;;;; CO2 (ppm)
  CO2_annualMin
  CO2_annualMax
  CO2_meanDailyFluctuation

  ;;;; Solar radiation (MJ/m2)
  solar_annualMax
  solar_annualMin
  solar_meanDailyFluctuation

  ;;; variables
  ;;;; time tracking
  currentYear
  currentDayOfYear

  ;;;; main (these follow a seasonal pattern and apply for all patches)
  T ; average temperature of current day (ºC)
  T_max ; maximum temperature of current day (ºC)
  T_min ; minimum temperature of current day (ºC)
  RAIN ; precipitation of current day (mm)
  CO2 ; average CO2 concentration of the current day (ppm)
  solarRadiation ; solar radiation of current day (MJ m-2)
  netSolarRadiation ; net solar radiation discount canopy reflection or albedo, assuming hypothetical grass reference crop (albedo = 0.23)
  precipitation_yearSeries
  precipitation_cumYearSeries
  ETr ; reference evapotranspiration
]

;;; agents variables

; no turtle/patch variables (assuming local homogeneous conditions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  ; --- loading/testing parameters -----------

  set-constants

  set-parameters

  ; --- core procedures ----------------------

  set currentDayOfYear 1

  update-weather

  ; -- time -------------------------------------

  reset-ticks

end

to set-constants

  ; "constants" are variables that will not be explored as parameters
  ; and may be used during a simulation.

  set yearLengthInDays 365

  ; elevation above sea level (m)
  set elevation 200

end

to set-parameters

  ; set random seed
  random-seed seed

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
    set precipitation_dailyCum_nSamples_yearlyMean precipitation_daily-cum_n-samples_yearly-mean
    set precipitation_dailyCum_nSamples_yearlySd precipitation_daily-cum_n-samples_yearly-sd
    set precipitation_dailyCum_maxSampleSize_yearlyMean precipitation_daily-cum_max-sample-size_yearly-mean
    set precipitation_dailyCum_maxSampleSize_yearlySd precipitation_daily-cum_max-sample-size_yearly-sd
  ]
  if (type-of-experiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set temperature_annualMaxAt2m 15 + random-float 25
    set temperature_annualMinAt2m -15 + random-float 30
    set temperature_meanDailyFluctuation random-float 5
    set temperature_dailyLowerDeviation random-float 10
    set temperature_dailyUpperDeviation random-float 10

    set CO2_annualMin random-normal 250 20
    set CO2_annualMax CO2_annualMin + random-float 10
    set CO2_meanDailyFluctuation max (list 0 random-normal 2.5 0.5)

    set solar_annualMin 1.5 + random-float 15
    set solar_annualMax 20 + random-float 10
    set solar_meanDailyFluctuation 3 + random-float 3

    set precipitation_yearlyMean 200 + random-float 800
    set precipitation_yearlySd random-float 200

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
    set precipitation_dailyCum_nSamples_yearlyMean 100 + random 200
    set precipitation_dailyCum_nSamples_yearlySd 10 + random 20
    set precipitation_dailyCum_maxSampleSize_yearlyMean 5 + random 20
    set precipitation_dailyCum_maxSampleSize_yearlySd 1 + random 5

    ;;; NOTES about calibration:
    ;;; Global Horizontal Irradiation can vary from about 2 to 7 KWh/m-2 per day.
    ;;; (conversion kWh/m2 to MJ/m2 is 1 : 3.6)
    ;;; See approx. values in https://globalsolaratlas.info/
    ;;; and https://www.researchgate.net/publication/271722280_Solmap_Project_In_India%27s_Solar_Resource_Assessment
    ;;; see general info in http://www.physicalgeography.net/fundamentals/6i.html
  ]

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

  if (solar_annual-max = 0)                                      [ set solar_annual-max                                         24.2 ]
  if (solar_annual-min = 0)                                      [ set solar_annual-min                                          9.2 ]
  if (solar_mean-daily-fluctuation = 0)                          [ set solar_mean-daily-fluctuation                              3.3 ]

  if (precipitation_yearly-mean = 0)                             [ set precipitation_yearly-mean                               489 ]
  if (precipitation_yearly-sd = 0)                               [ set precipitation_yearly-sd                                 142.2 ]
  if (precipitation_daily-cum_plateau-value_yearly-mean = 0)     [ set precipitation_daily-cum_plateau-value_yearly-mean         0.25 ]
  if (precipitation_daily-cum_plateau-value_yearly-sd = 0)       [ set precipitation_daily-cum_plateau-value_yearly-sd           0.1 ]
  if (precipitation_daily-cum_inflection1_yearly-mean = 0)       [ set precipitation_daily-cum_inflection1_yearly-mean          40 ]
  if (precipitation_daily-cum_inflection1_yearly-sd = 0)         [ set precipitation_daily-cum_inflection1_yearly-sd             5 ]
  if (precipitation_daily-cum_rate1_yearly-mean = 0)             [ set precipitation_daily-cum_rate1_yearly-mean                 0.07 ]
  if (precipitation_daily-cum_rate1_yearly-sd = 0)               [ set precipitation_daily-cum_rate1_yearly-sd                   0.02 ]
  if (precipitation_daily-cum_inflection2_yearly-mean = 0)       [ set precipitation_daily-cum_inflection2_yearly-mean         240 ]
  if (precipitation_daily-cum_inflection2_yearly-sd = 0)         [ set precipitation_daily-cum_inflection2_yearly-sd            20 ]
  if (precipitation_daily-cum_rate2_yearly-mean = 0)             [ set precipitation_daily-cum_rate2_yearly-mean                 0.08 ]
  if (precipitation_daily-cum_rate2_yearly-sd = 0)               [ set precipitation_daily-cum_rate2_yearly-sd                   0.02 ]
  if (precipitation_daily-cum_n-samples_yearly-mean = 0)         [ set precipitation_daily-cum_n-samples_yearly-mean           200 ]
  if (precipitation_daily-cum_n-samples_yearly-sd = 0)           [ set precipitation_daily-cum_n-samples_yearly-sd               5 ]
  if (precipitation_daily-cum_max-sample-size_yearly-mean = 0)   [ set precipitation_daily-cum_max-sample-size_yearly-mean      10 ]
  if (precipitation_daily-cum_max-sample-size_yearly-sd = 0)     [ set precipitation_daily-cum_max-sample-size_yearly-sd         3 ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set end-simulation-in-tick                     (5 * 365)

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
  set precipitation_daily-cum_n-samples_yearly-mean           200
  set precipitation_daily-cum_n-samples_yearly-sd               5
  set precipitation_daily-cum_max-sample-size_yearly-mean      10
  set precipitation_daily-cum_max-sample-size_yearly-sd         3

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ; --- core procedures -------------------------

  update-weather

  ; -- time -------------------------------------

  advance-time

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

  set CO2 gen-CO2 currentDayOfYear

  set solarRadiation gen-solar-radiation currentDayOfYear
  set netSolarRadiation (1 - albedo) * solarRadiation

  set ETr get-ETr

end

to update-temperature [ dayOfYear ]

  set T gen-temperature dayOfYear

  set T_min T - temperature_dailyLowerDeviation

  set T_max T + temperature_dailyUpperDeviation

end

to-report gen-temperature [ dayOfYear ]

  ;;; get temperature base level for the current day (ºC at lowest elevation)

  report (gen-annual-sinusoid-with-fluctuation
    temperature_annualMinAt2m
    temperature_annualMaxAt2m
    temperature_meanDailyFluctuation
    dayOfYear
    southHemisphere?
  )

end

to update-precipitation [ dayOfYear ]

  if (dayOfYear = 1) [ generate-annual-precipitation ]

  set RAIN item (dayOfYear - 1) precipitation_yearSeries

end

to generate-annual-precipitation

  ;;; Initialisation ===================================================================

  ;;; get randomised values for parameters of the double logistic curve
  let plateauValue clamp01 (random-normal precipitation_dailyCum_plateauValue_yearlyMean precipitation_dailyCum_plateauValue_yearlySd)
  let inflection1 clampMinMax (random-normal precipitation_dailyCum_inflection1_yearlyMean precipitation_dailyCum_inflection1_yearlySd) 1 yearLengthInDays
  let rate1 clampMin0 (random-normal precipitation_dailyCum_rate1_yearlyMean precipitation_dailyCum_rate1_yearlySd)
  let inflection2 clampMinMax (random-normal precipitation_dailyCum_inflection2_yearlyMean precipitation_dailyCum_inflection2_yearlySd) 1 yearLengthInDays
  let rate2 clampMin0 (random-normal precipitation_dailyCum_rate2_yearlyMean precipitation_dailyCum_rate2_yearlySd)
  ;print (word "plateauValue = " plateauValue ", inflection1 = " inflection1 ", rate1 = " rate1 ", inflection2 = " inflection2 ", rate2 = " rate2)
  let nSamples clampMin0 (random-normal precipitation_dailyCum_nSamples_yearlyMean precipitation_dailyCum_nSamples_yearlySd)
  let maxSampleSize clampMin0 (random-normal precipitation_dailyCum_maxSampleSize_yearlyMean precipitation_dailyCum_maxSampleSize_yearlySd)

  ;;; get randomised total precipitation of current year
  let totalYearPrecipitation clampMin0 (random-normal precipitation_yearlyMean precipitation_yearlySd)

  (gen-precipitation-of-year
    ; length of curve. NOTE: one more point besides lenghtOfCurve to account for the initial derivative
    (yearLengthInDays + 1)
    ; parameters for creating a double logistic curve
    plateauValue inflection1 rate1 inflection2 rate2
    ; parameters for stochastically breaking down the curve into steps
    nSamples maxSampleSize
    ; total annual precipitation
    totalYearPrecipitation)

end

to-report gen-CO2 [ dayOfYear ]

  ;;; get CO2 atmospheric concentration for the current day (ppm)

  report (gen-annual-sinusoid-with-fluctuation
    CO2_annualMin
    CO2_annualMax
    CO2_meanDailyFluctuation
    dayOfYear
    southHemisphere?
  )

end

to-report gen-solar-radiation [ dayOfYear ]

  ;;; get solar radiation for the current day (MJ/m2)

  report clampMin0 (gen-annual-sinusoid-with-fluctuation
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report gen-annual-sinusoid-with-fluctuation [ minValue maxValue meanFluctuation dayOfYear southHemisphere ]

  report random-normal (gen-annual-sinusoid minValue maxValue dayOfYear southHemisphere) meanFluctuation

end

to-report gen-annual-sinusoid [ minValue maxValue dayOfYear southHemisphere ]

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

to gen-precipitation-of-year [ yearLength plateauValue inflection1 rate1 inflection2 rate2 nSamples maxSampleSize totalYearPrecipitation ]

  set precipitation_cumYearSeries (gen-cumulative-precipitation-of-year
    yearLength plateauValue inflection1 rate1 inflection2 rate2 nSamples maxSampleSize)

  set precipitation_yearSeries (get-increments-from-cumulative-curve precipitation_cumYearSeries totalYearPrecipitation)

end

to-report gen-cumulative-precipitation-of-year [ yearLength plateauValue inflection1 rate1 inflection2 rate2 nSamples maxSampleSize ]

  ;;; Simulate *cumulative proportion of year precipitation*
  ;;; NOTE: double logistic curve as a proxy of the year series of daily cumulative precipitation

  ;;; get double logistic curve
  let cumulative-curve (gen-annual-double-logistic-curve
    yearLength
    plateauValue inflection1 rate1 inflection2 rate2
  )

  ;;; modify the curve breaking the continuous pattern by randomly averaging neighborhoods of values
  set cumulative-curve (discretise-curve cumulative-curve nSamples maxSampleSize)

  ;;; handle special case where the curve generated is a horizontal line (first = last)
  if (first cumulative-curve = last cumulative-curve) [
    set cumulative-curve (map [ i -> i / yearLength ] (n-values yearLength [ i -> i + 1 ]))
  ]

  ;;; NOTE: in some cases, the curve at this point might be too horizontal and fail to reach 1.
  ;;; This means that reaching 1 at the end of the curve (i.e. cumulative curve) takes precedence over the shape parameters
  if ((last cumulative-curve) < 1) [ print (word "Warning (precipitation): failed to generate a cumulative curve without re-scaling: " (last cumulative-curve) " < 1" )]

  ;;; re-scale the curve so it fits within 0 and 1
  set cumulative-curve rescale-curve cumulative-curve

  report cumulative-curve

end

to-report gen-annual-double-logistic-curve [ yearLength plateauValue inflection1 rate1 inflection2 rate2 ]

  let curve (list)

  foreach n-values yearLength [j -> j]
  [
    pointIndex ->
    set curve lput (gen-point-in-double-logistic pointIndex plateauValue inflection1 rate1 inflection2 rate2) curve
  ]

  report curve

end

to-report gen-point-in-double-logistic [ pointIndex plateauValue inflection1 rate1 inflection2 rate2 ]

  report (plateauValue / (1 + exp((inflection1 - pointIndex) * rate1))) + ((1 - plateauValue) / (1 + exp((inflection2 - pointIndex) * rate2)))

end

to-report discretise-curve [ curve nSamples maxSampleSize ]

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

  let newCurve curve

  let range-of-curve (last curve) - (first curve)

  ;if (range-of-curve = 0) [ report curve ]

  foreach n-values (length curve) [j -> j]
  [
    i ->
    set newCurve replace-item i newCurve (((item i curve) - (item 0 curve)) / ((last curve) - (item 0 curve)))
  ]

  report newCurve

end

to-report get-increments-from-cumulative-curve [ cumulative-curve totalYearPrecipitation ]

  ;;; Derivate *daily proportion of year precipitation* from simulated *cumulative proportion of year precipitation*.
  ;;; These are the difference between day i and day i - 1
  let increments get-incremets-from-curve cumulative-curve

  ;;; Calculate *daily precipitation* values by multipling *daily proportions of year precipitation* by the *year total precipitation*
  report map [ i -> i * totalYearPrecipitation ] increments

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

  ;;; exclude the first element (which is the extra theoretical day used for derivative calculation)
  set incrementsCurve but-first incrementsCurve

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
@#$#@#$#@
GRAPHICS-WINDOW
28
298
143
414
-1
-1
5.632
1
10
1
1
1
0
1
1
1
-9
9
-9
9
0
0
1
ticks
30.0

BUTTON
37
26
92
59
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
212
25
267
58
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
22
68
122
128
seed
0.0
1
0
Number

CHOOSER
2
182
140
227
type-of-experiment
type-of-experiment
"user-defined" "random"
0

MONITOR
1308
44
1490
81
NIL
temperature_annualMinAt2m
2
1
9

BUTTON
94
26
149
59
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
1308
10
1493
47
NIL
temperature_annualMaxAt2m
2
1
9

INPUTBOX
124
68
237
128
end-simulation-in-tick
0.0
1
0
Number

SLIDER
939
86
1310
119
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
939
121
1306
154
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
940
154
1307
187
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
937
12
1308
45
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
939
49
1303
82
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
1311
82
1491
119
NIL
temperature_meanDailyFluctuation
2
1
9

MONITOR
1307
120
1481
157
NIL
temperature_dailyLowerDeviation
2
1
9

MONITOR
1309
156
1483
193
NIL
temperature_dailyUpperDeviation
2
1
9

PLOT
277
17
933
191
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
39
238
118
283
NIL
currentYear
0
1
11

MONITOR
120
238
234
283
NIL
currentDayOfYear
0
1
11

SLIDER
941
205
1238
238
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
942
241
1238
274
CO2-annual-max
CO2-annual-max
CO2-annual-min
270
255.0
0.01
1
ppm (default: 255)
HORIZONTAL

PLOT
278
195
891
315
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

SLIDER
942
276
1238
309
CO2-mean-daily-fluctuation
CO2-mean-daily-fluctuation
0
5
1.0
0.01
1
ppm (default:1)
HORIZONTAL

BUTTON
151
25
211
58
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
941
353
1336
386
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
941
314
1338
347
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
942
391
1335
424
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
278
315
891
435
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
1238
202
1328
239
NIL
CO2_annualMin
2
1
9

MONITOR
1239
239
1358
276
NIL
CO2_annualMax
2
1
9

MONITOR
1239
277
1385
314
NIL
CO2_meanDailyFluctuation
2
1
9

MONITOR
1340
315
1452
352
NIL
solar_annualMin
3
1
9

MONITOR
1338
353
1452
390
NIL
solar_annualMax
3
1
9

MONITOR
1337
393
1495
430
NIL
solar_meanDailyFluctuation
3
1
9

SLIDER
944
442
1345
475
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
943
474
1345
507
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
941
509
1349
542
precipitation_daily-cum_n-samples_yearly-mean
precipitation_daily-cum_n-samples_yearly-mean
0
300
200.0
1.0
1
(default: 200)
HORIZONTAL

SLIDER
945
582
1348
615
precipitation_daily-cum_max-sample-size_yearly-mean
precipitation_daily-cum_max-sample-size_yearly-mean
1
20
10.0
1.0
1
(default: 10)
HORIZONTAL

SLIDER
936
663
1566
696
precipitation_daily-cum_plateau-value_yearly-mean
precipitation_daily-cum_plateau-value_yearly-mean
0.2
0.8
0.25
0.01
1
early (mm) / total (mm) (default: 0.25)
HORIZONTAL

SLIDER
938
695
1565
728
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
296
731
775
764
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
374
767
777
800
precipitation_daily-cum_inflection1_yearly-sd
precipitation_daily-cum_inflection1_yearly-sd
20
100
20.0
1.0
1
days (default: 5)
HORIZONTAL

SLIDER
374
805
779
838
precipitation_daily-cum_rate1_yearly-mean
precipitation_daily-cum_rate1_yearly-mean
0.01
0.07
0.01
0.001
1
(default: 0.07)
HORIZONTAL

SLIDER
374
842
777
875
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
936
734
1565
767
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
937
772
1565
805
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
938
809
1565
842
precipitation_daily-cum_rate2_yearly-mean
precipitation_daily-cum_rate2_yearly-mean
0.01
0.08
0.01
0.001
1
(default: 0.08)
HORIZONTAL

SLIDER
938
846
1565
879
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
1345
441
1473
478
NIL
precipitation_yearlyMean
2
1
9

MONITOR
1346
476
1484
513
NIL
precipitation_yearlySd
2
1
9

MONITOR
1348
507
1543
544
NIL
precipitation_dailyCum_nSamples_yearlyMean
2
1
9

MONITOR
1352
582
1563
619
NIL
precipitation_dailyCum_maxSampleSize_yearlyMean
2
1
9

MONITOR
1567
661
1695
698
NIL
precipitation_dailyCum_plateauValue_yearlyMean
2
1
9

MONITOR
1569
696
1707
733
NIL
precipitation_dailyCum_plateauValue_yearlySd
2
1
9

MONITOR
776
731
932
768
NIL
precipitation_dailyCum_inflection1_yearlyMean
2
1
9

MONITOR
779
770
933
807
NIL
precipitation_dailyCum_inflection1_yearlySd
2
1
9

MONITOR
776
806
932
843
NIL
precipitation_dailyCum_rate1_yearlyMean
2
1
9

MONITOR
779
845
933
882
NIL
precipitation_dailyCum_rate1_yearlySd
2
1
9

MONITOR
1569
733
1725
770
NIL
precipitation_dailyCum_inflection2_yearlyMean
2
1
9

MONITOR
1569
768
1723
805
NIL
precipitation_dailyCum_inflection2_yearlySd
2
1
9

MONITOR
1567
803
1723
840
NIL
precipitation_dailyCum_rate2_yearlyMean
2
1
9

MONITOR
1570
842
1724
879
NIL
precipitation_dailyCum_rate2_yearlySd
2
1
9

PLOT
275
435
936
724
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
"RAIN" 1.0 1 -16777216 true "" "plot RAIN"
"ETr" 1.0 0 -2674135 true "" "plot ETr"

PLOT
27
540
260
660
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
27
421
260
541
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
106
663
179
708
year total
sum precipitation_yearSeries
2
1
11

BUTTON
51
141
203
174
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

SWITCH
139
187
278
220
southHemisphere?
southHemisphere?
1
1
-1000

SLIDER
944
543
1348
576
precipitation_daily-cum_n-samples_yearly-sd
precipitation_daily-cum_n-samples_yearly-sd
1
10
5.0
1
1
(default: 5)
HORIZONTAL

SLIDER
945
617
1347
650
precipitation_daily-cum_max-sample-size_yearly-sd
precipitation_daily-cum_max-sample-size_yearly-sd
1
5
3.0
1
1
(default: 3)
HORIZONTAL

MONITOR
1349
545
1542
582
NIL
precipitation_dailyCum_nSamples_yearlySd
2
1
9

MONITOR
1352
617
1563
654
NIL
precipitation_dailyCum_maxSampleSize_yearlySd
2
1
9

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
NetLogo 6.4.0
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
