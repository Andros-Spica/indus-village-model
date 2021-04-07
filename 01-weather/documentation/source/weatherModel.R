# R source code for the demonstration of
# the Indus Village model - Weather model

# visit this model repository at https://github.com/Andros-Spica/indus-village-model/blob/master/01-weather

#=======================================================================
 
# base generic functions

## on solar radiation and temperature

################################################################################
#' @title Get a day's value in an annual sinusoidal curve
#' @description Calculate the value for a given day in an annual sinusoidal curve defined by maximum and minimum values, the length of the year in days, and the the day with the lowest value in an year
#' @param minValue,maxValue : minimum and maximum values of the sinusoidal curve within the year
#' @param dayOfYear : the day of year of the value to be returned
#' @param yearLengthInDays : the number of days in the given year
#' @param dayOfYearWithLowestValue : the day with the lowest value in an year
#' @return point value for the day of year (same units of minValue and maxValue)
#' @export
getDayValueInAnnualSinusoid <- function(dayOfYear,
                                        minValue, 
                                        maxValue,  
                                        yearLengthInDays, 
                                        dayOfYearWithLowestValue)
{
  amplitude = (maxValue - minValue) / 2
  
  return(
    minValue + amplitude * (1 + sin(2*pi * ((dayOfYear - (dayOfYearWithLowestValue - yearLengthInDays)) / yearLengthInDays) - pi/2))
  )
}

################################################################################
#' @title Get day of year with lowest value in annual sinusoidal curve
#' @description get the day of year with the lowest value in annual sinusoidal curve according to whether it refers to Earth's southern hemisphere
#' @param southHemisphere : whether the annual curve corresponds to values in the southern or the northern hemisphere; depending on this, winter and summer solstices, as the days with minimum and maximum values, are timed differently)
#' @param yearLengthInDays : the number of days in the given year
#' @return day of year with the lowest value in the annual sinusoidal curve
#' @export
getDayOfYearWithLowestValue <- function(southHemisphere)
{
  dayOfYearWithLowestValue = (31+28+31+30+31+30+31+31+30+31+30+21)
  # assuming northern hemisphere, winter solstice in 21st December
  if (southHemisphere)
  {
    dayOfYearWithLowestValue = (31+28+31+30+31+21)
  }
  # assuming southern hemisphere, winter solstice in 21st June
  
  return(dayOfYearWithLowestValue)
}

################################################################################
#' @title Get a day's value in an annual sinusoidal curve with fluctuations
#' @description Calculate the value for a given day in an annual sinusoidal curve defined by maximum and minimum values, the length of the year in days, and whether it refers to Earth's southern hemisphere
#' @param minValue,maxValue : minimum and maximum values of the sinusoidal curve within the year
#' @param dayOfYear : the day of year of the value to be returned
#' @param yearLengthInDays : the number of days in the given year
#' @param southHemisphere : whether the annual curve corresponds to values in the southern or the northern hemisphere; depending on this, winter and summer solstices, as the days with minimum and maximum values, are timed differently)
#' @param fluctuation : the standard deviation of the normal random noise to be added to the sinusoidal curve (units are the same than minValue and maxValue) 
#' @param seed : random number generator seed
#' @return point value for the day of year (same units of minValue and maxValue)
#' @export
getDayValueInAnnualSinusoidWithFluctuation <- function(dayOfYear, 
                                                       minValue, 
                                                       maxValue, 
                                                       yearLengthInDays, 
                                                       southHemisphere = FALSE,
                                                       fluctuation, 
                                                       seed = 0)
{
  set.seed(seed = seed)
  
  dayOfYearWithLowestValue = getDayOfYearWithLowestValue(southHemisphere)
  
  return(
    rnorm(1, 
          getDayValueInAnnualSinusoid(
            dayOfYear,
            minValue, maxValue,
            yearLengthInDays, dayOfYearWithLowestValue), 
          fluctuation)
  )
}

################################################################################
#' @title Get an annual sinusoidal curve
#' @description Calculate an annual sinusoidal curve defined by maximum and minimum values, the length of the year in days, and whether it refers to Earth's southern hemisphere
#' @param minValue,maxValue : minimum and maximum values of the sinusoidal curve within the year
#' @param yearLengthInDays : the number of days in the given year
#' @param southHemisphere : whether the annual curve corresponds to values in the southern or the northern hemisphere; depending on this, winter and summer solstices, as the days with minimum and maximum values, are timed differently)
#' @return values for each day of year describing an annual sinusoidal curve (same units of minValue and maxValue)
#' @export
getAnnualSinusoid <- function(minValue, 
                              maxValue,  
                              yearLengthInDays, 
                              southHemisphere = FALSE)
{
  dayOfYearWithLowestValue = getDayOfYearWithLowestValue(southHemisphere)
  
  curve <- c()
  
  for (dayOfYear in 1:yearLengthInDays)
  {
    curve <- c(curve, 
               getDayValueInAnnualSinusoid(
                 dayOfYear = dayOfYear,
                 minValue = minValue, 
                 maxValue = maxValue,
                 yearLengthInDays = yearLengthInDays, 
                 dayOfYearWithLowestValue = dayOfYearWithLowestValue
               )
    )
  }
  
  return(curve)
}

################################################################################
#' @title Get an annual sinusoidal curve with fluctuation
#' @description Calculate an annual sinusoidal curve defined by maximum and minimum values, the length of the year in days, and whether it refers to Earth's southern hemisphere
#' @param minValue,maxValue : minimum and maximum values of the sinusoidal curve within the year
#' @param yearLengthInDays : the number of days in the given year
#' @param southHemisphere : whether the annual curve corresponds to values in the southern or the northern hemisphere; depending on this, winter and summer solstices, as the days with minimum and maximum values, are timed differently)
#' @param fluctuation : the standard deviation of the normal random noise to be added to the sinusoidal curve (units are the same than minValue and maxValue) 
#' @return values for each day of year describing an annual sinusoidal curve with fluctuations (same units of minValue and maxValue)
#' @export
getAnnualSinusoidWithFluctuation <- function(minValue, 
                                             maxValue,  
                                             yearLengthInDays, 
                                             southHemisphere = FALSE,
                                             fluctuation,
                                             seed = 0)
{
  set.seed(seed = seed)
  
  curve <- c()
  
  for (dayOfYear in 1:yearLengthInDays)
  {
    curve <- c(curve, 
               getDayValueInAnnualSinusoidWithFluctuation(
                 dayOfYear = dayOfYear,
                 minValue = minValue, 
                 maxValue = maxValue,
                 yearLengthInDays = yearLengthInDays, 
                 southHemisphere = southHemisphere,
                 fluctuation = fluctuation,
                 seed = runif(1, 0, 2147483647)
               )
    )
  }
  
  return(curve)
}

#=======================================================================

## on precipitation

################################################################################
#' @title Get a day's value in an annual double logistic curve
#' @description Calculate the value for a given day in an annual double logistic curve defined by five shape parameters
#' @param plateauValue : the value (range of 0 to 1) in which the gap between logistic curves is set
#' @param inflection1,inflection2 : the days of year in which the first and second logistic curves have their maximum slope
#' @param rate1,rate2 : the maximum rate or slope increase of the first and second logistic curves
#' @return point value for the day of year (range of 0 to 1)
#' @export
getDayValueInAnnualDoubleLogistic <- function(dayOfYear, 
                                              plateauValue, 
                                              inflection1, 
                                              rate1, 
                                              inflection2, 
                                              rate2
)
{
  return(
    (plateauValue / (1 + exp((inflection1 - dayOfYear) * rate1))) + 
      ((1 - plateauValue) / (1 + exp((inflection2 - dayOfYear) * rate2)))
  )
}

################################################################################
#' @title Get an annual double logistic curve
#' @description Calculate the daily values of an annual double logistic curve defined by five shape parameters
#' @param plateauValue : the value (range of 0 to 1) in which the gap between logistic curves is set
#' @param inflection1,inflection2 : the days of year in which the first and second logistic curves have their maximum slope
#' @param rate1,rate2 : the maximum rate or slope increase of the first and second logistic curves
#' @param yearLengthInDays : the number of days in the given year
#' @return daily values following an annual double logistic curve (range of 0 to 1)
#' @export
getAnnualDoubleLogisticCurve <- function(plateauValue, 
                                inflection1, 
                                rate1, 
                                inflection2, 
                                rate2,
                                yearLengthInDays
)
{
  curve <- c()
  for (i in 1:yearLengthInDays)
  {
    curve <- c(curve, getDayValueInAnnualDoubleLogistic(i, plateauValue, inflection1, rate1, inflection2, rate2))
  }
  return(curve)
}

################################################################################
#' @title Escalonate curve stochastically
#' @description Break curve slope into several random steps, each consisting of an increase with maximum slope followed by a plateau
#' @param curve : the y variable describing the curve to be modified (numeric vector)
#' @param nSteps : the number of random samples or steps to be created in the curve
#' @param maxSampleSize : the maximum length of samples or step plateaus
#' @param seed : random number generator seed
#' @return an escalonated version of the curve given as input (numeric, vector)
#' @export
escalonateCurve <- function(curve, 
                            nSamples, 
                            maxSampleSize,
                            seed = 0)
{
  set.seed(seed = seed)
  
  indexes <- 1:length(curve)
  
  for (i in 1:nSamples)
  {
    # get a decreasing sample size proportionally to sample i
    thisSampleSize = ceiling(maxSampleSize * i / nSamples)
    # get random x coord point
    plateauMiddlePoint = round(runif(1, min = 1, max = length(indexes)))
    # set sample limits
    earliestNeighbour = max(1, plateauMiddlePoint - thisSampleSize)
    latestNeighbour = min(length(indexes), plateauMiddlePoint + thisSampleSize)
    neighbourhood = curve[indexes >= earliestNeighbour & indexes <= latestNeighbour]
    # get mean of neighbourhood
    meanNeighbourhood = mean(neighbourhood)
    # assign mean to all days in neighbourhood
    for (j in earliestNeighbour:latestNeighbour)
    {
      curve[indexes == j] <- meanNeighbourhood
    }
  }
  return(curve)
}

################################################################################
#' @title Rescale curve to 0-1 interval
#' @description Rescale curve to the 0-1 interval
#' @param curve : the y variable describing the curve to be modified (numeric vector)
#' @export
rescaleCurve <- function(curve)
{
  # cover special case where the curve is a horizontal line (first = last)
  # solution: interpolate 0-1 with a line
  if (curve[1] == curve[length(curve)]) 
  { 
    curve <- 1:length(curve) * 1 / length(curve)
  }
  
  return( (curve - curve[1]) / (curve[length(curve)] - curve[1]) )
}

################################################################################
#' @title Get increments from cumulative curve
#' @description Calculate the incremental or differences curve corresponding to a given cumulative curve
#' @param cumulativeCurve : the cumulative curve from which to calculate the incremental curve (numeric vector)
#' @return the incremental curve corresponding to the given cumulative curve (numeric vector)
#' @export
getIncrementsFromCumulativeCurve <- function(cumulativeCurve)
{
  incrementCurve <- c()
  
  # incrementCurve = incrementCurve, if it is the first point
  incrementCurve[1] = cumulativeCurve[1]

  incrementCurve <- c(incrementCurve, diff(cumulativeCurve))  
  # for (i in 2:length(cumulativeCurve))
  # {
  #   incrementCurve[i] <- cumulativeCurve[i] - cumulativeCurve[i - 1]
  # }
  
  # correct negative values (in rare occasions, very small negative numbers appear at this stage)
  incrementCurve <- sapply(incrementCurve, function(x) max(c(0, x)))
  
  return(incrementCurve)
}

################################################################################
#' @title Get daily precipitation of one year
#' @description Calculate the daily values of precipitation using the double logistic cumulative curve approach
#' @param plateauValue : the value (range of 0 to 1) in which the gap between logistic curves is set
#' @param inflection1,inflection2 : the days of year in which the first and second logistic curves have their maximum slope
#' @param rate1,rate2 : the maximum rate or slope increase of the first and second logistic curves
#' @param yearLengthInDays : the number of days in the given year
#' @param nSteps : the number of random samples or steps to be created in the cumulative curve
#' @param maxSampleSize : the maximum length of samples or step plateaus in the cumulative curve
#' @param annualSum : the annual sum of precipitation (mm)
#' @param seed : random number generator seed
#' @return daily precipitation values (numeric vector, mm)
#' @export
getPrecipitationOfYear <- function(plateauValue, 
                                   inflection1, 
                                   rate1, 
                                   inflection2, 
                                   rate2, 
                                   yearLengthInDays, 
                                   nSamples, 
                                   maxSampleSize, 
                                   annualSum,
                                   seed = 0)
{
  precipitationOfYear <- c()
  
  precipitationOfYear <- getAnnualDoubleLogisticCurve(
    plateauValue = plateauValue,
    inflection1 = inflection1,
    rate1 = rate1,
    inflection2 = inflection2,
    rate2 = rate2,
    yearLengthInDays = yearLengthInDays
  )
  
  precipitationOfYear <- escalonateCurve(
    curve = precipitationOfYear,
    nSamples = nSamples, 
    maxSampleSize = maxSampleSize,
    seed = seed)
  
  # check if tries were enough to get the precipitation of the year
  if (getLastItemInVector(precipitationOfYear) < 1) 
  { 
    simpleWarning(paste(
      "failed to generate a precipitation of the year that fulfills 'annualSum' without re-scaling"
    ))
  }
  
  precipitationOfYear <- rescaleCurve(precipitationOfYear)
  
  precipitationOfYear <- getIncrementsFromCumulativeCurve(precipitationOfYear) * annualSum
  
  return(precipitationOfYear)
}

################################################################################
#' @title Get daily proportion to annual sum of precipitation of one year
#' @description Get daily proportion to annual sum of precipitation of one year 
#' @param dailyPrecipitationYear : daily values of precipitation for a length of one year
#' @return daily proportion of annual precipitation (numeric vector, mm/mm)
#' @export
getProportionPrecipitationOfYear <- function(dailyPrecipitationYear)
{
  return(dailyPrecipitationYear / sum(dailyPrecipitationYear))
}

################################################################################
#' @title Get daily proportion to annual sum of precipitation of multiple years
#' @description Get daily proportion to annual sum of precipitation of multiple years 
#' @param dailyPrecipitation : daily values of precipitation for multiple full years
#' @return daily proportion of annual precipitation (numeric vector, mm/mm)
#' @export
getProportionPrecipitation <- function(dailyPrecipitation, years)
{
  proportionSeries <- c()
  
  for (year in levels(factor(years)))
  {
    proportionSeries <- c(
      proportionSeries,
      getProportionPrecipitationOfYear(dailyPrecipitation[years == year])) 
  }
  
  return(proportionSeries)
}

################################################################################
#' @title Get daily cumulative sum of precipitation of one year
#' @description Get daily cumulative sum of precipitation of one year 
#' @param dailyPrecipitationYear : daily values of precipitation for a length of one year
#' @return daily cumulative sum of annual precipitation (numeric vector, mm/mm)
#' @export
getCumulativePrecipitationOfYear <- function(dailyPrecipitationYear)
{
  return(cumsum(dailyPrecipitationYear) / sum(dailyPrecipitationYear))
}

################################################################################
#' @title Get daily cumulative sum of precipitation of multiple years
#' @description Get daily cumulative sum of precipitation of multiple years 
#' @param dailyPrecipitation : daily values of precipitation for multiple full years
#' @return daily cumulative sum of annual precipitation (numeric vector, mm/mm)
#' @export
getCumulativePrecipitation <- function(dailyPrecipitation, years)
{
  cumulativeProportionSeries <- c()
  
  for (year in levels(factor(years)))
  {
    cumulativeProportionSeries <- c(
      cumulativeProportionSeries,
      getCumulativePrecipitationOfYear(dailyPrecipitation[years == year])) 
  }
  
  return(cumulativeProportionSeries)
}

#=======================================================================

## reference evapotranspiration
################################################################################
#' @title Estimate daily reference evapotranspiration (ETr)
#' @description Estimates daily reference evapotranspiration using either Penman-Monteith equation (default) or Priestley-Taylor equation.
#' @param R_s : solar radiation or insolation (MJ m2 day-1)
#' @param temperature : daily average temperature at 2m (ºC)
#' @param temperature_max : daily maximum temperature at 2m (ºC)
#' @param temperature_min : daily minimum temperature at 2m (ºC)
#' @param temperature_dew : daily dew point temperature at 2m (ºC). If not available, can be assumed to be equal to temperature_min, following recommendations in http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data. However, possibly temperature_min > temperature_dew under arid conditions
#' @param windSpeed : mean daily wind speed at 2m (m s-1). If not available, can be assumed to be 2, following recommendations in http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data
#' @param albedo : canopy reflection or albedo. If not available, it can be set at 0.23, assuming hypothetical grass reference crop
#' @param z : elevation above sea level (m)
#' @param lambda : latent heat of vaporisation = 2.45 MJ.kg^-1
#' @param c_p : specific heat at constant pressure, 1.013 10-3 (MJ kg-1 °C-1)
#' @param epsilon : ratio molecular weight of water vapour/dry air
#' @param method : "PM" to use Penman-Monteith equation; "PT" to use Priestley-Taylor equation
#' @param C_n : constant used in Penman-Monteith equation. Set as 900 assuming grass reference or 1600 for alfafa reference
#' @param C_d : constant used in Penman-Monteith equation. Set as 0.34 assuming grass reference or 0.38 for alfafa reference
#' @param alpha : Priestley-Taylor coefficient (1.26)
#' @return vector with daily ETr (mm day-1)
#' @export
estimateETr <- function(R_s, 
                        temperature,  temperature_max, temperature_min,
                        temperature_dew = temperature_min,
                        windSpeed = 2, 
                        albedo = 0.23,
                        z = 200,
                        lambda = 2.45,
                        c_p = 1.013 * 10^-3,
                        epsilon = 0.622,
                        method = 'PM',
                        C_n = 900,
                        C_d = 0.34,
                        alpha = 1.26)
{
  ETr <- NULL
  
  # estimate the net solar radiation, 
  
  netSolarRadiation <- (1 - albedo) * R_s
  
  # estimation of saturated vapour pressure (e_s) and actual vapour pressure (e_a)
  # according to 
  # Allen, R. G., Pereira, L. A., Raes, D., and Smith, M. 1998. 
  # “Crop evapotranspiration.”FAO irrigation and  drainage paper 56, FAO, Rome.
  # also: http://www.fao.org/3/X0490E/x0490e07.htm
  
  e_o <- function(temperature) {
    return(0.6108 * exp(17.27 * temperature / (temperature + 237.3)))
  }
  e_s = (e_o(temperature_max) + e_o(temperature_min)) / 2
  
  e_a = e_o(temperature_dew)
  
  # slope of the vapor pressure-temperature curve (kPa ºC−1)
  DELTA = 4098 * e_o(temperature) / (temperature + 237.3) ^ 2
  
  # atmospheric pressure (kPa)
  P = 101.3 * ((293 - 0.0065 * z) / 293) ^ 5.26
  # psychometric constant (kPa ºC−1)
  gamma = c_p * P / (epsilon * lambda) 
  
  if (method == 'PM')
  {
    # Penman-Monteith equation from: fao.org/3/X0490E/x0490e0 ; and from: weap21.org/WebHelp/Mabia_Alg ETRef.htm
    
    ETr <- (0.408 * DELTA * netSolarRadiation + gamma * (C_n / (temperature + 273)) * windSpeed * (e_s - e_a)) / (DELTA + gamma * (1 + C_d * windSpeed))
  }
  
  if (method == 'PT')
  {
    # alternatively, using Priestley-Taylor equation 
    # (Priestley and Taylor 1972, https://doi.org/10.1175/1520-0493(1972)100%3C0081:OTAOSH%3E2.3.CO;2)
    
    ETr <- (alpha / lambda) * (DELTA / (DELTA + gamma)) * netSolarRadiation
  }
  
  return(ETr)
  
  # useful references:
  # Suleiman A A and Hoogenboom G 2007 
  # Comparison of Priestley-Taylor and FAO-56 Penman-Monteith for Daily Reference Evapotranspiration Estimation in Georgia 
  # J. Irrig. Drain. Eng. 133 175–82 Online: http://ascelibrary.org/doi/10.1061/%28ASCE%290733-9437%282007%29133%3A2%28175%29
  # also: Jia et al. 2013 - doi:10.4172/2168-9768.1000112
  # constants found in: http://www.fao.org/3/X0490E/x0490e07.htm
  # see also r package: Evapotranspiration (consult source code)
}

#=======================================================================

# auxiliar functions

################################################################################
#' @title Get last item in vector
#' @param x : vector of any type
#' @export
getLastItemInVector <- function(x)
{
  return(x[length(x)])
}

################################################################################
#' @title Create vertical lines marking the end of years (time-series plot)
#' @description Calculate the incremental or differences curve corresponding to a given cumulative curve
#' @param lengthOfData : length of the x axis in the data
#' @param offset : scaling offset of each mark (helpful to adjust marks if barplot is used)
#' @param yearLengthInDays : the number of days per year
#' @return the incremental curve corresponding to the given cumulative curve (numeric vector)
#' @export
markEndYears <- function(lengthOfData, 
                         offset = 1,
                         yearLengthInDays = 365,
                         lty = 3
)
{
  for (i in 1:lengthOfData)
  {
    if (i %% (yearLengthInDays * offset) == 0)
    {
      abline(v = i, lty = lty)
    }
  }
}

#=======================================================================

# Model main procedures

################################################################################
#' @title Initialise the Weather model
#' @description Initialise the Weather model by creating a complex R object containing parameter values and empty holders for variables 
#' @param yearLengthInDays : the number of days per year
#' @param seed : the seed to be used the random number generator (numeric, integer)
#' @param albedo : albedo used in the calculation of reference evapotranspiration (numeric, range from 0 to 1)
#' @param southHemisphere : whether the annual sinusoidal curves (solar radiation and temperature) corresponds to values in the southern or the northern hemisphere; depending on this, winter and summer solstices, as the days with minimum and maximum values, are timed differently)
#' @param temperature_annualMaxAt2m,temperature_annualMinAt2m : annual maximum and minimum daily average temperature at 2m (ºC)
#' @param temperature_meanDailyFluctuation : standard deviation of the normal random noise of daily average temperature at 2m (ºC)
#' @param temperature_dailyLowerDeviation,temperature_dailyUpperDeviation : daily maximum (upper) and minimum (lower) deviation of temperature at 2m (ºC)
#' @param solar_annualMax,solar_annualMax : annual maximum and minimum daily solar radiation (MJ m2)
#' @param solar_meanDailyFluctuation : standard deviation of the normal random noise of daily solar radiation (MJ m2)
#' @param precip_yearlyMean : the annual sum of precipitation (mm)
#' @param precip_nSamples,precip_maxSampleSize : the number of random samples and the maximum length of samples used to escalonate the cumulative curve of annual precipitation
#' @param precip_plateauValue_yearlyMean,precip_plateauValue_yearlySd : the proportion of annual sum of precipitation (range of 0 to 1) in which the gap between logistic curves is set in the cumulative curve of annual precipitation (mean and standard deviation used for normal random sampling every year)
#' @param precip_inflection1_yearlyMean,precip_inflection1_yearlySd : the days of year in which the first logistic curve of the cumulative curve of annual precipitation have their maximum slope (mean and standard deviation used for normal random sampling every year)
#' @param precip_rate1_yearlyMean,precip_rate1_yearlySd : the maximum rate of the first logistic curve (mean and standard deviation used for normal random sampling every year)
#' @param precip_inflection2_yearlyMean,precip_inflection2_yearlySd : the days of year in which the second logistic curve of the cumulative curve of annual precipitation have their maximum slope (mean and standard deviation used for normal random sampling every year)
#' @param precip_rate2_yearlyMean,precip_rate2_yearlySd : the maximum rate of the second logistic curve (mean and standard deviation used for normal random sampling every year)
#' @return complex R object subdivided into PARS (named list of parameter values), annualPrecipitationPars (named list of parameter values used to generate daily precipitation, reset every year), and daily (named list of empty vectors to hold time-series variables) 
#' @export
weatherModel.init <- function(
  yearLengthInDays = 365,
  seed = 0,
  albedo = 0.4,
  southHemisphere = FALSE,
  temperature_annualMaxAt2m = 40,
  temperature_annualMinAt2m = 15,
  temperature_meanDailyFluctuation = 5,
  temperature_dailyLowerDeviation = 5,
  temperature_dailyUpperDeviation = 5,
  solar_annualMax = 7,
  solar_annualMin = 3,
  solar_meanDailyFluctuation = 1,
  precip_yearlyMean = 400,
  precip_yearlySd = 130,
  precip_nSamples = 200,
  precip_maxSampleSize = 10,
  precip_plateauValue_yearlyMean = 0.1,
  precip_plateauValue_yearlySd = 0.05,
  precip_inflection1_yearlyMean = 40,
  precip_inflection1_yearlySd = 20,
  precip_rate1_yearlyMean = 0.15,
  precip_rate1_yearlySd = 0.02,
  precip_inflection2_yearlyMean = 200, 
  precip_inflection2_yearlySd = 20,
  precip_rate2_yearlyMean = 0.05,
  precip_rate2_yearlySd = 0.01
)
{
  set.seed(seed)
  
  weatherModel <- list()
  
  weatherModel$PARS <- list(
    seed = seed,
    yearLengthInDays = yearLengthInDays,
    albedo = albedo,
    southHemisphere = southHemisphere,
    
    temperature = list(
      annualMaxAt2m = temperature_annualMaxAt2m,
      annualMinAt2m = temperature_annualMinAt2m,
      meanDailyFluctuation = temperature_meanDailyFluctuation,
      dailyLowerDeviation = temperature_dailyLowerDeviation,
      dailyUpperDeviation = temperature_dailyUpperDeviation
    ),
    
    solar = list(
      annualMax = solar_annualMax,
      annualMin = solar_annualMin,
      meanDailyFluctuation = solar_meanDailyFluctuation
    ),
    
    precipitation = list(
      yearlyMean = precip_yearlyMean,
      yearlySd = precip_yearlySd,
      nSamples = precip_nSamples,
      maxSampleSize = precip_maxSampleSize,
      plateauValue_yearlyMean = precip_plateauValue_yearlyMean,
      plateauValue_yearlySd = precip_plateauValue_yearlySd,
      inflection1_yearlyMean = precip_inflection1_yearlyMean,
      inflection1_yearlySd = precip_inflection1_yearlySd,
      rate1_yearlyMean = precip_rate1_yearlyMean,
      rate1_yearlySd = precip_rate1_yearlySd,
      inflection2_yearlyMean = precip_inflection2_yearlyMean, 
      inflection2_yearlySd = precip_inflection2_yearlySd,
      rate2_yearlyMean = precip_rate2_yearlyMean,
      rate2_yearlySd = precip_rate2_yearlySd
    )
  )
  
  weatherModel$annualPrecipitationPars <- list(
    annualSum = c(),
    plateauValue = c(),
    inflection1 = c(),
    rate1 = c(),
    inflection2 = c(),
    rate2 = c()
  )
  
  weatherModel$daily <- list(
    currentYear = c(),
    currentDayOfYear = c(),
    temperature = c(),
    temperature_max = c(),
    temperature_min = c(),
    solarRadiation = c(),
    ETr = c(),
    precipitation = c()
  )
  
  return(weatherModel)
}

################################################################################
#' @title Run the Weather model
#' @description Runs for the given number of years an initialised instance of weather model
#' @param weatherModel : initialised instance of weather model (object generated with weatherModel.init())
#' @param numberOfYears : number of years to be simulated
#' @return the instance of weather model containing the corresponding time-series recorded inside the "daily" partition
#' @export
weatherModel.run <- function(weatherModel, 
                             numberOfYears)
{
  for (year in 1:numberOfYears)
  {
    # set the precipitation parameters for the year
    
    weatherModel$annualPrecipitationPars$annualSum <- c(
      weatherModel$annualPrecipitationPars$annualSum,
      max(0, 
          rnorm(1, 
                weatherModel$PARS$precipitation$yearlyMean, 
                weatherModel$PARS$precipitation$yearlySd)
      )
    )
    
    weatherModel$annualPrecipitationPars$plateauValue <- c(
      weatherModel$annualPrecipitationPars$plateauValue,
      min(1, 
          max(0, 
              rnorm(1, 
                    weatherModel$PARS$precipitation$plateauValue_yearlyMean, 
                    weatherModel$PARS$precipitation$plateauValue_yearlySd)
          )
      )
    )
    
    weatherModel$annualPrecipitationPars$inflection1 <- c(
      weatherModel$annualPrecipitationPars$inflection1,
      min(weatherModel$PARS$yearLengthInDays, 
          max(1, 
              rnorm(1, 
                    weatherModel$PARS$precipitation$inflection1_yearlyMean, 
                    weatherModel$PARS$precipitation$inflection1_yearlySd)
          )
      )
    )
    
    weatherModel$annualPrecipitationPars$rate1 <- c(
      weatherModel$annualPrecipitationPars$rate1,
      max(0, 
          rnorm(1, 
                weatherModel$PARS$precipitation$rate1_yearlyMean, 
                weatherModel$PARS$precipitation$rate1_yearlySd)
      )
    )
      
    weatherModel$annualPrecipitationPars$inflection2 <- c(
      weatherModel$annualPrecipitationPars$inflection2,
      min(weatherModel$PARS$yearLengthInDays, 
          max(1, 
              rnorm(1, 
                    weatherModel$PARS$precipitation$inflection2_yearlyMean, 
                    weatherModel$PARS$precipitation$inflection2_yearlySd)
          )
      )
    )
    
    weatherModel$annualPrecipitationPars$rate2 <- c(
      weatherModel$annualPrecipitationPars$rate2,
      max(0, 
          rnorm(1, 
                weatherModel$PARS$precipitation$rate2_yearlyMean, 
                weatherModel$PARS$precipitation$rate2_yearlySd)
      )
    )
    
    # Generate annual precipitation
    
    weatherModel$daily$precipitation <- c(
      weatherModel$daily$precipitation,
      getPrecipitationOfYear(
        plateauValue = getLastItemInVector(weatherModel$annualPrecipitationPars$plateauValue),
        inflection1 = getLastItemInVector(weatherModel$annualPrecipitationPars$inflection1),
        rate1 = getLastItemInVector(weatherModel$annualPrecipitationPars$rate1),
        inflection2 = getLastItemInVector(weatherModel$annualPrecipitationPars$inflection2),
        rate2 = getLastItemInVector(weatherModel$annualPrecipitationPars$rate2), 
        yearLengthInDays = weatherModel$PARS$yearLengthInDays,
        nSamples = weatherModel$PARS$precipitation$nSamples,
        maxSampleSize = weatherModel$PARS$precipitation$maxSampleSize,
        annualSum = getLastItemInVector(weatherModel$annualPrecipitationPars$annualSum),
        seed = runif(1, 0, 2147483647)
      )
    )
    
    # generate other weather variables
    
    for (day in 1:weatherModel$PARS$yearLengthInDays)
    {
      weatherModel$daily$currentYear <- c(weatherModel$daily$currentYear, year)
      
      weatherModel$daily$currentDayOfYear <- c(weatherModel$daily$currentDayOfYear, day)
      
      weatherModel$daily$temperature <- c(
        weatherModel$daily$temperature,
        getDayValueInAnnualSinusoidWithFluctuation(
          minValue = weatherModel$PARS$temperature$annualMinAt2m, 
          maxValue = weatherModel$PARS$temperature$annualMaxAt2m, 
          fluctuation = weatherModel$PARS$temperature$meanDailyFluctuation, 
          dayOfYear = day, yearLengthInDays = weatherModel$PARS$yearLengthInDays,
          southHemisphere = weatherModel$PARS$southHemisphere,
          seed = runif(1, 0, 2147483647)
        )
      )
      
      weatherModel$daily$temperature_min <- c(
        weatherModel$daily$temperature_min,
        getLastItemInVector(weatherModel$daily$temperature) - weatherModel$PARS$temperature$dailyLowerDeviation
      )
      
      weatherModel$daily$temperature_max <- c(
        weatherModel$daily$temperature_max,
        getLastItemInVector(weatherModel$daily$temperature) + weatherModel$PARS$temperature$dailyUpperDeviation
      )
      
      weatherModel$daily$solarRadiation <- c(
        weatherModel$daily$solarRadiation,
        max(0, # solar radiation cannot be negative
            getDayValueInAnnualSinusoidWithFluctuation(
              minValue = weatherModel$PARS$solar$annualMin, 
              maxValue = weatherModel$PARS$solar$annualMax, 
              fluctuation = weatherModel$PARS$solar$meanDailyFluctuation, 
              dayOfYear = day, yearLengthInDays = weatherModel$PARS$yearLengthInDays,
              southHemisphere = weatherModel$PARS$southHemisphere,
              seed = runif(1, 0, 2147483647)
            )
        )
      )
      
      weatherModel$daily$ETr <- c(
        weatherModel$daily$ETr,
        estimateETr(
          R_s = getLastItemInVector(weatherModel$daily$solarRadiation), 
          temperature = getLastItemInVector(weatherModel$daily$temperature), 
          temperature_max = getLastItemInVector(weatherModel$daily$temperature_max), 
          temperature_min = getLastItemInVector(weatherModel$daily$temperature_min)
        )
      )
    }
  }
  
  return(weatherModel)
}
