################################################################################
# "Working with dynamic models for agriculture"
# R script for practical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA), Sylvain Toulet (INRA, internship 2012)
# version : 2019-10-30 by Andreas Angourakis (using version from 2012-04-23)
# Model described in the book, Appendix. Models used as illustrative examples: description and R code
################################ FUNCTIONS #####################################
################################################################################
#' @title WaterBalance model - calculate change in soil water for one day
#' @description WaterBalance model - calculate change in soil water for one day
#' @param modelVars : A dataframe containing the daily series for a year of Rainfall of day (mm) or RAIN, Evapotranspiration of day (mm) or ETr, water at the beginning of the day (mm) or WAT, and the ARID drought index.
#' @param lastDay : the day from which variables will be calculated (there must be a slot in modelVars for lastDay + 1).
#' @param param : a vector of parameters
#' @param FC : Water content at field capacity (cm^3.cm^-3)
#' @param WP : Water content at wilting Point (cm^3.cm^-3)
#' @param typeOfParValue : type of parameter value: (nominal, binf, bsup)
#' @export
watbal.update = function(modelVars, lastDay, param, WP, FC, typeOfParValue){
  WHC =(param[typeOfParValue, "WHC"])
  MUF = (param[typeOfParValue, "MUF"])
  DC = (param[typeOfParValue, "DC"])
  z = (param[typeOfParValue, "z"])
  CN = (param[typeOfParValue, "CN"])
  # Maximum abstraction (for run off)
  S = 25400 / CN - 254
  # Initial Abstraction (for run off)
  IA = 0.2 * S
  # WATfc : Maximum Water content at field capacity (mm)
  WATfc = FC * z
  # WATwp : Water content at wilting Point (mm)
  WATwp = WP * z
  
  # get lastDay's values
  RAIN = modelVars$RAIN[lastDay]
  ETr = modelVars$ETr[lastDay]
  WAT = modelVars$WAT[lastDay]

  # Change in Water Before Drainage (Precipitation - Runoff)
  if (RAIN > IA){RO = (RAIN - 0.2 * S)^2 / (RAIN + 0.8 * S)} else {RO = 0}
  # Calculating the amount of deep drainage
  if (WAT + RAIN - RO > WATfc){DR = DC * (WAT + RAIN - RO - WATfc)} else {DR = 0}
  
  # Calculate rate of change of state variable WAT
  # Compute maximum water uptake by plant roots on a day, RWUM
  RWUM = MUF * (WAT + RAIN - RO - DR - WATwp)
  # Calculate the amount of water lost by transpiration (TR)
  TR = min(RWUM, ETr)
  
  dWAT = RAIN - RO -DR -TR
  modelVars$WAT[lastDay + 1] = WAT + dWAT

  # compute the ARID index. Note that it is an auxilliary variable, not a "state variable" as is WAT[day]
  if (TR < ETr) {modelVars$ARID[lastDay + 1] = 1 - TR / ETr}
  else {modelVars$ARID[lastDay + 1] = 0.0}
  
  return(modelVars)
}

################################################################################
#' @title WaterBalance model - calculate soil water over designated time period
#' @description WaterBalance model - calculate soil water over designated time period
#' @param param : a vector of parameters
#' @param weather : weather data.frame for one single year
#' @param WP : Water content at wilting Point (cm^3.cm^-3)
#' @param FC : Water content at field capacity (cm^3.cm^-3)
#' @param WAT0 : Initial Water content (mm). If NA WAT0=z*FC
#' @return data.frame with daily RAIN, ETR, Water at the beginning of the day (absolute : WAT, mm and relative value : WATp, -), and ARID coefficient
#' @export
watbal.model = function(param, weather, WP, FC, WAT0=NA, typeOfParValue = "nominal")
{
  z = (param[typeOfParValue, "z"])
  # input variable describing the soil
  # WP : Water content at wilting Point (cm^3.cm^-3)
  # FC : Water content at field capacity (cm^3.cm^-3)
  # WAT0 : Initial Water content (mm)
  if (is.na(WAT0)) {WAT0 = z*FC}
  
  # Initialize variable
  modelVars <- data.frame(
    year = weather$year,
    day = weather$day, 
    RAIN = weather$RAIN, 
    ETr = weather$ETr, 
    # WAT : Water at the beginning of the day (mm) : State variable
    WAT = rep(NA, nrow(weather)), 
    WATp = rep(NA, nrow(weather)), 
    # supplementary variable ARID drought index. 
    # computed as the ratio of transpiration to potential transpiration. (See Woli, 2010)        
    # A value of ARID = 0 means that there is no water stress in the crop; a value of ARID=1 means a maximum stress with no growth
    ARID = rep(NA, nrow(weather)))
  
  # initialisation use Amount of water at the beginning
  modelVars$WAT[1] = WAT0
  modelVars$ARID[1] = NA

  # integration loops
  for (day in 1:(nrow(modelVars) - 1))
  {
    modelVars <- watbal.update(modelVars, day, param, WP, FC, typeOfParValue)
  }
  
  # Volumetric Soil Water content (fraction : mm.mm-1)
  modelVars$WATp = modelVars$WAT / z
  return(modelVars);
}

################################################################################
#' @title Define values of the parameters for the WaterBalance model
#' @description Define values of the parameters for the WaterBalance model
#' @return matrix with parameter values (nominal, binf, bsup)
#' @export
watbal.define.param = function()
{
  # nominal, binf, bsup
  # WHC  : Water Holding Capacity of the soil (cm^3 cm^-3)
  WHC = c(0.13, 0.05, 0.18);
  # MUF : Water Uptake coefficient (mm^3 mm^-3)
  MUF = c(0.096, 0.06, 0.11);
  # DC :  Drainage coefficient (mm^3 mm^-3)
  DC = c(0.55, 0.25, 0.75);
  # z : root zone depth (mm)
  z = c(400, 300, 600);
  # CN : Runoff curve number
  CN = c(65, 15, 90); # nominal = 58 in the description ??
  
  param = data.frame(WHC, MUF, DC, z, CN);
  row.names(param) = c("nominal","binf","bsup");
  param = as.matrix(param)
  attributes(param)$description=t(t(c("WHC"="Water Holding Capacity of the soil (cm3.cm-3)",
                                      "MUF" = "Water Uptake coefficient (mm^3 mm^-3)", "DC" = "Drainage coefficient (mm3.mm-3)",
                                      "z" = "root zone depth (mm)",  "CN" = "Runoff curve number")))
  return(param)
}

################################################################################
#' @title Read weather data for the WaterBalance model (West of France Weather)
#' @description Read weather data for the WaterBalance model (West of France Weather)
#' @param working.year : year for the subset of weather data (default=NA : all the year)
#' @param working.site : site for the subset of weather data (default=NA : all the site)
#' @return data.frame with daily weather data for one or several site(s) and for one or several year(s)
#' @export
# Reading Weather data function
watbal.weather = function(working.year=NULL, working.site=NULL)
{
  #day month year R Tmax Tmin rain ETP
  # R : solar radiation (MJ)
  # Tmax : maximum temperature (degC)
  # Tmin : minimum temperature (degC)
  weather=ZeBook::weather_FranceWest
  names(weather)[names(weather)=="WEDAY"]= "day"
  names(weather)[names(weather)=="WEYR"]= "year"
  names(weather)[names(weather)=="SRAD"]= "I"
  names(weather)[names(weather)=="TMAX"]= "Tmax"
  names(weather)[names(weather)=="TMIN"]= "Tmin"
  names(weather)[names(weather)=="RAIN"]= "RAIN"
  names(weather)[names(weather)=="ETr"]= "ETr"
  # if argument working.year/working.site is specified, work on one particular year/site
  if (!is.null(working.year)&!is.null(working.site)) {weather=weather[(weather$year==working.year)&(weather$idsite==working.site),] }
  else{
    if (!is.null(working.year)) {weather<-weather[(weather$year==working.year),]}
    if (!is.null(working.site)) {weather<-weather[(weather$idsite==working.site),]}}
  return (weather)
}

################################################################################
#' @title Read weather data for the WaterBalance model (NASA POWER csv download)
#' @description Read weather data for the WaterBalance model (NASA POWER csv download)
#' @param fileName : name of the file containing the weather data dowloaded from NASA power (power.larc.nasa.gov)
#' @param year : selected year (NULL = all)
#' @return data.frame with daily weather data extracted from file, plus estimated ETr
#' @export
# Reading Weather data function
watbal.weather.file = function(fileName, year = NULL)
{
  #day month year R Tmax Tmin rain ETP
  # R : solar radiation (MJ)
  # Tmax : maximum temperature (degC)
  # Tmin : minimum temperature (degC)
  # assuming file downloaded in NASA POWER (power.larc.nasa.gov/data-access-viewer/)
  weather <- read.csv(fileName, skip = 17) 
  
  
  names(weather)[names(weather)=="DOY"]= "day"
  names(weather)[names(weather)=="YEAR"]= "year"
  names(weather)[names(weather)=="ALLSKY_SFC_SW_DWN"]= "I"
  names(weather)[names(weather)=="T2M_MAX"]= "Tmax"
  names(weather)[names(weather)=="T2M_MIN"]= "Tmin"
  names(weather)[names(weather)=="PRECTOT"]= "RAIN"
  
  # if argument working.year/working.site is specified, work on one particular year/site
  if (!is.null(year)) {
    weather=weather[weather$year %in% year,] }
  return (weather)
}

################################################################################
#' @title WaterBalance model - Variant with another order of calculation and ARID index
#' @description WaterBalance model - Variant with another order of calculation and ARID index
#' @param WHC : Water Holding Capacity of the soil (cm^3 cm^-3)
#' @param MUF : Water Uptake coefficient (mm^3 mm^-3)
#' @param DC : Drainage coefficient (mm^3 mm^-3)
#' @param z : root zone depth (mm)
#' @param CN : Runoff curve number
#' @param weather : weather data.frame for one single year
#' @param WP : Water content at wilting Point (cm^3.cm^-3)
#' @param FC : Water content at field capacity (cm^3.cm^-3)
#' @param WAT0 : Initial Water content (mm). If NA WAT0=z*FC
#' @return data.frame with daily RAIN, ETR, Water at the beginning of the day (absolute : WAT, mm and relative value : WATp, -)
#' @export
watbal.model.arid = function(WHC, MUF, DC, z, CN, weather, WP, FC, WAT0=NA)
{
  #WHC :Water Holding Capacity of the soil (cm3.cm-3)
  #MUF :Water Uptake coefficient (mm^3 mm^-3)        
  #DC :Drainage coefficient (mm3.mm-3)              
  #z :root zone depth (mm)                         
  #CN :Runoff curve number 
  
  # Maximum abstraction (for run off)
  S = 25400/CN-254
  # Initial Abstraction (for run off)
  IA = 0.2*S
  # WATfc : Maximum Water content at field capacity (mm)
  WATfc = FC*z
  # WATwp : Water content at wilting Point (mm)
  WATwp = WP*z
  
  # input variable describing the soil
  # WP : Water content at wilting Point (cm^3.cm^-3)
  # FC : Water content at field capacity (cm^3.cm^-3)
  # WAT0 : Initial Water content (mm)
  if (is.na(WAT0)) {WAT0 = z*FC}
  # Initialize variable
  # WAT : Water at the beginning of the day (mm) : State variable
  WAT = rep(NA, nrow(weather))
  
  # supplementary variable ARID drought index. 
  # computed as the ratio of transpiration to potential transpiration. (See Woli, 2010)        
  # A value of ARID = 0 means that there is no water stress in the crop; a value of ARID=1 means a maximum stress with no growth
  ARID = rep(NA, nrow(weather))
  
  # initialisation-use Amount of water at the beginning
  WAT[1]=WAT0
  ARID[1] = NA
  # integration loops
  for (day in 1:(nrow(weather)-1))
  {
    # Calculate rate of change of state variable WAT
    # Compute maximum water uptake by plant roots on a day, RWUM
    RWUM = MUF*(WAT[day]-WATwp)
    # Calculate the amount of water lost by transpiration (TR)-prior to RAIN, RO, and DR 
    TR = min(RWUM, weather$ETr[day])       
    
    # Compute Surface Runoff (RO)
    if (weather$RAIN[day]>IA){RO = (weather$RAIN[day]-0.2*S)^2/(weather$RAIN[day]+0.8*S)}else{RO = 0}
    # Calculate the amount of deep drainage (DR)
    if (WAT[day]+weather$RAIN[day]-RO > WATfc){DR = DC*(WAT[day]+weather$RAIN[day]-RO-WATfc)}else{DR = 0}
    
    # Update state variables 
    dWAT = weather$RAIN[day] - RO -DR -TR
    WAT[day+1] = WAT[day] + dWAT
    
    # compute the ARID index. Note that it is an auxilliary variable, not a "state variable" as is WAT[day]
    if (TR < weather$ETr[day])   {ARID[day+1] = 1 - TR/weather$ETr[day]}      else    {ARID[day+1] = 0.0}
    
  }
  
  # Volumetric Soil Water content (fraction : mm.mm-1)
  WATp=WAT/z
  return(data.frame(day = weather$day, RAIN = weather$RAIN, ETr = weather$ETr, WAT = WAT, WATp=WATp, ARID=ARID));
}

# End of file
