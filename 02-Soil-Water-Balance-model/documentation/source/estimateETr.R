################################################################################
#' @title Estimate daily reference evapotranspiration (ETr)
#' @description Estimates daily reference evapotranspiration using either Penman-Monteith equation (default) or Priestley-Taylor equation.
#' @param R_s : solar radiation or insolation (MJ m2 day-1)
#' @param Temp : daily average temperature at 2m (ºC)
#' @param maxTemp : daily maximum temperature at 2m (ºC)
#' @param minTemp : daily minimum temperature at 2m (ºC)
#' @param dewTemp : daily dew point temperature at 2m (ºC)
#' @param windSpeed : mean daily wind speed at 2m (m s-1)
#' @param method : "PM" to use Penman-Monteith equation; "PT" to use Priestley-Taylor equation
#' @param z : elevation above sea level (m)
#' @return vector with daily ETr (mm day-1)
#' @export
estimateETr <- function(R_s, Temp,  maxTemp, minTemp, dewTemp = NULL, windSpeed = NULL, method = "PM", z = 200)
{
  ETr <- NULL
  
  # estimate the net solar radiation, 
  # assuming canopy reflection or albedo of hypothetical grass reference crop (0.23)
  R_n <- (1 - 0.23) * R_s
  
  if (is.null(windSpeed)) 
  {
    windSpeed = 2 
    # as recommended by 
    # http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data
  }
  
  # estimation of saturated vapour pressure (e_s) and actual vapour pressure (e_a)
  # according to 
  # Allen, R. G., Pereira, L. A., Raes, D., and Smith, M. 1998. 
  # “Crop evapotranspiration.”FAO irrigation and  drainage paper 56, FAO, Rome.
  # also: http://www.fao.org/3/X0490E/x0490e07.htm
  
  e_o <- function(temperature) {
    return(0.6108 * exp(17.27 * temperature / (temperature + 237.3)))
  }
  e_s = (e_o(maxTemp) + e_o(minTemp)) / 2
  
  if (is.null(dewTemp))
  {
    dewTemp <- minTemp
    # as recommended by 
    # http://www.fao.org/3/X0490E/x0490e07.htm#estimating%20missing%20climatic%20data
    # however, possibly minTemp > dewTemp under arid conditions
  }
  e_a = e_o(dewTemp)
  
  
  # slope of  the  vapor  pressure-temperature  curve (kPa ºC−1)
  DELTA = 4098 * e_o(Temp) / (Temp + 237.3) ^ 2
  
  # useful references:
  # Suleiman A A and Hoogenboom G 2007 
  # Comparison of Priestley-Taylor and FAO-56 Penman-Monteith for Daily Reference Evapotranspiration Estimation in Georgia 
  # J. Irrig. Drain. Eng. 133 175–82 Online: http://ascelibrary.org/doi/10.1061/%28ASCE%290733-9437%282007%29133%3A2%28175%29
  # also: Jia et al. 2013 - doi:10.4172/2168-9768.1000112
  # constants found in: http://www.fao.org/3/X0490E/x0490e07.htm
  # see also r package: Evapotranspiration (consult source code)
  
  # latent heat of vaporisation = 2.45 MJ.kg^-1
  lambda = 2.45
  
  # specific heat at constant pressure, 1.013 10-3 [MJ kg-1 °C-1]
  c_p = 1.013 * 10^-3
  # ratio molecular weight of water vapour/dry air
  epsilon = 0.622
  # atmospheric pressure (kPa); z = elevation above sea level [m]
  P = 101.3 * ((293 - 0.0065 * z) / 293) ^ 5.26
  # psychometric constant (kPa ºC−1)
  gamma = c_p * P / (epsilon * lambda) 
  
  if (method == "PM")
  {
    # Penman-Monteith equation from: fao.org/3/X0490E/x0490e0 ; and from: weap21.org/WebHelp/Mabia_Alg ETRef.htm
    
    # 900 and 0.34 for the grass reference; 1600 and 0.38 for the alfalfa reference
    C_n = 900
    C_d = 0.34
    
    ETr <- (0.408 * DELTA * R_n + gamma * (C_n / (Temp + 273)) * windSpeed * (e_s - e_a)) / (DELTA + gamma * (1 + C_d * windSpeed))
  }
  
  if (method == "PT")
  {
    # alternatively, using Priestley-Taylor equation 
    # (Priestley and Taylor 1972, https://doi.org/10.1175/1520-0493(1972)100%3C0081:OTAOSH%3E2.3.CO;2)
    
    # Priestley-Taylor coefficient
    alpha = 1.26
    
    ETr <- (alpha / lambda) * (DELTA / (DELTA + gamma)) * R_n
  }
  
  return(ETr)
}
