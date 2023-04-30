#' Almond Profit Function
#'
#' Compute profit per acre of almond farm using Lobell et al. 2006 model
#' @param climate_data time series data frame consisting of daily data with requisite
#' columns day(1-31), month(1-12), year(YYYY), water year(YYYY),
#' Minimum daily temperature in Celsius (tmin_c), daily precipitation in millimeters (precip)
#' @param almond_price the price of almonds per ton. $4036 is the average price for May 2022
#' @param average_yield ton of almonds yielded per farm. Average yielded per farm is 50 tons
#' @return profit in dollars per acre of almond farm
#' @examples
#' almond_profit(timeseries_climate_dataframe)
#' 
almond_profit <- function(climate_data
                          , almond_price = 4036
                          , average_yield = 50) {
  
  ## Error condition to ensure that all inputs are numeric 
  if (!is.numeric(c(almond_price
                    , average_yield))) {
    
    errorCondition("Inputs must be numeric.")
    
  } else {
    
    source(here('almond_yield_response.R'))
    
    mean_anamoly <- almond_yield_response(climate_data)$almond_yield_anomaly
    
    profit <- (average_yield * mean_anamoly * almond_price)
    
    return(profit)
  }
}
