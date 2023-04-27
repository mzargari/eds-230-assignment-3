#' Almond Profit Function
#'
#' Compute profit per acre of almond farm using Lobell et al. 2006 model
#' @param almond_price
#' @param average_yield
#' @return profit in dollars per acre of almond farm
#' @examples
#' almond_profit(timeseries_climate_dataframe)
#' 
almond_profit <- function(climate_data, almond_price = 4036, average_yield = 50) {
  
  ## Error condition to ensure that all inputs are numeric 
  if (!is.numeric(c(almond_price
                    , average_yield))) {
    
    errorCondition("Inputs must be numeric.")
    
  } else {
    
    source(here('almond_yield_response.R'))
    
    mean_anamoly <- almond_yield_response(climate_data)$mean_anomaly
    
    profit <- (average_yield * mean_anamoly * almond_price)
    
    return(profit)
  }
}
