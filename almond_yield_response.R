#'  Min, Mean, Max Almond Yield Anomaly  
#'
#'Compute Min, Mean, Max Almond Yield Anomaly using Lobell et al. 2006 model
#' @param climate_data time series data frame consisting of daily data with requisite 
#' columns day(1-31), month(1-12), year(YYYY), water year(YYYY), 
#' Minimum daily temperature in Celsius (tmin_c), daily precipitation in millimeters (precip)
#' @param min_temp_coeff_1 Minimum temperature coefficient 1
#' @param min_temp_coeff_2 Minimum temperature coefficient 2
#' @param precip_coeff_1 Precipitation Coefficient 1
#' @param precip_coeff_2 Precipitation Coefficient 2
#' @param intercept 
#' @return anomaly_values - a data frame with min, mean, max yield anomaly for 
#'  input time series in ton acre^-1
#' @examples
#' almond_yield_response <- function(timeseries_climate_dataframe)
#' 
almond_yield_response <- function(climate_data
                                  , min_temp_coeff_1 = 0.015
                                  , min_temp_coeff_2 = 0.0046
                                  , precip_coeff_1 = 0.07
                                  , precip_coeff_2 = 0.0043
                                  , intercept = 0.28) {
  
  ## Error condition to ensure that all inputs are numeric 
  if (!is.numeric(c(min_temp_coeff_1
                    , min_temp_coeff_2
                    , precip_coeff_1
                    , precip_coeff_2
                    , intercept))) {
    
    errorCondition("Inputs must be numeric.")
    } else {
    
    # extract January precipitation values for each water year
    climate_var_precip <- climate_data %>% 
      filter(month == 1) %>%
      group_by(month, wy) %>%
      summarize(precip_sum = sum(precip, na.rm = TRUE)) 
    
    # extract February temperature values for each water year
    climate_var_temp <- climate_data %>%
      filter(month == 2) %>%
      group_by(month, wy) %>%
      summarize(temp_min = min(tmin_c, na.rm= TRUE)) 
    
    # join climate variables by water year
    yearly_climate_vars <- left_join(climate_var_precip,
                                     climate_var_temp, by = 'wy') %>%
      select(wy, precip_sum, temp_min)
    
    # calculate almond yield anomaly for each water year
    yield_model <- yearly_climate_vars  %>%
      mutate(almond_yield_anomaly = -(min_temp_coeff_1 * temp_min) - (min_temp_coeff_2 * (temp_min)^2) - (precip_coeff_1 * precip_sum) + (precip_coeff_2 * (precip_sum)^2) + intercept)
    
    # extract min, mean, max almond yield anomaly for entire time series
    anomaly_values <-  tibble(
      min_anomaly = min(yield_model$almond_yield_anomaly),
      max_anomaly = max(yield_model$almond_yield_anomaly),
      mean_anomaly = mean(yield_model$almond_yield_anomaly))
    
    return(anomaly_values)
  }
}