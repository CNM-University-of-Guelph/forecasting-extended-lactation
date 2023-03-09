# Function for calculating goodness of fit statistics
# David Innes


# requires car package (for Durbin Watson calc)

# INPUT:
#  - actual_vec: vector of numbers representing actual observed values
#  - fitted_vec: vector of numbers representing fitted predicted values
#  - n_params: number of parameters in the model used to get the fitted_vec. This is used for adjusted-R2 calculation.

# OUTPUT:
#  - A dataframe of statistics calculated, see list at end of script for details.


f_goodness_of_fit <- function(actual_vec, fitted_vec, n_params){
  # n_params = number of parameters in the model. Used for calculating adjusted-R2. E.g. use 4 for Dijkstra model

  if(!length(actual_vec) == length(fitted_vec)){
    stop("Input vectors not of equal size.")
  }

  #population SD function: https://stackoverflow.com/a/66349386
  .sd.p <- function(x){sd(x) * sqrt((length(x) - 1) / length(x))}

  # calculate residual (as observed - predicted)
  .resid <- actual_vec - fitted_vec

  # calculate number of observations
  n <- length(.resid)

  actual_mean <- mean(actual_vec)
  actual_sd.p <- .sd.p(actual_vec)

  fitted_mean <- mean(fitted_vec)
  fitted_sd.p <- .sd.p(fitted_vec)


  #########################
  # CCC - Concordance Correlation Coefficient
  # based on Lin 1989
  #########################
  v  <-  actual_sd.p / fitted_sd.p

  u <- (actual_mean - fitted_mean) / sqrt(actual_sd.p * fitted_sd.p)

  Cb <- 2/(v+(1/v)+u^2)

  r <- cor(actual_vec, fitted_vec, method = 'pearson')

  CCC <- r * Cb

  #########################
  # RMSE(%)
  #########################

  RSS <- sum(.resid^2) #Residual sum of squares

  MSPE <- RSS/n # RSS/ number of observations

  rMSPE <- sqrt(MSPE) # root mean square prediction error

  RMSE <- rMSPE / actual_mean * 100 # root mean square error (as % of observed mean)


  # add adjustment for n-number of params
  MSPE_adj <- RSS/(n-n_params) # RSS/ number of observations

  rMSPE_adj <- sqrt(MSPE_adj) # root mean square prediction error


  #########################
  # MAE - Mean Absolute Error
  #########################

  # absolute (e.g. kg/d)
  MAE <- sum(abs(.resid))/n # MAE =  ∑|Obs-Pred| / n (number of obs)

  MAE_adj <- sum(abs(.resid))/(n-n_params)

  # as percent of mean observed
  MAE_perc <- MAE / actual_mean * 100

  #########################
  # Decomposition of error
  # Bibby and Toutenburg (1997).
  #########################
  mean_bias <- (fitted_mean - actual_mean)^2

  slope_bias <- (fitted_sd.p - r * actual_sd.p)^2

  random_error <- (1 - r^2) * actual_sd.p^2

  # As a % of MSPE (as typically reported)
  mean_bias_perc_MSPE <- mean_bias / MSPE * 100

  slope_bias_perc_MSPE <- slope_bias / MSPE * 100

  random_error_perc_MSPE <- random_error / MSPE * 100

  #########################
  # R^2 and adjusted R^2
  #########################
  TSS <- sum((actual_vec - actual_mean)^2)

  R2 <- 1-(RSS/TSS)

  R2_adj <-  1 - ((n - 1)) / ((n - n_params))  * (1 - R2)


  #########################
  # Durbin Watson test for autocorrelation
  #########################

  DW <- car::durbinWatsonTest(.resid)


  #########################
  # OUTPUT
  #########################
  df_out <-
    list(
      n = n,
      obs_mean = actual_mean,
      pred_mean = fitted_mean,
      R2_adj = R2_adj,
      RMSPE = rMSPE,
      RMSE_perc = RMSE,
      MAE = MAE,
      MAE_perc = MAE_perc,
      RMSPE_adj = rMSPE_adj,
      MAE_adj = MAE_adj,
      DW = DW,
      CCC = CCC,
      Cb = Cb,
      r = r,
      mean_bias = mean_bias,
      slope_bias = slope_bias,
      random_error = random_error,
      mean_bias_perc_MSPE = mean_bias_perc_MSPE,
      slope_bias_perc_MSPE = slope_bias_perc_MSPE,
      random_error_perc_MSPE = random_error_perc_MSPE
      ) %>%
    as.data.frame()

  return(df_out)

}
