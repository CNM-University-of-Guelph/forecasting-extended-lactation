# A function to take fitted MY data and calculate the probability that the
# predicted MY at d 305 is >= certain threshold values.
#
# David Innes
#
# Uses the t statistic which in this case is calculated as:
# t_stat = (X_305-P_305)/s_pred
#
# where s_pred is substituted for RMSPE (as the measure of standard error) adjusted for n-4 degrees of freedom due to having 4 params in the model,
# X_305 = Target Threshold of MY at 305 d
# P_305 = predicted MY at 305 d (mean 303 to 307 d)
#
# Then probability
# Prob(P_305 >= X_305) = 1 - [ T ~ t(n-4) ]
#
# Targets tested: 10, 15, 20, 25, 30 kg
#
#
# Requires: f_goodness_of_fit()
#
# Input:
#   predicted_MY = df from a call to augment() on nls object. cols = ID, DIM, my, .fitted, .resid
#                 Should have the same dimensions as original data (i.e. not forecasts)
#   forecasted_305d_MY = df of mean fitted values for 305 DIM.  cols = ID, mean_fitted_305d_MY
#   n_params = int. number of parameters used when model was fit
#
# Output:
# a list of 2 df:
#   df_P_wide_out: probability for each ID across Targets: 10, 15, 20, 25, 30 kg
#   df_probability_full: all columns used for calculating P values.
#
f_forecast_P <- function(predicted_MY, forecasted_305d_MY, n_params = 4){


  # Calculate goodness-of-fit and prepare the rMSPE_adj for s_pred
  individual_fit_stats <-
    predicted_MY %>%
    group_by(ID) %>%
    nest() %>%
    mutate(fit_stats = purrr::map(data,
                                  ~ f_goodness_of_fit(.x$my, .x$.fitted, n_params = n_params))) %>%
    unnest(fit_stats) %>%
    select(!where(is.list))

  df_RMSPE_adj <- individual_fit_stats %>% select(ID, RMSPE_adj, n)


  # Calculate t_test
  df_probability_full <-
    df_RMSPE_adj %>%
    left_join(forecasted_305d_MY) %>%
    mutate( X_305 = list(c(10, 15, 20, 25, 30))) %>%
    group_by(ID) %>%
    unnest(X_305) %>%
    mutate(t_stat = (X_305 - mean_fitted_305d_MY)/RMSPE_adj,
           P = 1- pt(t_stat, df = n-4))

  df_P_wide_out <-
    df_probability_full %>%
    select(ID, X_305, P) %>%
    pivot_wider(names_from = X_305,
                values_from = P)

  return(list(
    df_P_wide_out = df_P_wide_out ,
    df_probability_full = df_probability_full
  ))


  }


