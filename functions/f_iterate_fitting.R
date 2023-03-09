# Function to fit Dijkstra model with varying DIM threshold
# David Innes

# Input:
# - cleaned_data_in: df. output from f_clean_lac_data()
# - DIM_threshold: number representing the DIM to filter data with. i.e. DIM <= DIM_threshold.
# Bounds for parameters. Based on mean+SD of each.

# Output:
# a list of:
# - fit_info_out
# - params_out
# - params_wide
# - preds_out
# - individual_fit_stats
# - fitted_MY_all = df of fitted MY for all DIM in cleaned_data_in
# - fitted_305d_MY
# - P_wide = P values, wide format
# - P_full_calc = df of full calcs of P values

f_iterate_fitting <- function(cleaned_data_in,
                              DIM_threshold,
                              b_lower_bound = 0,
                              b_upper_bound = 1,
                              b0_lower_bound = 0,
                              b0_upper_bound = 1,
                              c_lower_bound = 0,
                              c_upper_bound = 1
                              ){


  print(paste("Starting DIM_threshold =", DIM_threshold))
  print(Sys.time())
  print("Preparing Data...")

  # #########################
  # Define Bounds & copy to each node
  # force it so that param can't be literally 0, as it makes it impossible to calculate Tp or PY

  if(b_lower_bound == 0) {b_lower_bound <- b_lower_bound+0.000000001
  } else {  b_lower_bound <- round(b_lower_bound, 5)
  }

  b_upper_bound <- round(b_upper_bound, 5)

  if(b0_lower_bound == 0) { b0_lower_bound <- b0_lower_bound+0.000000001
  } else {  b0_lower_bound <- round(b0_lower_bound, 5)  }

  b0_upper_bound <- round(b0_upper_bound, 5)

  if(c_lower_bound == 0) { c_lower_bound <- c_lower_bound+0.000000001
  } else { c_lower_bound <- round(c_lower_bound, 5)   }

  c_upper_bound <- round(c_upper_bound, 5)


  multidplyr::cluster_copy(cluster,
                           c("b_upper_bound", "b_lower_bound",
                             "b0_upper_bound", "b0_lower_bound",
                             "c_upper_bound", "c_lower_bound"))

  print(paste0("Using bounds on b param: ", b_lower_bound, " to ", b_upper_bound))
  print(paste0("Using bounds on b0 param: ", b0_lower_bound, " to ", b0_upper_bound))
  print(paste0("Using bounds on c param: ", c_lower_bound, " to ", c_upper_bound))

  ###########################
  # subset data
  current_subset <-
    cleaned_data_in %>%
    filter(DIM <= DIM_threshold)


  # Partition data for fitting across cores
  partitioned_data <-  current_subset %>%
    group_by(ID) %>%
    nest() %>%
    partition(cluster)

  print("Preparing Data...END")
  ###########################
  # Dijkstra
  ###########################

  print("Fitting DIJKSTRA...")
  print(Sys.time())
  # run nls_multstart with shotgun approach
  # this function uses the marquardt method
  # fit over each set of groupings
  # Uses a nested dataframe with map, which iterates through each set of data and returns it in a df.
  fits_dijkstra <- partitioned_data %>%
    mutate(fit = purrr::map(data,
                            ~ nls_multstart(my ~ dijkstra_eq(a,b,b0,c, t=DIM),
                                            data = .x,
                                            iter = 500,
                                            start_lower = c(a = 5, b = 0.01, b0 = 0.01, c = 0.001),
                                            start_upper = c(a = 35, b = 0.09, b0 = 0.09, c = 0.005),
                                            supp_errors = 'N',
                                            convergence_count = 100,
                                            na.action = na.omit,
                                            lower = c(a = 0, b = b_lower_bound, b0 = b0_lower_bound, c = c_lower_bound),
                                            upper = c(a = Inf, b = b_upper_bound, b0 = b0_upper_bound, c = c_upper_bound)
                            ))) %>%
    collect()

  print(fits_dijkstra)

  # get summary
  info_dijkstra <- fits_dijkstra %>%
    mutate(summary = map(fit, glance)) %>%
    unnest(summary) %>%
    select(-where(is.list))

  # get params
  params_dijkstra <- fits_dijkstra %>%
    mutate(p = map(fit, tidy)) %>%
    unnest(p) %>%
    select(-where(is.list))


  # get predictions
  preds_dijkstra <- fits_dijkstra %>%
    mutate(p = map(fit, augment)) %>%
    unnest(p) %>%
    select(-where(is.list))


  # calculate peak yield and time to peak yield
  PY_TP_dijkstra <-
    params_dijkstra %>%
    select(ID, term, estimate) %>%
    pivot_wider(names_from = term, values_from=estimate) %>%
    group_by(ID) %>%
    mutate(Tp = dijkstra_time_PY_eq(b, c, b0),
           Mp =  dijkstra_eq(a, b, b0, c, t = Tp))


  individual_fit_stats_dijkstra <-
    preds_dijkstra %>%
    nest() %>%
    mutate(fit_stats = purrr::map(data,
                                  ~ f_goodness_of_fit(.x$my, .x$.fitted, n_params = 4))) %>%
    unnest(fit_stats) %>%
    select(!where(is.list))


  #############################
  # FORECASTING
  # Calculate fitted values for all DIM
  params_wide_dijkstra <-
    params_dijkstra %>%
    select(ID, term, estimate) %>%
    pivot_wider(names_from = term, values_from=estimate)

  # fits data from day 0 to max DIM for all lactations (to cover all possible date ranges for plotting)
  complete_template <-
    cleaned_data_in %>%
    group_by(ID) %>%
    summarise(to = max(DIM)) %>%
    mutate(DIM = map(to, ~seq(1,to=.x,by=1))) %>%
    unnest(DIM) %>%
    select(-to) %>%
    ungroup()

  fitted_values <-
    params_wide_dijkstra %>%
    left_join(complete_template) %>%
    mutate(fitted_MY =  dijkstra_eq(a,b,b0,c,DIM)) %>%
    select(-a,-b,-b0,-c) %>%
    # add on my column (observed my) for plotting downstream
    left_join(
      cleaned_data_in %>% select(DIM, ID, my)
    )

  #############################
  # FORECASTED PERSISTENCY
  # Time to maximum DIM
  # Set MAX DIM to 305 to standardise the forecasted values
  #
  t_max_MY_dijkstra <-
    fitted_values %>%
    filter(DIM == 305) %>%
    select(ID, MY_tmax = fitted_MY)

  # Persistency
  # join summary stats together and calculate Persistency
  full_parameters_per_lac_dijkstra <-
    left_join(PY_TP_dijkstra, t_max_MY_dijkstra, by = "ID") %>%
    ungroup() %>%
    mutate(Persistency_forecasted = persistency_eq(MY_tmax, 305, PY = Mp, PT = Tp))



  #############################
  # Calculate fitted 305 d MY
    fitted_305d_MY_dijkstra <-
    fitted_values %>%
    filter(between(DIM, 303, 307)) %>%
    group_by(ID) %>%
    summarise(
      mean_305d_MY = mean(my, na.rm=TRUE),
      mean_fitted_305d_MY = mean(fitted_MY, na.rm=TRUE))



  print("Fitting DIJKSTRA... END")
  print(Sys.time())

  print("Calculting P values...")

  t_out <-
    f_forecast_P(
      predicted_MY = preds_dijkstra,
      forecasted_305d_MY = fitted_305d_MY_dijkstra,
      n_params = 4
      )

  print("Calculting P values...END")

  return(list(
    fit_info_out = info_dijkstra,
    params_out = params_dijkstra,
    params_wide = full_parameters_per_lac_dijkstra,
    preds_out = preds_dijkstra,
    individual_fit_stats = individual_fit_stats_dijkstra,
    fitted_MY_all = fitted_values,
    fitted_305d_MY = fitted_305d_MY_dijkstra,
    fits_nls_table = fits_dijkstra,
    P_wide = t_out$df_P_wide_out,
    P_full_calc = t_out$df_probability_full
  ))


}
