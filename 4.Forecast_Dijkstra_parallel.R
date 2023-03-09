# Function to iterate through different DIM thresholds for Dijkstra model
# re-fits model for each level of DIM_thresh_list defined in code.

# David Innes


# Input: cleaned_data from f_clean_lac_data()
# requires: folder called ./functions with:
#     - f_lac_equations.R
#     - f_goodness_of_fit.R
#     - f_iterate_fitting.R
# also requires user input to set behaviour of script.

# Output: saves following .rds files in Outputs/:

# combined_fit_info_out
# combined_params_out
# combined_fitted_MY_all
# combined_preds_out
# combined_fit_stats
# combined_fitted_305d_MY
# combined_P_wide
# combined_P_calcs

  ###########################
  # Set User Inputs
  ###########################
  ## Set BOUNDs for parameters
  # This can be set as mean+SD*n :

  # NOTE: these are currently set to match publication, not the example dataset
  b_lower = 0
  b_upper = 0.10924

  b0_lower = 0.01725
  b0_upper = 0.08723

  c_lower = 0.00087
  c_upper = 0.00449

  # Take user input when script is sourced:
  ## Flag for using bounds:
  ## If TRUE, then bounds are used, if FALSE then 0 and 1 (defaults are used)
  ## Output is labelled accordingly
  is_constrained = NULL

  user_in <- readline("Set is_constrained as TRUE (T) or FALSE (F). Enter T or F: ")

  is_constrained <- as.logical(user_in)

  if(!is.logical(is_constrained)){
    stop("'is_constrained' must be TRUE or FALSE")
  }


  print("START")
  print(Sys.time())

  ###########################
  # Libraries
  ###########################

  suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE))
  library(nlme)
  library(nls.multstart)
  library(broom)
  library(purrr)
  library(multidplyr)
  # requires car

  ###########################
  # Define Equations and Functions
  ###########################

  source("./functions/f_lac_equations.R")

  source("./functions/f_goodness_of_fit.R")

  source("./functions/f_iterate_fitting.R")

  source("./functions/f_forecast_P.R")


  ##########################
  # Set cores
  ##########################
  # make clusters
  cluster <- new_cluster(parallel::detectCores()-1)

  # start libraries on each
  cluster_library(cluster, "nls.multstart")

  ###########################
  # Import Files
  ###########################

  # Import file. Keep only the columns we need for this script:
  # This should be filtered data to only include lactations that converged with full dataset

  # uses user input to select the correct file.
  file_list <- list.files("./Outputs/", pattern = "filtered_cleaned_data")

  print(paste("Filtered_cleaned_data version             :", as.list(file_list)))

  user_in <- readline("Input number matching 'filtered_cleaned_data' to import:")

  file_subset <- as.integer(user_in)

  if(!is.integer(file_subset) | is.na(file_subset) | file_subset > length(file_list)){
    stop("Unrecoginsed number. Cannot select file.")
  }

 # read in select filed
  working_data <- readRDS(paste0("./Outputs/", file_list[file_subset]))



  ###########################
  # Copy lac curve equations to each core
  ###########################

  multidplyr::cluster_copy(cluster, c("dijkstra_eq"))

  #############################
  # Execute
  # Actual model fitting is done by `f_iterate_fitting.R`
  #############################

  # setup DIM list
  DIM_thresh_list <-
    setNames(list(30, 60, 90, 120, 150, 200, 250, 300),
             as.character(c(30, 60, 90, 120, 150, 200, 250, 300))
    )


  print(paste("Fitting with `is_constrained` == ", is_constrained))

  if(is_constrained){

    # Execute function
    fitting_output <-
      map(DIM_thresh_list,
          ~ f_iterate_fitting(cleaned_data_in = working_data,
                              DIM_threshold = .x,
                              b_lower_bound = b_lower,
                              b_upper_bound = b_upper,
                              b0_lower_bound = b0_lower,
                              b0_upper_bound = b0_upper,
                              c_lower_bound = c_lower,
                              c_upper_bound = c_upper))
  } else if(is_constrained==FALSE){

    fitting_output <-
      map(DIM_thresh_list,
          ~ f_iterate_fitting(cleaned_data_in = working_data,
                              DIM_threshold = .x))
  }



  print("Fitting models complete")
  ###################################
  # Combine results
  ###################################
  print("Combining results...")

  combined_params_out <-
    fitting_output %>%
    map("params_wide") %>%
    bind_rows(.id = "DIM_thresh") %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))

  combined_fitted_MY_all <-
    fitting_output %>%
    map("fitted_MY_all") %>%
    bind_rows(.id= 'DIM_thresh') %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))

  combined_fit_stats <-
    fitting_output %>%
    map("individual_fit_stats") %>%
    bind_rows(.id = 'DIM_thresh') %>%
    mutate(across(where(is.double), ~round(.x,3))) %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))

  combined_fitted_305d_MY <-
    fitting_output %>%
    map('fitted_305d_MY') %>%
    bind_rows(.id = 'DIM_thresh') %>%
    mutate(across(where(is.double), ~round(.x,3))) %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))

  combined_fit_info_out <-
    fitting_output %>%
    map('fit_info_out') %>%
    bind_rows(.id = 'DIM_thresh') %>%
    mutate(across(where(is.double), ~round(.x,3))) %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))

  combined_preds_out <-
    fitting_output %>%
    map('preds_out') %>%
    bind_rows(.id = 'DIM_thresh') %>%
    mutate(across(where(is.double), ~round(.x,3))) %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))

  combined_P_wide <-
    fitting_output %>%
    map('P_wide') %>%
    bind_rows(.id = 'DIM_thresh') %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))

  combined_P_calcs <-
    fitting_output %>%
    map('P_full_calc') %>%
    bind_rows(.id = 'DIM_thresh') %>%
    mutate(DIM_thresh = factor(DIM_thresh, levels = as.character(c(30, 60, 90, 120, 150, 200, 250, 300))))
  print("Combining results...END")

  ###################################
  # Export
  ###################################
  print("Exporting results...")
  # save nested output
  #saveRDS(fitting_output, "./Output/iterated_full_nested_Rfile.rds")
  # Very large output!!

  if(is_constrained){
    output_label <- "params_constrained"
  } else if(is_constrained==FALSE){
    output_label <- "params_default"
  }

  saveRDS(combined_fit_info_out, paste0("./Outputs/",output_label, "_iterated_fit_info_.rds"))

  saveRDS(combined_params_out, paste0("./Outputs/",output_label, "_iterated_DIM_params.rds"))

  saveRDS(combined_fitted_MY_all, paste0("./Outputs/",output_label, "_iterated_DIM_fitted_MY_all.rds"))

  saveRDS(combined_preds_out, paste0("./Outputs/",output_label, "_iterated_preds_out.rds"))

  saveRDS(combined_fit_stats, paste0("./Outputs/",output_label, "_iterated_DIM_fit_stats.rds"))

  saveRDS(combined_fitted_305d_MY, paste0("./Outputs/",output_label, "_iterated_DIM_fitted_305d_MY.rds"))

  saveRDS(combined_P_wide, paste0("./Outputs/",output_label, "_iterated_Pvalues_wide.rds"))

  saveRDS(combined_P_calcs, paste0("./Outputs/",output_label, "_iterated_Pvalues_FULL.rds"))

  print("Exporting results...END")

  print(Sys.time())
  print("END")

