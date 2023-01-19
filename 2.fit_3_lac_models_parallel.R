# Fit 3 lac curves
# David Innes


# Input: cleaned_data from f_clean_lac_data()
# requires: folder called ./functions with:
#     - f_lac_equations.R
#     - f_goodness_of_fit.R

# Output: saves 4x dataframes:
#   1. Combined_params
#   2. Combined_fit_stats
#   3. Combined_preds
#   4. combined fit stats
# Also saves plots

#setwd("./R_lac/")

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

##########################
# Set cores
##########################
# make clusters
cluster <- new_cluster(parallel::detectCores()-1)

# start libraries on each
cluster_library(cluster, "nls.multstart")

###########################
# Import File
###########################

# Import file. It's also stored as a .csv but using .rds is faster.
# it was stored with groupings, so remove them here to start clean.
# keep only the columns we need for this script:

cleaned_data <-
  readRDS("./Outputs/cleaned_data_out.rds") %>%
  ungroup() %>%
  select(Cow, Parity, ID, DIM, my, maxDIM)

# Partition data for fitting across cores
partitioned_data <- cleaned_data %>%
  group_by(ID) %>%
  nest() %>%
  partition(cluster)


###########################
# Define Equations and Functions
###########################

source("./functions/f_lac_equations.R")

source("./functions/f_goodness_of_fit.R")

###########################
# Copy lac curve equations to each core
###########################

multidplyr::cluster_copy(cluster,
                         c("dijkstra_eq", "wilmink_eq", "wood_eq"))

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
                                          lower = c(a = 0, b = 0, b0 = 0, c = 0),
                                          upper = c(a = Inf, b = 1, b0 = 1, c = 1)
                          ))) %>%
  collect()

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

# Time to maximum DIM (length of lactation)
t_max_MY_dijkstra <-
  preds_dijkstra %>%
  select(ID,DIM, .fitted) %>%
  group_by(ID) %>%
  mutate(t_max = max(DIM)) %>%
  filter(DIM == t_max) %>%
  select(ID, MY_tmax = .fitted, t_max)

# Persistency
# join summary stats together and calculate Persistency
full_parameters_per_lac_dijkstra <-
  left_join(PY_TP_dijkstra, t_max_MY_dijkstra, by = "ID") %>%
  ungroup() %>%
  mutate(Persistency = persistency_eq(MY_tmax, t_max, PY = Mp, PT = Tp))

individual_fit_stats_dijkstra <-
  preds_dijkstra %>%
  nest() %>%
  mutate(fit_stats = purrr::map(data,
                                ~ f_goodness_of_fit(.x$my, .x$.fitted, n_params = 4))) %>%
  unnest(fit_stats) %>%
  select(!where(is.list))

#cleanup
rm(fits_dijkstra, PY_TP_dijkstra,t_max_MY_dijkstra )

print("Fitting DIJKSTRA... END")
print(Sys.time())

###########################
# Wood
###########################

print("Fitting WOOD...")
print(Sys.time())

# Partition data for fitting across cores - repeated to see if improves time
partitioned_data <- cleaned_data %>%
  group_by(ID) %>%
  nest() %>%
  partition(cluster)


fits_wood <- partitioned_data %>%
  mutate(fit = purrr::map(data,
                          ~ nls_multstart(my ~ wood_eq(a,b,c, t=DIM),
                                          data = .x,
                                          iter = 500,
                                          start_lower = c(a=10, b=0.02,c=0.001),
                                          start_upper = c(a=100, b=1,c=1),
                                          supp_errors = 'Y',
                                          convergence_count = 100,
                                          na.action = na.omit
                          ))) %>%
  collect()

# get summary
info_wood <- fits_wood %>%
  mutate(summary = map(fit, glance)) %>%
  unnest(summary) %>%
  select(-where(is.list))

# get params
params_wood <- fits_wood %>%
  mutate(p = map(fit, tidy)) %>%
  unnest(p) %>%
  select(-where(is.list))


# get predictions
preds_wood <- fits_wood %>%
  mutate(p = map(fit, augment)) %>%
  unnest(p) %>%
  select(-where(is.list))


# calculate peak yield and time to peak yield

PY_TP_wood <-
  params_wood %>%
  select(ID, term, estimate) %>%
  pivot_wider(names_from = term, values_from=estimate) %>%
  group_by(ID) %>%
  mutate(Tp = wood_time_PY_eq(b,c),
         Mp =  wood_eq(a,b,c, t = Tp))

# Time to maximum DIM (length of lactation)
t_max_MY_wood <-
  preds_wood %>%
  select(ID,DIM, .fitted) %>%
  group_by(ID) %>%
  mutate(t_max = max(DIM)) %>%
  filter(DIM == t_max) %>%
  select(ID, MY_tmax = .fitted, t_max)

# Persistency
# join summary stats together and calculate Persistency
full_parameters_per_lac_wood <-
  left_join(PY_TP_wood, t_max_MY_wood, by = "ID") %>%
  ungroup() %>%
  mutate(Persistency = persistency_eq(MY_tmax, t_max, PY = Mp, PT = Tp))

individual_fit_stats_wood <-
  preds_wood %>%
  nest() %>%
  mutate(fit_stats = purrr::map(data,
                                ~ f_goodness_of_fit(.x$my, .x$.fitted, n_params = 3))) %>%
  unnest(fit_stats) %>%
  select(!where(is.list))

#cleanup
rm(fits_wood, PY_TP_wood,t_max_MY_wood )


print("Fitting WOOD...END")
print(Sys.time())
###########################
# Wilmink
###########################


print("Fitting WILMINK...")
print(Sys.time())

# Partition data for fitting across cores - repeated to see if improves time
partitioned_data <- cleaned_data %>%
  group_by(ID) %>%
  nest() %>%
  partition(cluster)



fits_wilmink <- partitioned_data %>%
  mutate(fit = purrr::map(data,
                          ~ nls_multstart(my ~ wilmink_eq(a,b,b0,c, t=DIM),
                                          data = .x,
                                          iter = 500,
                                          start_lower = c(a=10, b=0,  b0 = 0.01, c = 0.001),
                                          start_upper = c(a=100, b=20,b0 = 0.09, c = 0.005),
                                          supp_errors = 'Y',
                                          convergence_count = 100,
                                          na.action = na.omit
                          ))) %>%
  collect()

# get summary
info_wilmink <- fits_wilmink %>%
  mutate(summary = map(fit, glance)) %>%
  unnest(summary) %>%
  select(-where(is.list))

# get params
params_wilmink <- fits_wilmink %>%
  mutate(p = map(fit, tidy)) %>%
  unnest(p) %>%
  select(-where(is.list))


# get predictions
preds_wilmink <- fits_wilmink %>%
  mutate(p = map(fit, augment)) %>%
  unnest(p) %>%
  select(-where(is.list))


# calculate peak yield and time to peak yield

PY_TP_wilmink <-
  params_wilmink %>%
  select(ID, term, estimate) %>%
  pivot_wider(names_from = term, values_from=estimate) %>%
  group_by(ID) %>%
  mutate(Tp = wilmink_time_PY_eq(b,b0,c),
         Mp =  wilmink_eq(a,b,b0,c, t = Tp))

# Time to maximum DIM (length of lactation)
t_max_MY_wilmink <-
  preds_wilmink %>%
  select(ID,DIM, .fitted) %>%
  group_by(ID) %>%
  mutate(t_max = max(DIM)) %>%
  filter(DIM == t_max) %>%
  select(ID, MY_tmax = .fitted, t_max)

# Persistency
# join summary stats together and calculate Persistency
full_parameters_per_lac_wilmink <-
  left_join(PY_TP_wilmink, t_max_MY_wilmink, by = "ID") %>%
  ungroup() %>%
  mutate(Persistency = persistency_eq(MY_tmax, t_max, PY = Mp, PT = Tp))


individual_fit_stats_wilmink <-
  preds_wilmink %>%
  nest() %>%
  mutate(fit_stats = purrr::map(data,
                                ~ f_goodness_of_fit(.x$my, .x$.fitted, n_params = 3))) %>%
  unnest(fit_stats) %>%
  select(!where(is.list))

#cleanup
rm(fits_wilmink, PY_TP_wilmink,t_max_MY_wilmink )


print("Fitting WILMINK...END")
print(Sys.time())

###########################
# Output: Model Parameters
###########################

print("Output files...")
print(Sys.time())
#reformat param tables,  joining together
combined_params <-
  full_parameters_per_lac_dijkstra %>%
  mutate(Model = "Dijkstra") %>%
  bind_rows(
    full_parameters_per_lac_wood %>%  mutate(Model = "Wood")
  ) %>%
  bind_rows(
    full_parameters_per_lac_wilmink %>% mutate(Model = "Wilmink")
  )

# annotate with parity info
parity_key <-
  cleaned_data %>%
  select(ID, Parity, maxDIM) %>%
  distinct() %>%
  mutate(Parity_class = case_when(Parity == 1 ~ "Primiparous",
                                  Parity > 1 ~ "Multiparous"),
         Lac_len_group = case_when(between(maxDIM, 305, 399) ~ "Group1",
                                   maxDIM >=400 ~ "Group2"))

combined_params <-
  parity_key %>%
  right_join(combined_params)


# output as CSV
data.table::fwrite(combined_params, file = "./Outputs/combined_params.csv")

# output as .RDS
saveRDS(combined_params, "./Outputs/combined_params.rds")


###########################
# Output: Fit statistics
###########################
combined_fit_stats <-
  individual_fit_stats_dijkstra %>%
  mutate(Model = "Dijkstra") %>%
  bind_rows(
    individual_fit_stats_wood %>% mutate(Model = "Wood")
  ) %>%
  bind_rows(
    individual_fit_stats_wilmink %>% mutate(Model = "Wilmink")
  ) %>%
  left_join(parity_key) # annotate with parity info

# output as CSV
data.table::fwrite(combined_fit_stats, file = "./Outputs/combined_fit_stats.csv")

# output as .RDS
saveRDS(combined_fit_stats, "./Outputs/combined_fit_stats.rds")



###########################
# Output: Fitted data
###########################

combined_preds_fitted <-
  preds_dijkstra %>%
  mutate(Model = "Dijkstra") %>%
  bind_rows(
    preds_wood %>% mutate(Model = "Wood")
  ) %>%
  bind_rows(
    preds_wilmink %>% mutate(Model = "Wilmink")
  ) %>% ungroup() %>%
  left_join(parity_key) # annotate with parity info

# output as CSV
data.table::fwrite(combined_preds_fitted, file = "./Outputs/combined_preds_fitted.csv")

# output as .RDS
saveRDS(combined_preds_fitted, "./Outputs/combined_preds_fitted.rds")



###########################
# Output: Fit info
###########################

combined_fit_info <-
  info_dijkstra %>%
  mutate(Model = "Dijkstra") %>%
  bind_rows(
    info_wood %>% mutate(Model = "Wood")
  ) %>%
  bind_rows(
    info_wilmink %>% mutate(Model = "Wilmink")
  ) %>% ungroup()

# output as CSV
data.table::fwrite(combined_fit_info, file = "./Outputs/combined_fit_info.csv")

# output as .RDS
saveRDS(combined_fit_info, "./Outputs/combined_fit_info.rds")

print("Output files...END")
print(Sys.time())

###########################
# Output: Plots
###########################

print("Output plots...")
print(Sys.time())

.f_plot_combined <- function(.x){

  p <- ggplot() +
    geom_point(aes(DIM, my), .x) +
    geom_line(aes(DIM, .fitted, colour = Model, linetype = Model),
              .x,
              size = 1.5)+
    geom_vline(aes(xintercept=305), colour='red')+
    ggtitle(paste("Cow_lactation number =",.x$ID))



  print(paste0("plot_",unique(.x$ID),".png"))

  p <- ggsave(filename = paste0("plot_",unique(.x$ID),".png"),
              device = 'png',
              path = "./Plots/Combined_lac_fitted/",
              plot = p,
              dpi = 120,
              width=10,
              height=7)

}


combined_preds_fitted %>%
  group_by(ID) %>%
  group_split() %>%
  map(.f_plot_combined)

print("Output plots...END")
print(Sys.time())
print("END")











