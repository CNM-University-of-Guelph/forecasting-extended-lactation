# Run t test between groups of interest
# David Innes

# Makes comparisons within a Model, but between the 'group_by' provided to function

# requires: rstatix

# Input:
#   df_in =
#   group_by = str of either 'Parity_class' or 'Lac_len_group'

# Output:
# a df of t_test results

f_t_test_output <-
  function(df_in, group_by){

    # for 'inject' function below, makes a formula dynamically
    gb <- rlang::ensym(group_by)

    df_in %>%
      drop_na() %>%
      group_by(Model) %>%
      nest() %>%
      summarise(test_output = purrr::map(data,
                                         # function to run t tests for each {{ group by }}, within each model at a time
                                         # Stores in nested format, which is unnested in next step
                                         function(x){
                                           x %>%
                                             group_by( variable ) %>%
                                             rstatix::t_test(
                                               rlang::inject(value ~ !!gb) ,
                                               var.equal=FALSE,
                                               p.adjust.method = "none",
                                               detailed = TRUE
                                             ) %>%
                                             mutate(p_rounded = round(p, digits = 3),
                                                    sig = case_when(p < 0.05 ~ TRUE, TRUE ~ FALSE))
                                         }
      )) %>%
      unnest(test_output)
  }
