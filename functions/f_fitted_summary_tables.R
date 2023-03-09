# Functions to summarise model outputs by taking means of parameters or fit statistics
# David Innes

# Input:
#     - combined_params_in = df. output from fit_3_lac_models()
#     - groups_by = name of column/s to group data by when producing summary tables, in form: c(col1, col2)

# Output: df of summarised_params
#


f_fitted_summary_tables <- function(combined_params_in, groups_by){

  summarised_params <-
    combined_params_in %>%
    mutate(across(where(is.double), ~ case_when(.x == Inf ~ NaN, TRUE ~ .x))) %>%
    group_by( across({{ groups_by }})) %>%
    summarise(across(where(is.double),
                     .fns = list(mean = ~ mean(.x, na.rm=TRUE),
                                 sd = ~ sd(.x, na.rm=TRUE)))) %>%
    arrange( across({{ groups_by }})) %>%
    rename_at(vars(contains("Mp")), list(~str_replace(., "Mp_", "Peak_Yield_kg_"))) %>%
    rename_at(vars(contains("Tp")), list(~str_replace(., "Tp_", "Time_to_PY_d"))) %>%
    select(-contains('t_max'), -contains('tmax'))

  return(summarised_params)

}


# Helper function to format tables for publication format
# takes output from above and reshapes it

f_format_pub <- function(summary_df, groups_by){
  summary_df %>%
    select({{ groups_by }}, contains('mean'), contains('sd'), -contains('maxDIM')) %>%
    pivot_longer(cols = where(is.double)) %>%
    pivot_wider(names_from = {{groups_by}}, values_from=value) %>%
    mutate(across(where(is.double), ~round(.x, 4) ))
}



f_fit_stats_summary <- function(fit_stats_in, groups_by){
  fit_stats_in %>%
    group_by(across({{ groups_by }})) %>%
    summarise(across(where(is.double),
                     .fns = list(mean = ~ mean(.x, na.rm=TRUE),
                                 sd = ~ sd(.x, na.rm=TRUE))),
              DW_prop_LT_1.5 = length(DW[DW<1.5]) /  n()*100)
}
