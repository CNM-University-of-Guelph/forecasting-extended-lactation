# Function to recreate Table 2.3 or Table 2.7 summaries from Linaya's thesis
# David Innes

# Input:
#   - cleaned_data: dataframe. Output from f_clean_lac_data()
#   - total_MY_summary: dataframe. Output from f_clean_lac_data()
#   - to_group_by: Column name to group by for summary, e.g. Parity_class, Lac_len_group

# Output: dataframe

# Note: Group 1 and 2 changed to Group S and Group L in publication by editing final plots and tables only.



f_summary_tables <- function(cleaned_data, total_MY_summary, to_group_by ){

  .cleaned_data <-
    cleaned_data %>%
    ungroup() %>%
    mutate(Parity_class = case_when(Parity == 1 ~ "Primiparous",
                                    Parity > 1 ~ "Multiparous"),
           Parity_class = factor(Parity_class,
                                 levels = c("Primiparous","Multiparous")),
           Lac_len_group = case_when(between(maxDIM, 305, 399) ~ "Group1",
                                     maxDIM >= 400 ~ "Group2"))

  ID_level_summarised <-
    .cleaned_data %>%
    group_by(Cow, {{ to_group_by }}, ID) %>%
    summarise(maxDIM = max(DIM),
              minDIM = min(DIM),
              n = n()) %>%
    left_join(total_MY_summary, by = 'ID') %>%
    group_by({{ to_group_by }}) %>%
    summarise(n = n(),
              mean_lac_length_d = mean(maxDIM),
              sd_lac_length_d = sd(maxDIM),
              mean_total_305d_my_kg = mean(MY_305d_total),
              sd_total_305d_my_kg = sd(MY_305d_total),
              mean_305d_my_kg = mean(MY_305d),
              sd_305d_my_kg = sd(MY_305d)) %>%
    mutate(across(where(is.double), ~round(.x, 2)))


  whole_data_summarised <-
    .cleaned_data %>%
    group_by({{ to_group_by }}) %>%
    summarise(n = length(unique(ID)),
              mean_MY_kg_d = mean(my, na.rm = TRUE),
              sd_MY_kg_d = sd(my, na.rm = TRUE)) %>%
    mutate(across(where(is.double), ~round(.x, 2)))


  df_out <-
    left_join(ID_level_summarised, whole_data_summarised)  %>%
    pivot_longer(cols = n:last_col()) %>%
    pivot_wider(names_from = {{ to_group_by }})

  return(df_out)

}

