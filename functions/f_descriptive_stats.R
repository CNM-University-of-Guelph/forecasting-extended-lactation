# Function to generate descriptive stats for publication
# David Innes

# Input:
# - cleaned_data_in: output from f_clean_lac_data
# - total_MY_summary_in: output from f_clean_lac_data
#  This is interpolated data for calculating total MY from 10 DIM to 305 DIM and MY at 305 d

# OUTPUT:
# - 1 df with 2 columns (Description, Value_new)
# Includes rows:
    # n_Cows
    # n_Lactations
    # n_Primiparous
    # n_Multiparous
    # n_300 to 399 d in length
    # n_> 400 d in length
    # nrecords_per_lac_Mean
    # nrecords_per_lac_SD
    # lac_length_Mean
    # lac_length_SD
    # lac_length_Mode
    # lac_length_Minimum
    # lac_length_Maximum
    # d305MY_Mean
    # d305MY_SD
    # d305MY_Mode
    # d305MY_Minimum
    # d305MY_Maximum



# f_descriptive_stats
f_descriptive_stats <- function(cleaned_data_in){

  # function for mode:
  # code from: https://stackoverflow.com/a/8189441/18398573
  .Modes <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
  }

  # Both input dataframes come from the output of the f_clean_lac_data()

  cleaned_data <- cleaned_data_in

  ##########################
  # Step 1 - summarise data (max, min, n)
  ##########################

  summary_DIM <- cleaned_data %>%
    group_by(Cow, Parity, ID) %>%
    summarise(maxDIM = max(DIM),
              minDIM = min(DIM),
              n = n())


  ##########################
  # Step 2 - store other summary stats into objects
  ##########################

  n_cows <- summary_DIM$Cow %>% unique() %>% length()
  print(paste("Number of cows:",n_cows ))


  n_Lactations <- length(which(summary_DIM$maxDIM > 305))
  print(paste("Number of complete lactations (> 305 DIM):",n_Lactations ))


  #Check that none are less:
  if(any(summary_DIM$maxDIM < 305)) {
    stop("Lactations with max DIM less than 305 days detected in cleaned_data_in. Check cleaning steps.
         All lactations should be > 305 DIM.")
  }


  ##########################
  # Step 3 - Parity summary & lac length summary
  ##########################

  annotated_data <-
    cleaned_data %>%
    ungroup() %>%
    mutate(Parity_class = case_when(Parity == 1 ~ "primiparous_lactation",
                                    Parity > 1 ~ "multiparous_lactation"),
           Lac_length_group = case_when(between(maxDIM, 305, 399) ~ "Group1",
                                        maxDIM >= 400 ~ "Group2"))

  parity_class_count <-
    annotated_data %>%
    select(ID, Parity_class) %>%
    distinct() %>% # 1 entry per lactation
    group_by(Parity_class) %>%
    summarise(n = n())


  n_first_lac <- parity_class_count %>% filter(Parity_class == "primiparous_lactation") %>% pull(n)
  n_multi_lac <- parity_class_count %>% filter(Parity_class == "multiparous_lactation") %>% pull(n)


  lac_length_group_count <-
    annotated_data %>%
    select(ID, Lac_length_group) %>%
    distinct() %>%
    group_by(Lac_length_group) %>%
    summarise(n = n())


  n_group1 <- lac_length_group_count %>% filter(Lac_length_group == "Group1") %>% pull(n)
  n_group2 <- lac_length_group_count %>% filter(Lac_length_group == "Group2") %>% pull(n)


  ##########################
  # Step 4 - Summarise records per lactation
  ##########################

  records_per_lac <-
    cleaned_data %>%
    group_by(ID) %>%
    count() %>%
    ungroup() %>%
    summarise("Avg_records" = mean(n) %>% round(0),
              "SD_records" = sd(n) %>% round(0))

  nrecords_per_lac_Mean <- records_per_lac %>% pull(Avg_records)
  nrecords_per_lac_SD <- records_per_lac %>% pull(SD_records)

  ##########################
  # Step 5 - Summary of lac length  (d)
  ##########################

  summarised_maxDIM <-
    cleaned_data %>%
    ungroup() %>%
    summarise("mean_maxDIM" = mean(maxDIM),
              "SD_maxDIM" = sd(maxDIM),
              'mode_maxDIM' = .Modes(maxDIM),
              "min_maxDIM" = min(maxDIM),
              'max_maxDIM' = max(maxDIM)) %>%
    mutate(across(where(is.numeric), ~round(.x, 0)))


  ##########################
  # Step 6 - Summary of MY at d 305 (kg)
  ##########################

  summarised_d305MY <-
    cleaned_data %>%
    filter(between(DIM, 303, 307)) %>%
    group_by(ID) %>%
    summarise(MY_305d = mean(my, na.rm=TRUE)) %>%
    ungroup() %>%
    summarise("mean_d305MY" = mean(MY_305d),
              "SD_d305MY" = sd(MY_305d),
              'mode_d305MY' = .Modes(MY_305d) %>% median(), #for when multiple modes
              "min_d305MY" = min(MY_305d),
              'max_d305MY' = max(MY_305d)) %>%
    mutate(across(where(is.numeric), ~round(.x, 1)))


  ##########################
  # Step 8 - Create dataframe of results
  ##########################

  df_out <- data.frame(
    Description = c(
      'n_Cows',
      'n_Lactations',
      'n_Primiparous',
      'n_Multiparous',
      'n_305_to_399d_in_length',
      'n_GT_400d_in_length',
      'nrecords_per_lac_Mean',
      'nrecords_per_lac_SD',
      'lac_length_Mean',
      'lac_length_SD',
      'lac_length_Mode',
      'lac_length_Minimum',
      'lac_length_Maximum',
      'd305MY_Mean',
      'd305MY_SD',
      'd305MY_Mode',
      'd305MY_Minimum',
      'd305MY_Maximum'
      ),
    Value_new =   c(n_cows,
                    n_Lactations,
                    n_first_lac,
                    n_multi_lac,
                    n_group1,
                    n_group2,
                    nrecords_per_lac_Mean,
                    nrecords_per_lac_SD,
                    summarised_maxDIM$mean_maxDIM,
                    summarised_maxDIM$SD_maxDIM,
                    summarised_maxDIM$mode_maxDIM,
                    summarised_maxDIM$min_maxDIM,
                    summarised_maxDIM$max_maxDIM,
                    summarised_d305MY$mean_d305MY,
                    summarised_d305MY$SD_d305MY,
                    summarised_d305MY$mode_d305MY,
                    summarised_d305MY$min_d305MY,
                    summarised_d305MY$max_d305MY
    ))


  return(df_out)

}
