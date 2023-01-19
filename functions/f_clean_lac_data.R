# Function to run all pre-processing and data cleaning steps for analysis of lactation data
# David Innes

# INPUT:
# - workingfile_in: a dataframe with at least these columns:
#       1. Cow (cow's number identification)
#       2. Parity (number representing what parity the lactation represents)
#       3. ID (lactation ID, as Cow_Parity#)
#       4. DIM (days in milk; d)
#       5. my (milk yield; kg)
#
# - generate_plots: boolean:
#       TRUE will generate plots for individual lactations (e.g. 500 plots) which are stored in a list object (not saved with ggsave).
#       FALSE will skip generate these plots, but will still plot summary/overall plots.

# OUTPUT:
# - A list of:
#   cleaned_data      = a dataframe of data after all cleaning steps are completed
#   log               = a list of printed summary outputs saved throughout analysis
#   plots             = a list of various plots stored throughout cleaning, including individual plots of generate_plots == TRUE
#   total_MY_summary  = a dataframe of summarised MY data (305 d MY, etc) calculated based off interpolated data
#   interpolated_raw  = a dataframe of MY after interpolation. Not to be used for downstream analysis, only for summary (above)

f_clean_lac_data <-
  function(workingfile_in, generate_plots = TRUE){

    ##########################
    # Step 1 - remove NA
    ##########################
    print("Step 1 - remove NA")
    # Count number of rows in each column with NA values
    log_list <- list()
    log_list$Step1_NA_removed <- list( workingfile_in %>% summarise(across(everything(), ~sum(is.na(.x)))))

    # Remove NA's
    workingfile_noNA <- workingfile_in %>% drop_na()


    ##########################
    # Step 2 - Remove non-integer DIM
    # Remove whole lactation. '407_10' should be removed in this case.
    ##########################
    # TO DO: could use . %>% filter(DIM %% 1 > 0)  instead
    print("Step 2 - Remove non-integer DIM")
    DIM_test <- workingfile_noNA %>%
      mutate(DIM2 = as.integer(DIM))

    #find ID's
    IDs_bad_DIM <- DIM_test[which(!DIM_test$DIM == DIM_test$DIM2),] %>% pull(ID) %>% unique()

    #store to log
    log_list$IDs_bad_DIM <- IDs_bad_DIM

    #remove from working file
    workingfile_step2_out <- workingfile_noNA %>% filter(!ID %in% IDs_bad_DIM)

    #print size of dataset
    print(paste("Step 2 - Remove non-integer DIM - dimensions = ", paste(dim(workingfile_step2_out), collapse = ", ")))

    #cleanup
    rm(DIM_test, IDs_bad_DIM)


    ##########################
    # Step 2a - Plot n per DIM prefilter
    ##########################
    print("Step 2a - Plot n per DIM prefilter")
    plots_list <- list()

    plots_list$n_per_DIM_prefilter <-
      workingfile_step2_out %>%
      group_by(DIM) %>%
      count() %>%
      ggplot(aes(x = DIM, y = n))+
      geom_point()+
      geom_vline(aes(xintercept = 305), colour = 'red')+
      labs(title="Number of individual lactations (n) per Days In Milk (DIM)",
           subtitle = "Red line shows 305 DIM")


    ##########################
    # Step 3 - Filter
    ##########################
    print("Step 3 - Filter")
    log_list$"PRE-FILTER - Number Unique Cows" <-  workingfile_step2_out$Cow %>% unique() %>% length()
    log_list$"PRE-FILTER - Number Unique Lactations" <-  workingfile_step2_out$ID %>% unique() %>% length()

    #set up function, used again later.
    f_initial_filter <- function(df_in) {
      df_in %>%
        group_by(ID) %>%
        #calculate stats, by group, stored in new columns
        mutate(maxDIM = max(DIM),
               minDIM = min(DIM),
               n = n()) %>%
        #use these columsn to filter dataframe
        filter(maxDIM > 305 & n > 150 & minDIM < 10)
    }

    #execute
    workingfile_step3_out <-  workingfile_step2_out %>% f_initial_filter()


    log_list$"1st-FILTER: Number Unique Cows" <-  workingfile_step3_out$Cow %>% unique() %>% length()
    log_list$"1st-FILTER: Number Unique Lactations" <-  workingfile_step3_out$ID %>% unique() %>% length()
    log_list$"1st-FILTER: Number rows" <-  nrow(workingfile_step3_out)


    ##########################
    # Step 3b - Plot n per DIM
    ##########################
    print("Step 3b - Plot n per DIM ")
    n_unique_lac <- workingfile_step3_out$ID %>% unique() %>% length()

    plots_list$n_per_DIM_postfilter <-
      workingfile_step3_out %>%
      # filter(DIM <= 20) %>%
      group_by(DIM) %>%
      count() %>%
      ungroup() %>%
      #mutate(n_perc = round(n/n_unique_lac*100,2)) %>%
      ggplot(aes(x = DIM, y=n))+
      geom_point()+
      geom_vline(aes(xintercept = 10), colour = 'red')+
      geom_vline(aes(xintercept = 305), colour = 'red')+
      geom_hline(aes(yintercept = n_unique_lac), colour ='blue')+
      scale_x_continuous(breaks = c(0,10,100,200,300,400,500,600,700))+
      labs(title="Post-Filtering - Number of individual lactations (n) per Days In Milk (DIM)",
           subtitle = "Red lines indicate 10 and 305 DIM (the range for total MY). \nBlue line indicates total number of unique lactations.")


    #cleanup
    rm(workingfile_noNA, workingfile_step2_out)

    ##########################
    # Step 4 - Gap detection
    ##########################
    print("Step 4 - Gap detection")
    # Ensure DIM are numbered sequentally first
    # Use lag() to calculate diff between rows
    # by group

    workingfile_step4_out <-
      workingfile_step3_out %>%
      group_by(ID) %>%
      arrange(DIM, .by_group = TRUE) %>%
      mutate(lag_DIM = DIM - lag(DIM),
             lag_MY = my - lag(my),
             my_moving_avg = slider::slide_dbl(my, mean, .before = 7, .after = 0),
             my_moving_SD = slider::slide_dbl(my, sd, .before = 2, .after = 0),
             lag_MY_avg = my_moving_avg - lag(my_moving_avg)
      )

    #######
    # Find lactations with a break in the middle of lactation <305d
    ######
    mid_lac_break_ID <-
      workingfile_step4_out %>%
      group_by(ID) %>%
      arrange(DIM, .by_group = TRUE) %>%
      mutate(lag_DIM = DIM - lag(DIM)) %>%
      filter(DIM > 20) %>%
      #filter(lag_DIM > 14 & DIM >= 300) %>%
      filter(lag_DIM > 14 & DIM < 305) %>%
      arrange(desc(lag_DIM)) %>%
      pull(ID) %>%
      unique()


    # store to log
    log_list$"MID-Lac Gap: mid_lac_break_IDs" <- mid_lac_break_ID
    log_list$"MID-Lac Gap: n_lactations_removed_mid_lac_break" <- length(mid_lac_break_ID)


    # check for whole dataset
    plots_list$lacs_mid_break_REMOVED <-
      workingfile_step4_out %>%
      filter(ID %in% mid_lac_break_ID) %>%
      ggplot(aes(x = DIM, y = my))+
      geom_point(size = 1, alpha = 0.5)+
      geom_line(colour = 'red')+
      facet_wrap(facets="ID")+
      labs(title="Lactations with a mid-lactation break in data > 14 days",
           subtitle="These will be removed from dataset.")


    ##########################
    # 4a - filter out based on gaps
    #
    # 1. remove full lactation where there is a break > 14 days and < 305 DIM (n = 12)
    # 2. Remove trailing data that is after end of lactation:
    #
    #   - For each lactation, find row where lag_DIM > 14 days and DIM >= 305
    #   - Remove this row, and all rows with a greater DIM.
    #
    # 3. Re-filter data with original criteria (e.g. > 305 DIM). Some lactations will be removed that previously weren't.
    ##########################
    print("Step 4a - filter data based on gaps")

    workingfile_step4a_out <-
      workingfile_step4_out %>%
      group_by(ID) %>%
      mutate(DIM_filter = case_when(lag_DIM > 14 & DIM >= 305 ~ DIM)) %>%  #return DIM for filter (used later)
      group_split() %>%
      map_dfr(.f = function(df){
        #1.
        if(any(df$ID %in% mid_lac_break_ID)){
          # If lactation has break mid-lac then
          # set all rows as TRUE (i.e. to remove)
          df <- df %>% mutate(DIM_flag = TRUE)
          return(df)

          #2a.
        } else if(sum(df$DIM_filter, na.rm = TRUE) > 0){
          #sum()>0 finds lactations that need filtering
          # temp number to filter with
          .DIM_filter <- min(df$DIM_filter, na.rm = TRUE)

          # add column with flag, add additional colum that will be used to count only rows removed by this particular part of the filter
          df <- df %>%
            mutate(DIM_flag = case_when(DIM >= .DIM_filter ~ TRUE, TRUE ~ FALSE),
                   DIM_flag_reporting = case_when(DIM >= .DIM_filter ~ TRUE, TRUE ~ FALSE))

          return(df)

          #2b.
        } else{
          # for lactations without any breaks, keep.
          df <- df %>% mutate(DIM_flag = FALSE)

          return(df)
        }
      })

    if(generate_plots == TRUE){
      # Plot to check classifications
      # WARNING: ~500 plots stored in this list
      plots_list$list_gap_classification_plots <-
        workingfile_step4a_out %>%
        group_by(ID) %>%
        {  setNames(group_split(.), pull(., ID) %>% unique())  } %>%
        map(.f = function(df){
          ggplot(df,aes(x = DIM, y = my, colour = DIM_flag))+
            geom_point()+
            geom_line()+
            labs(title = paste(df$ID %>% unique(),collapse=" "),
                 subtitle = "ID points or lactations removed due to gaps in data")+
            scale_colour_manual(values = c("TRUE" = "#36AAF9FF", "FALSE" = "#7A0403FF"))
      })
    }


    # Count rows removed by DIM_flag
    log_list$"GAP-FILTER: n_rows_pre-gap-filter" <- nrow(workingfile_step4_out)
    log_list$"GAP-FILTER: n_rows_removed >=305 DIM" <- workingfile_step4a_out %>% group_by(DIM_flag) %>% count()
    log_list$"MID-Lac GAP-FILTER: n_rows_removed mid lac break > 14 d" <- workingfile_step4_out %>% filter(ID %in% mid_lac_break_ID) %>% nrow()


    # Remove flagged data, and re-filter using initial criteria
    workingfile_step4b_out <-
      workingfile_step4a_out %>%
      filter(DIM_flag == FALSE)


    log_list$"GAP-FILTER: Number Unique Cows" <-  workingfile_step4b_out$Cow %>% unique() %>% length()
    log_list$"GAP-FILTER: Number Unique Lactations" <- workingfile_step4b_out$ID %>% unique() %>% length()
    log_list$"GAP-FILTER: n_rows_post-gap-filter" <- nrow(workingfile_step4b_out)

    # re-filter with intiial criteria
    workingfile_step4_out <-
      workingfile_step4b_out %>%
      f_initial_filter()

    log_list$"RE-FILTER: Number Unique Cows" <-  workingfile_step4_out$Cow %>% unique() %>% length()
    log_list$"RE-FILTER: Number Unique Lactations" <- workingfile_step4_out$ID %>% unique() %>% length()
    log_list$"RE-FILTER: n_rows post-re-filter" <- nrow(workingfile_step4_out)


    ##########################
    # Step 5. Outlier detection
    #
    # This method uses the loess function to get a smoothed average, with the span set to 0.25
    # so that it follows sufficient variation while smoothing any large outliers.
    # The 'residual' from the list smoothed line is then used to detect points that are >= 3 SD from the line.
    ##########################
    print("Step 5. Outlier detection")

    workingfile_step5_out <-
      workingfile_step4_out %>%
      group_by(ID) %>%
      mutate(seq_index = row_number()) %>% #sequence for loess, by each group
      group_split() %>%
      map_df(function(.x){
        .loess_out  <-
          loess(my ~ seq_index,
                data = .x,
                span = 0.25,
                control = loess.control(surface="direct"))

        # add column with data
        .x$smoothed_MY <- predict(.loess_out)
        return(.x)
      }
      ) %>%
      group_by(ID) %>%
      mutate(my_resid = my-smoothed_MY,
             resid_SD = sd(my_resid),
             SD_ratio = abs(my_resid)/resid_SD,
             SD_from_fitted = cut(SD_ratio, breaks = 0:10, labels=FALSE)-1,
             is_outlier = abs(my_resid) >= resid_SD*3 )

    if(generate_plots == TRUE){
    # Visualise outliers
    # WARNING: ~500 plots stored in this list
    plots_list$list_outlier_detection_plots <-
      workingfile_step5_out %>%
      group_by(ID) %>%
      {  setNames(group_split(.), pull(., ID) %>% unique())  }%>%
      map(.f = function(x){
        ggplot(x)+
          geom_point(aes(DIM, my, colour = as.factor(SD_from_fitted), shape = is_outlier)) +
          geom_line(aes(DIM, smoothed_MY))+
          geom_vline(aes(xintercept=305), colour='red')+
          labs(title = paste(x$ID %>% unique(),collapse=" "),
               subtitle = "is_outlier == TRUE indicate points that are >= 3 SD from fitted line.")
      })
    }


    ##########################
    # Step 6. Remove outliers (detected above)
    ##########################
    print("Step 6. Remove outliers")

    log_list$"OUTLIER DETECTION: n_rows_pre_filter" <- nrow(workingfile_step5_out)

    cleaned_data_out <-
      workingfile_step5_out  %>%
      filter(is_outlier == FALSE)


    log_list$"OUTLIER DETECTION: n_rows_post_filter" <- nrow(cleaned_data_out)

    ##########################
    # Step 7 . Interpolate missing data and calculate MY totals
    ##########################
    print("Step 7 . Interpolate missing data and calculate MY totals")
    # This is only for MY total lactation calcs, not to be used in modelling. use 'cleaned_data_out' for modelling.
    interpolated_1 <-
      cleaned_data_out %>%
      group_by(ID) %>%
      filter(DIM >=10) %>%
      expand(DIM = 10:max(DIM)) %>% #Forces it to start on 10, for each of ID (due to grouping)
      left_join(cleaned_data_out , by = c('ID', 'DIM')) %>% # add data, leaves NA for missing DIM rows
      # set avg at DIM = 0 for interpolation

      mutate(my_int = zoo::na.approx(my, maxgap=Inf, na.rm=FALSE))

    interpolated_summary <-
      interpolated_1 %>%
      group_by(ID) %>%
      summarise(MY_305d_total = sum(my_int[between(DIM, 10, 305)], na.rm = TRUE),
                MY_305d = mean(my_int[between(DIM, 303, 307)], na.rm = TRUE)
      )

    #############
    #output lists
    print("OUTPUT...END")
    out <- list(cleaned_data = cleaned_data_out,
                log = log_list,
                plots = plots_list,
                total_MY_summary = interpolated_summary,
                interpolated_raw = interpolated_1)

    return(out)

  }

