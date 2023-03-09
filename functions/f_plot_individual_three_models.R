# Function to plot individual fitted lactation curves on observed MY data
# Facets by lactation ID, with 3 plots per column
# David Innes

# Inputs:
#   df_fit_stats = a dataframe of the combined_fit_stats; output from fitting
#   df_preds_fitted = a dataframe of combined_preds_fitted; output from fitting
#   IDs = a vector of lactation IDs to plot
#   error_rank =  a string of either 'CCC' or 'MAE' to indicate which error to rank by (MAE as %)
#   y_label_loc = number in same units as y-axis of where to add the labels
#   ymax = limit of y axis, will filter data below this number.
#   plot_title = a string of the title to give the plot

# Output: a ggplot

# this function will return MAE_perc and CCC for all in label. Ranked on CCC by default.

f_plot_individual_three_models_2_errors <-
  function(df_fit_stats,
           df_preds_fitted,
           IDs,
           error_rank = "CCC",
           y_label_loc = 5,
           ymax = 60,
           plot_title = "Title"){

    label_data <-
      df_fit_stats %>%
      select(ID, CCC,MAE_perc, Model) %>%
      filter(ID %in% IDs) %>%
      pivot_wider(names_from = Model, values_from= c(CCC, MAE_perc)) %>%
      rowwise() %>%
      mutate(mean_ERROR = mean(c_across(starts_with(error_rank)))) %>%
      mutate(across(contains(c("CCC", "MAE")), ~case_when(.x >= 10 ~ format(signif(.x, 3), nsmall = 1),
                                                          .x < 10 & .x >= 1 ~ format(signif(.x, 3), nsmall = 2),
                                                          .x < 1 ~ format(signif(.x, 2), nsmall = 2)
      ))) %>%

      mutate(ERROR_label = str_c( "Dijkstra CCC: ", CCC_Dijkstra, "  MAE: ", MAE_perc_Dijkstra , # nsmall keeps 0 to right of decimal
                                  "\nWilmink CCC: ", CCC_Wilmink, "  MAE: ", MAE_perc_Wilmink,
                                  "\n   Wood CCC: ", CCC_Wood, "  MAE: ", MAE_perc_Wood
      )) %>%
      arrange(mean_ERROR)

    p <-
      df_preds_fitted %>%
      filter(ID %in% IDs) %>%
      left_join(label_data) %>%
      mutate(ID = factor(ID, ordered=TRUE,  levels = as_factor(IDs))) %>%
      filter(.fitted<= ymax) %>%
      ggplot()+
      geom_point(aes(x=DIM, y=my))+
      geom_line(aes(x=DIM, y=.fitted, colour = Model), linewidth = 1)+
      scale_x_continuous(n.breaks = 10)+
      labs(title = plot_title)+
      ggpubr::theme_classic2()+
      viridis::scale_colour_viridis(discrete = TRUE, option = "H", begin = 0.15, end = 0.85)+
      facet_wrap(facets = "ID", ncol=3, scales='free_x')+
      scale_y_continuous( n.breaks = 10, limits = c(0,ymax))+
      ylab("Milk Yield (kg/d)")+
      xlab("Days in milk (DIM)")+
      geom_text(aes(label = ERROR_label, x = maxDIM / 2.35, y = y_label_loc), inherit.aes=FALSE)

    return(p)
  }
