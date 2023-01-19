# Function to plot individual fitted lactation curves on observed MY data
# Facets by lactation ID, with 3 plots per column
# David Innes

# Inputs:
#   df_fit_stats = a dataframe of the combined_fit_stats; output from fitting
#   df_preds_fitted = a dataframe of combined_preds_fitted; output from fitting
#   Ids = a vector of lactation IDs to plot
#   plot_title = a string of the title to give the plot

# Output: a ggplot

f_plot_individual_three_models <- function(df_fit_stats, df_preds_fitted, IDs, y_label_loc = 5, plot_title = "Title"){

  label_data_MAE <-
    df_fit_stats %>%
    select(ID, MAE, Model) %>%
    filter(ID %in% IDs) %>%
    pivot_wider(names_from = Model, values_from= MAE) %>%
    rowwise() %>%
    mutate(mean_MAE = mean(c_across(where(is.double)))) %>%
    ungroup() %>%
    mutate(MAE_label = str_c("Dijkstra MAE: ", round(Dijkstra,2),
                             '\n  Wood MAE: ', round(Wood,2),
                             '\nWilmink MAE: ', round(Wilmink,2))) %>%
    arrange(mean_MAE)

  # Calculate a modified x-axis to ensure variable x-axis scales are at least n long
  maxDIM_data <- df_fit_stats %>%
    select(ID, maxDIM) %>%
    distinct() %>%
    filter(ID %in% IDs) %>%
    mutate(ID = factor(ID, ordered=TRUE,  levels = as_factor(IDs))) %>%
    mutate(maxD = if_else(maxDIM < 450, true = as.integer(450), false =  maxDIM))


  p <-
    df_preds_fitted %>%
    filter(ID %in% IDs) %>%
    left_join(label_data_MAE) %>%
    mutate(ID = factor(ID, ordered=TRUE,  levels = as_factor(IDs))) %>%
    filter(.fitted<= 60) %>%
    ggplot()+
    geom_point(aes(x=DIM, y=my))+
    geom_line(aes(x=DIM, y=.fitted, colour = Model), size = 1)+
    geom_blank(aes(x = maxD), data = maxDIM_data)+ #modified max days
    scale_x_continuous(n.breaks = 10)+
    labs(title = plot_title)+
    ggpubr::theme_classic2()+
    viridis::scale_colour_viridis(discrete = TRUE, option = "H", begin = 0.15, end = 0.85)+
    facet_wrap(facets = "ID", ncol=3,
               scales='free_x'
    )+
    scale_y_continuous( n.breaks = 10, limits = c(0,60))+
    ylab("Milk Yield (kg/d)")+
    xlab("Days in milk (DIM)")+
    geom_text(aes(label = MAE_label, x = 100, y = y_label_loc), inherit.aes=FALSE)

  return(p)
}
