# Plots of average curves using mean of fitted/predicted values
#
# David Innes


# INPUTS:
#   - combined_preds_fitted_in = df. output from fit_3_lac_models().
#       Contains columns: Model, DIM, .fitted, my, .resid and the column used in groups_by.
#   - groups_by = name of column to group by for plot e.g. Parity_class or Lac_len_group
#   - plot_n_colour = either: FALSE (no colour) or TRUE (for normal n) or
#                        a string suitable for `trans =` e.g. 'log', 'log2', 'log10'
#   - plot_residuals = bool

# OUTPUT: a plot

f_plot_mean_lac_curves <- function(combined_preds_fitted_in,
                                  groups_by,
                                  plot_n_colour = FALSE,
                                  plot_residuals = FALSE){

  # plot_n_colour can be FALSE or TRUE (for normal n) or a string suitable for trans =
  # e.g. 'log', 'log2', 'log10'


  # Calculate means, and n
  mean_preds <-
    combined_preds_fitted_in %>%
    mutate(Parity_class = factor(Parity_class, levels = c("Primiparous", "Multiparous")),
           Model = factor(Model, levels = c('Dijkstra', 'Wood', 'Wilmink'))) %>%
    group_by({{ groups_by }}, Model, DIM) %>%
    summarise(mean_predicted_my = mean(.fitted),
              sd_predicted_my = sd(.fitted),
              mean_observed_my = mean(my, na.rm=TRUE),
              mean_residual = mean(.resid, na.rm=TRUE),
              n = n())


  if(plot_residuals == FALSE){

    if (plot_n_colour == FALSE){
      p <-
        mean_preds %>%
        ggplot()+
        geom_point(aes(x=DIM, y = mean_observed_my), alpha=0.25, size = 1.5)+
        geom_line(aes(x = DIM, y = mean_predicted_my, colour = Model), linesize=1, alpha=0.95)+
        viridis::scale_colour_viridis(discrete=TRUE, option = "D", begin = 0.2, end = 0.9)


    } else if(plot_n_colour == TRUE){
      p <-
        mean_preds %>%
        ggplot()+
        geom_point(aes(x=DIM, y = mean_observed_my, colour = n), alpha=0.25, size = 1.5)+
        geom_line(aes(x = DIM, y = mean_predicted_my), linesize=1, alpha=0.95)+
        viridis::scale_color_viridis(direction = -1, option = "H", labels = ~round(.,0), begin = 0.1)


    } else if(is.character(plot_n_colour)){
      # uses plot_n_colour as transformation
      p <-
        mean_preds %>%
        ggplot()+
        geom_point(aes(x=DIM, y = mean_observed_my, colour = n), alpha=0.25, size = 1.5)+
        geom_line(aes(x = DIM, y = mean_predicted_my), linesize=1, alpha=0.95)+
        viridis::scale_color_viridis(direction = -1, option = "H", trans = plot_n_colour, labels = ~round(.,0), begin = 0.1)

    } else {
      stop('Check plot_n_colour and try again')
    }

    # add extra formatting and faceting

    p <- p +
      facet_grid(cols=vars( {{ groups_by }} ), rows=vars(Model))+
      ylab("Milk Yield (kg/d)")+
      xlab("Days in milk (DIM)")+
      scale_x_continuous(n.breaks = 10)+
      ggpubr::theme_classic2()

    return(p)
  }

  if(plot_residuals == TRUE){

    p_resid <-
      mean_preds %>%
      filter(DIM != 0) %>% # huge variability on day 0 wrecks interpretation
      ggplot()+
      geom_hline(aes(yintercept = 0), colour = 'black')+
      geom_point(aes(x = DIM, y = mean_residual, colour = Model), size=1.2, alpha=0.85)+
      facet_wrap(facets=as_label(enexpr(groups_by)), ncol = 1)+
      viridis::scale_colour_viridis(discrete=TRUE, option = "D", begin = 0.2, end = 0.9)+
      scale_x_continuous(n.breaks=20)+
      ggpubr::theme_classic2()+
      ylab("Mean residuals")+
      xlab("Days in milk (DIM)")

    return(p_resid)
    }
}
