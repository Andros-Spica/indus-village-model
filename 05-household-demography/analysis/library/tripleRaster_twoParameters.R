tripleRaster_twoParameters <- function(two_parameter_exploration_results, 
                                       parameter_x, parameter_y, 
                                       variable_1, variable_2, variable_3,
                                       parameter_x_label = NULL, parameter_y_label = NULL, 
                                       variable_1_label = NULL, variable_2_label = NULL, variable_3_label = NULL,
                                       plot_scale = 1, 
                                       variable_3_print_values = TRUE, variable_3_value_label_size = 0.5, 
                                       variable_3_point_shape = 1, variable_3_point_size = c(0.2, 1), variable_3_point_number_legend_keys = 3,
                                       axis_title_size = 3.5, axis_text_size = 2, legend_title_size = 3.5, legend_text_size = 2)
{
  if (is.null(parameter_x_label)) {
    parameter_x_label = parameter_x
  }
  if (is.null(parameter_y_label)) {
    parameter_y_label = parameter_y
  }
  if (is.null(variable_1_label)) {
    variable_1_label = variable_1
  }
  if (is.null(variable_2_label)) {
    variable_2_label = variable_2
  }
  
  variable_1_min = min(two_parameter_exploration_results[,variable_1])
  variable_1_max = max(two_parameter_exploration_results[,variable_1])
  variable_2_min = min(two_parameter_exploration_results[,variable_2])
  variable_2_max = max(two_parameter_exploration_results[,variable_2])
  variable_3_min = min(two_parameter_exploration_results[,variable_3])
  variable_3_max = max(two_parameter_exploration_results[,variable_3])
  
  thePlot <-
    ggplot(two_parameter_exploration_results, 
           aes_string(x = parameter_x, 
                      y = parameter_y, 
                      label = as.character(variable_3))) +
    geom_raster(aes_string(fill = variable_1)) +
    geom_point(aes_string(color = variable_2),
               shape = 15,
               size = plot_scale * 1.5) 
  
  if (variable_3_print_values)
  {
    thePlot <- thePlot + geom_text(size = plot_scale * variable_3_value_label_size)
  } else {
    thePlot <- thePlot + geom_point(aes_string(size = variable_3), shape = variable_3_point_shape)
  }
  
  thePlot <- thePlot +
    scale_fill_gradientn(
      colors = c('white', 'darkblue'),
      values = scales::rescale(c(variable_1_min, variable_1_max)),
      limits = c(variable_1_min, variable_1_max)
    ) +
    scale_color_gradientn(
      colors = c('white', 'darkred'),
      values = scales::rescale(c(variable_2_min, variable_2_max)),
      limits = c(variable_2_min, variable_2_max)
    ) +
    scale_size(
      breaks = seq(variable_3_min, variable_3_max, length.out = variable_3_point_number_legend_keys),
      limits = c(variable_3_min, variable_3_max),
      range = c(plot_scale * variable_3_point_size[1], plot_scale * variable_3_point_size[2])
    ) +
    labs(x = parameter_x_label,
         y = parameter_y_label,
         fill = variable_1_label,
         color = variable_2_label,
         size = variable_3_label) +
    guides(
      color = guide_colorbar(order = 0),
      fill = guide_colorbar(order = 1),
      size = guide_legend(order = 2)
    ) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      axis.title = element_text(size = plot_scale * axis_title_size),
      axis.text = element_text(size = plot_scale * axis_text_size),
      legend.title = element_text(size = plot_scale * legend_title_size),
      legend.text = element_text(size = plot_scale * legend_text_size)
    )
  
  thePlot
}
