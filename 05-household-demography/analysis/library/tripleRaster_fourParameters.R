tripleRaster_fourParameters <- function(four_parameter_exploration_results,
                                        parameter_x, parameter_y, 
                                        parameter_strip_x, parameter_strip_y, 
                                        variable_1, variable_2, variable_3, 
                                        parameter_x_label = NULL, parameter_y_label = NULL, 
                                        variable_1_label = NULL, variable_2_label = NULL,  variable_3_label = NULL,
                                        plot_scale = 1, 
                                        variable_3_print_values = TRUE, variable_3_value_label_size = 0.4, 
                                        variable_3_point_shape = 1, variable_3_point_size = c(0.2, 1), variable_3_point_number_legend_keys = 3,
                                        axis_title_size = 3, axis_text_size = 1.5, 
                                        strip_text_size = 1, strip_parameter_name_max_length = 25, 
                                        strip_parameter_name_occlusion_keep_start_end = c(FALSE, TRUE),
                                        strip_parameter_name_occlusion_text = '(...)',
                                        legend_title_size = 3.5, legend_text_size = 2)
{
  if (is.null(parameter_x_label)) {
    parameter_x_label = parameter_small_x
  }
  if (is.null(parameter_y_label)) {
    parameter_y_label = parameter_small_y
  }
  if (is.null(variable_1_label)) {
    variable_1_label = variable_1
  }
  if (is.null(variable_2_label)) {
    variable_2_label = variable_2
  }
  
  variable_1_min = min(four_parameter_exploration_results[,variable_1])
  variable_1_max = max(four_parameter_exploration_results[,variable_1])
  variable_2_min = min(four_parameter_exploration_results[,variable_2])
  variable_2_max = max(four_parameter_exploration_results[,variable_2])
  variable_3_min = min(four_parameter_exploration_results[,variable_3])
  variable_3_max = max(four_parameter_exploration_results[,variable_3])
  
  # simplify parameter_small_x and parameter_small_y names if these are too long to be properly displayed
  four_parameter_exploration_results$parameter_strip_x_labels <- 
    simplifyNameForStripLabel(four_parameter_exploration_results,
                              parameter_strip_x, 
                              max_name_length = strip_parameter_name_max_length, 
                              keep_start_end = strip_parameter_name_occlusion_keep_start_end,
                              occlusion_text = strip_parameter_name_occlusion_text)
  four_parameter_exploration_results$parameter_strip_y_labels <- 
    simplifyNameForStripLabel(four_parameter_exploration_results,
                              parameter_strip_y, 
                              max_name_length = strip_parameter_name_max_length, 
                              keep_start_end = strip_parameter_name_occlusion_keep_start_end,
                              occlusion_text = strip_parameter_name_occlusion_text)
  
  thePlot <- 
  ggplot(four_parameter_exploration_results, aes_string(x = parameter_x, y = parameter_y, label = as.character(variable_3))) +
    geom_raster(aes_string(fill = variable_1)) +
    geom_point(aes_string(color = variable_2),
               shape = 15,
               size = plot_scale * 0.8)
  
  if (variable_3_print_values)
  {
    thePlot <- thePlot + geom_text(size = plot_scale * variable_3_value_label_size)
  } else {
    thePlot <- thePlot + geom_point(aes_string(size = variable_3), shape = variable_3_point_shape)
  }
  
  thePlot <- thePlot + 
    scale_fill_gradientn(
      colors = c('white', 'darkblue'),
      values = rescale(c(variable_1_min, variable_1_max)),
      limits = c(variable_1_min, variable_1_max)
    ) +
    scale_color_gradientn(
      colors = c('white', 'darkred'),
      values = rescale(c(variable_2_min, variable_2_max)),
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
    facet_grid(
      #parameter_strip_y_labels ~ parameter_strip_x_labels,
      reformulate(parameter_strip_x, parameter_strip_y), 
      scales='free', 
      as.table = FALSE,
      labeller = label_value
      #,labeller = label_both
    ) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text = element_text(size = plot_scale * strip_text_size),
      panel.grid.minor =  element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = plot_scale * axis_title_size),
      axis.text = element_text(size = plot_scale * axis_text_size),
      legend.title = element_text(size = plot_scale * legend_title_size),
      legend.text = element_text(size = plot_scale * legend_text_size)
    )
  
  thePlot
}

simplifyNameForStripLabel <- function(aDataFrame, 
                                      parameterName, 
                                      max_name_length = 20, 
                                      keep_start_end = c(FALSE, TRUE), 
                                      occlusion_text = '(...)')
{
  nameLength = nchar(parameterName)
  
  if (nameLength > max_name_length)
  {
    fragmentLenght = floor((max_name_length - nchar(occlusion_text)))# / 2)
    
    labelVariable <- factor(
      paste0(ifelse(keep_start_end[1],
                    substr(parameterName, 1, fragmentLenght),
                    ''),
             occlusion_text, 
             ifelse(keep_start_end[2],
                    substr(parameterName, nameLength - fragmentLenght + 1, nameLength), 
                    ''),
             ': ', aDataFrame[,parameterName]
      )
    )
  }
  else
  {
    labelVariable <- factor(
      paste0(parameterName, 
             ': ', aDataFrame[,parameterName])
    )
  }
  
  return(labelVariable)
}
