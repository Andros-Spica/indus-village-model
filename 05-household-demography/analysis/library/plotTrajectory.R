plotTrajectory <- function(melted_run_data, plot_scale = 1)
{
  require(ggplot2)
  
  ggplot(melted_run_data,
         aes(x = time, y = value, colour = variable), size = 2 * plot_scale) +
    geom_line(size = 0.2 * plot_scale) +
    ylab("population") + 
    scale_color_manual(labels = c('Humans', ' Plants'), values = c('blue', 'red')) +
    
    theme(axis.title = element_text(size = 3 * plot_scale),
          axis.text = element_text(size = 2 * plot_scale),
          legend.title = element_blank(),
          legend.text = element_text(size = 3 * plot_scale),
          legend.key.size = unit(0.2 * plot_scale, 'lines'))
}
