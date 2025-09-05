bifurcationPlotSimple <- function(parameter_values, 
                                  observed_variables, 
                                  parameter_x_label = "parameter", 
                                  variable_y_label = "observed variable",
                                  legend_position = c(0, 0),
                                  variable_names = NULL,
                                  col = NULL,
                                  cex = 1
)
{
  if (!is.null(ncol(observed_variables))) {
    
    if (is.null(variable_names)) {
      variable_names = paste("variable ", seq(0, ncol(observed_variables)))
    }
    
    if (is.null(col)) {
      col <- rainbow(ncol(observed_variables), v = 0.8)
    }
    
    plot(parameter_values,
         unlist(observed_variables[,1]),
         type = 'l',
         col = col[1],
         ylim = c(min(observed_variables), max(observed_variables)) * 1.01,
         ylab = variable_y_label,
         xlab = parameter_x_label,
         cex.axis = cex * 0.3,
         cex.lab = cex * 0.3,
         lwd = cex)
    
    for (v in 1:ncol(observed_variables))
    {
      points(parameter_values, unlist(observed_variables[,v]), type = 'l', col = col[v], lwd = cex)
      points(parameter_values, unlist(observed_variables[,v]), col = col[v],pch = 1, cex = cex)
    }
    
    legend(legend_position[1],legend_position[2], legend = variable_names, 
           col = col, lty = 1, lwd = cex, cex = cex * 0.3)
    
  } else {
    col <- "black"
    
    par(mar = c(5,5,1,1))
    plot(parameter_values,
         observed_variables,
         type = 'l',
         col = col,
         ylim = c(min(observed_variables), max(observed_variables)) * 1.01,
         ylab = variable_y_label,
         xlab = parameter_x_label,
         cex.axis = cex * 0.3,
         cex.lab = cex * 0.3,
         lwd = cex)
  }
}

bifurcationPlotPair <- function(result_A, result_B, 
                                parameter_x_label, variable_y_label_A, variable_y_label_B, 
                                plot_scale = 1, type = 'c')
{
  ylim = c(
    min(c(min(result_A[,2]), min(result_B[,2]))), 
    max(c(max(result_A[,2]), max(result_B[,2])))
  )
  
  layout(matrix(c(1, 2), ncol = 2))
  
  par(mar = plot_scale * c(1.2, 1.2, 0.1, 0.1),
      mgp = plot_scale * c(0.8, 0.3, 0),
      cex.lab = plot_scale * 0.5,
      cex.axis = plot_scale * 0.5,
      lwd = plot_scale
  )
  
  plot(result_A,
       pch = '.',
       xlab = parameter_x_label,
       ylab = variable_y_label_A,
       cex = plot_scale,
       type = type,
       ylim = ylim
  )
  
  plot(result_B,
       pch = '.',
       xlab = parameter_x_label,
       ylab = variable_y_label_B,
       cex = plot_scale,
       type = type,
       ylim = ylim
  )
}
