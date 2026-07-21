plot_household_structure_faceted_fixed <- function(
    endstates,
    color_var = "max_couple_count_distribution_max",
    point_alpha = 0.08,
    facet_var = "residence_rule",
    facet_ncol = 2,
    facet_scales = "fixed",
    xy_log10 = TRUE,
    x_axis_label = "number of households",
    y_axis_label = "number of individuals",
    color_label = "Max. couple count allowed in a household\n(parametric maximum)",
    legend_position = "bottom",
    rich_text_size = 3.2
) {
    # Ensure ggtext is installed for handling the line breaks cleanly
    if (!requireNamespace("ggtext", quietly = TRUE)) {
        stop("Package 'ggtext' is required for multi-line annotations. Please install it.")
    }

    # 1. Standardize dynamic column selections
    x_var <- if (xy_log10) "log_totalHouseholds" else "totalHouseholds"
    y_var <- if (xy_log10) "log_totalIndividuals" else "totalIndividuals"

    # 2. Filter survival data
    cleaned_data <- endstates |>
        dplyr::filter(survival != "Extinction")

    if (!is.null(facet_var) && length(facet_var) != 1) {
        stop("This implementation requires a maximum of one 'facet_var' string.")
    }

    # 3. Define grouping variable for summarization
    group_var <- facet_var
    if (is.null(facet_var)) {
        group_var <- color_var
    }

    # 4. Compute metrics and build an HTML/Markdown string for ggtext
    facet_annotations <- cleaned_data |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::group_modify(~ {
            df_group <- .x
            x_vals <- df_group[[x_var]]
            y_vals <- df_group[[y_var]]
            
            # Household Ratio calculations
            mean_size <- mean(df_group$totalIndividuals / df_group$totalHouseholds, na.rm = TRUE)
            sd_size   <- sd(df_group$totalIndividuals / df_group$totalHouseholds, na.rm = TRUE)
            
            # Linear model on the exact data being plotted (log data if xy_log10 is TRUE)
            fit <- lm(y_vals ~ x_vals)
            fit_summary <- summary(fit)
            
            intercept_val <- coef(fit)[1]
            slope_val     <- coef(fit)[2]
            r_squared     <- fit_summary$r.squared
            
            # Use HTML/Markdown for guaranteed clean multi-line stacking
            markdown_text <- paste0(
                "**Avg:** ", round(mean_size, 2), " &plusmn; ", round(sd_size, 2), " SD<br>",
                "*y* = ", round(intercept_val, 2), " + ", round(slope_val, 2), "*x*<br>",
                "*R*<sup>2</sup> = ", round(r_squared, 3)
            )
            
            tibble::tibble(label_str = markdown_text)
        }) |>
        dplyr::ungroup()

    # 5. Smart placement coordinates (top-left corner of each facet plot area)
    x_min <- min(cleaned_data[[x_var]], na.rm = TRUE)
    x_max <- max(cleaned_data[[x_var]], na.rm = TRUE)
    y_min <- min(cleaned_data[[y_var]], na.rm = TRUE)
    y_max <- max(cleaned_data[[y_var]], na.rm = TRUE)

    facet_annotations <- facet_annotations |>
        dplyr::mutate(
            x_pos = x_min + (x_max - x_min) * 0.03,
            y_pos = y_max - (y_max - y_min) * 0.05
        )

    # 6. Dynamic tick boundaries
    x_max_tick <- ceiling(x_max)
    y_max_tick <- ceiling(y_max)

    # 7. Build the ggplot
    household_plot <- cleaned_data |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
        ggplot2::geom_point(ggplot2::aes(color = .data[[color_var]]), alpha = point_alpha) +
        
        # Trend line
        ggplot2::geom_smooth(
            ggplot2::aes(color = NULL), 
            method = "lm", 
            formula = y ~ x, 
            color = "red", 
            se = FALSE, 
            linewidth = 1
        ) +
        
        # Fixed multi-line text renderer using ggtext
        ggtext::geom_richtext(
            data = facet_annotations,
            ggplot2::aes(x = x_pos, y = y_pos, label = label_str),
            hjust = 0, 
            vjust = 1, # Aligns text downward from the top coordinate boundary
            size = rich_text_size,
            color = "black",
            fill = NA,          # Removes background box
            label.color = NA,   # Removes border box
            label.padding = ggplot2::unit(c(0, 0, 0, 0), "lines") # Strips margins
        )

    # 8. Axis Scale adjustments
    if (xy_log10) {
        household_plot <- household_plot +
            ggplot2::scale_y_continuous(breaks = c(0, 1:y_max_tick), labels = c(0, 10^(1:y_max_tick))) +
            ggplot2::scale_x_continuous(breaks = c(0, 1:x_max_tick), labels = c(0, 10^(1:x_max_tick)))
    }

    # 9. Facet Layout
    if (!is.null(facet_var)) {
        if (!facet_var %in% colnames(cleaned_data)) {
            stop("Facet variable not found in data frame")
        }
        household_plot <- household_plot +
            ggplot2::facet_wrap(
                as.formula(paste("~", facet_var)),
                ncol = facet_ncol,
                scales = facet_scales
            )
    }

    # 10. Finalize plot aesthetics
    household_plot <- household_plot  +
        ggplot2::labs(
            x = x_axis_label,
            y = y_axis_label,
            color = color_label
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = legend_position,
            strip.text = ggplot2::element_text(face = "bold", size = 11)
        )
    
    return(household_plot)
}
