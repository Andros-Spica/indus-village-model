plot_category_proportion <- function(
    df,
    category_var,
    category_fill = TRUE,
    category_labs_size = 5,
    category_labs_angle = 45,
    category_labs_hjust = 0.5,
    facet_var = "residence_rule",
    facet_ncol = 2,
    facet_scales = "fixed"
) {
    category_prop_plot <- NULL

    if (category_fill)
    {
        category_prop_plot <- df |>
        ggplot(
            aes(
                .data[[category_var]],
                fill = .data[[category_var]]
            )
        ) +
        guides(
            fill = "none"
        )

        if (category_var == "regulation_regime") {
            # use colour scheme for regulation_regime
            category_prop_plot <- category_prop_plot +
                scale_color_viridis_d(
                    option = "turbo",
                    drop = FALSE,
                    guide = guide_legend(
                        title = "",
                        override.aes = list(linewidth = 5, alpha = 1),
                        reverse = TRUE
                    )
                )
            # schemes for other variables can be added here or after this function
        }
    } else {
        category_prop_plot <-  df |>
        ggplot(
            aes(
                .data[[category_var]]
            )
        )
    }

    category_prop_plot <- category_prop_plot +
        geom_bar() +
        labs(
            x = ""
        ) +
        theme(
            axis.text.x = element_text(
                size = category_labs_size,
                angle = category_labs_angle,
                hjust = category_labs_hjust
            )
        )

    if (!is.null(facet_var)) {
        # TO-DO: add check
        category_prop_plot <- category_prop_plot +
            facet_wrap(
                as.formula(paste("~", facet_var)),
                ncol = facet_ncol,
                scales = facet_scales
            )
    }
    
    category_prop_plot
}

plot_category_proportion_stacked <- function(
    df,
    x_var,
    fill_var,
    facet_var = "residence_rule",
    facet_ncol = 2,
    facet_scales = "fixed",
    y_axis_label = "Share of simulations"
) {
    category_prop_stacked_plot <- NULL

    # Optional faceting
    if (!is.null(facet_var)) {

        if (!all(facet_var %in% colnames(df))) {
            stop("Facet variable(s) not found in data frame")
        }
        
        if (length(facet_var) == 1) {
            category_prop_stacked_plot <- df |>
                count(
                    .data[[x_var]], 
                    .data[[facet_var]], 
                    .data[[fill_var]]) |>
                group_by(
                    .data[[x_var]], 
                    .data[[facet_var]]
                    ) |>
                mutate(freq = n / sum(n)) |>
                ggplot(
                    aes(
                        .data[[x_var]], 
                        freq, 
                        fill = .data[[fill_var]]
                    )
                ) +
                facet_wrap(
                    as.formula(
                        paste("~", facet_var)
                    )
                )
        } else if (length(facet_var) == 2) {
            category_prop_stacked_plot <- df |>
                count(
                    .data[[x_var]], 
                    .data[[facet_var[1]]],
                    .data[[facet_var[2]]], 
                    .data[[fill_var]]) |>
                group_by(
                    .data[[x_var]], 
                    .data[[facet_var[1]]],
                    .data[[facet_var[2]]]
                    ) |>
                mutate(freq = n / sum(n)) |>
                ggplot(
                    aes(
                        .data[[x_var]], 
                        freq, 
                        fill = .data[[fill_var]]
                    )
                ) +
                facet_grid(
                    as.formula(
                        paste(facet_var[1], "~", facet_var[2])
                    )
                )
        } else {
            stop("Facet variable must be of length 1 or 2")
        } 
    } else {
        category_prop_stacked_plot <- df |>
            count(
                .data[[x_var]],
                .data[[fill_var]]
                ) |>
            mutate(freq = n / sum(n)) |>
            ggplot(
                aes(
                    .data[[x_var]], 
                    freq, 
                    fill = .data[[fill_var]]
                )
            )
    }

    category_prop_stacked_plot <- category_prop_stacked_plot +
        geom_col(position = "fill") +
        scale_y_continuous(labels = percent) +
        labs(
            x = NULL,
            y = y_axis_label,
            fill = ""
        )

    if (fill_var == "survival" & any(df$survival == "Extinction")) {
        # use colour scheme for survival
        category_prop_stacked_plot <- category_prop_stacked_plot +
            scale_fill_viridis_d(
                begin = 1,
                end = 0,
                option = "turbo",
                guide = guide_legend(
                    title = "",
                    override.aes = list(linewidth = 5, alpha = 1),
                    reverse = TRUE
                )
            )
    } else if (fill_var == "survival" & any(df$survival != "Extinction")) {
        cut_limit <- if_else(
            nlevels(df$survival) == 3, # base model
            0.5,
            0.65
        )
        category_prop_stacked_plot <- category_prop_stacked_plot +
            scale_fill_viridis_d(
                begin = cut_limit,
                end = 0,
                option = "turbo",
                guide = guide_legend(
                    title = "",
                    override.aes = list(linewidth = 5, alpha = 1),
                    reverse = TRUE
                )
            )
    }
    # schemes for other variables can be added here or after this function

    category_prop_stacked_plot
}