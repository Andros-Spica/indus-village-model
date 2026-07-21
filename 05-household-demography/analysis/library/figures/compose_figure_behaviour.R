compose_figure_behaviour <- function(
    endstates,
    sampled_trajectories,
    group_var = "survival",
    facet_var = "residence_rule",
    y_threshold_var = NULL,
    bins = 40,
    file_name = "figure_behaviour",
    width = 12,
    height = 10,
    heights = c(1, 1),
    outcome_palette = SURVIVAL_PALETTE,
    legend_title = "",
    legend_position = "bottom",
    strip_text_size = 8
) {
    outcome_spec <- NULL

    if (group_var == "survival") {
        model_versions <- unique(
            c(
                as.character(endstates$model_version),
                as.character(sampled_trajectories$model_version)
            )
        )

        outcome_spec <- find_model_versions_subset(
            version_var = model_versions,
            outcome_palette = outcome_palette
        )

        endstates <- endstates |>
            mutate(
                survival = factor(
                    survival,
                    levels = outcome_spec$levels
                )
            )

        sampled_trajectories <- sampled_trajectories |>
            mutate(
                survival = factor(
                    survival,
                    levels = outcome_spec$levels
                )
            )
    }

    max_pop <- max(
        endstates$log_totalIndividuals,
        na.rm = TRUE
    )

    endstates_distrib_size_plot <- endstates |>
        plot_endstate_variable_distribution(
            main_var = "log_totalIndividuals",
            fill_var = group_var,
            facet_var = facet_var,
            bins = bins,
            x_axis_label = "Final population size",
            y_axis_label = "Simulation count",
            x_axis_log10 = TRUE,
            x_max = max_pop * 1.1,
            y_axis_log10 = TRUE,
            legend_position = "inside"
        )

    if (group_var == "survival") {
        endstates_distrib_size_plot <-
            endstates_distrib_size_plot +
            scale_fill_manual(
                values = outcome_spec$palette,
                limits = outcome_spec$levels,
                breaks = outcome_spec$levels,
                drop = FALSE
            )
    }

    endstates_distrib_size_plot <-
        endstates_distrib_size_plot +
        guides(fill = "none")

    pop_trajectories_plot <- sampled_trajectories |>
        plot_trajectories(
            y_var = "log_totalIndividuals",
            trajectory_id = "run_unique_id",
            line_alpha = 0.15,
            median_alpha = 0.8,
            median_linewidth = 1.2,
            y_axis_log10 = TRUE,
            y_axis_label = "number of individuals",
            group_var = group_var,
            facet_var = facet_var,
            facet_ncol = 2,
            facet_scales = "fixed",
            y_threshold_var = y_threshold_var,
            thresholds_linetype = 2,
            legend_position = legend_position
        )

    if (group_var == "survival") {
        pop_trajectories_plot <-
            pop_trajectories_plot +
            scale_colour_manual(
                values = outcome_spec$palette,
                limits = outcome_spec$levels,
                breaks = outcome_spec$levels,
                drop = FALSE,
                guide = guide_legend(
                    title = legend_title,
                    override.aes = list(
                        linewidth = 5,
                        alpha = 1
                    ),
                    position = legend_position
                )
            )
    }

    combined_figure <-
        endstates_distrib_size_plot /
        pop_trajectories_plot +
        plot_layout(
            heights = heights
        ) +
        plot_annotation(
            tag_levels = "A"
        ) &
        theme(
            plot.title = element_text(
                size = 14,
                face = "bold"
            ),
            plot.tag = element_text(
                size = 14 ,
                face = "bold"
            ),
            plot.tag.position = "topleft",
            plot.margin = ggplot2::margin(
                t = 5 ,
                r = 5 ,
                b = 5 ,
                l = 5 
            ),
            axis.title = element_text(
                size = 9 
            ),
            axis.text = element_text(
                size = 5
            ),
            legend.title = element_text(
                size = 9 
            ),
            legend.text = element_text(
                size = 8 
            ),
            strip.text = element_text(
                size = strip_text_size
            )
        )

    export_figure(
        combined_figure,
        paste0("figures/", file_name),
        width = width,
        height = height
    )
}

find_model_versions_subset <- function(
    version_var,
    outcome_palette = SURVIVAL_PALETTE
) {
    model_versions <- version_var |>
        as.character() |>
        unique() |>
        stats::na.omit()

    unknown_versions <- setdiff(
        model_versions,
        c(
            BASELINE_MODEL_VERSIONS,
            DENSITY_MODEL_VERSIONS
        )
    )

    if (length(unknown_versions) > 0) {
        stop(
            "Unknown model version(s): ",
            paste(unknown_versions, collapse = ", ")
        )
    }

    includes_baseline <- any(
        model_versions %in% BASELINE_MODEL_VERSIONS
    )

    includes_density <- any(
        model_versions %in% DENSITY_MODEL_VERSIONS
    )

    outcome_family <- dplyr::case_when(
        includes_baseline && includes_density ~ "mixed",
        includes_baseline ~ "baseline",
        includes_density ~ "density",
        TRUE ~ NA_character_
    )

    if (is.na(outcome_family)) {
        stop("No recognised model versions were supplied.")
    }

    outcome_levels <- switch(
        outcome_family,
        baseline = SURVIVAL_LEVELS_BASELINE,
        density = SURVIVAL_LEVELS_DENSITY,
        mixed = SURVIVAL_LEVELS_MIXED
    )

    missing_colours <- setdiff(
        outcome_levels,
        names(outcome_palette)
    )

    if (length(missing_colours) > 0) {
        stop(
            "The outcome palette has no colour for: ",
            paste(missing_colours, collapse = ", ")
        )
    }

    list(
        family = outcome_family,
        model_versions = model_versions,
        levels = outcome_levels,
        palette = outcome_palette[outcome_levels]
    )
}
