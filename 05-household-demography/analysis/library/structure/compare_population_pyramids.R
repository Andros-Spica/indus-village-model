library(dplyr)
library(ggplot2)

# ============================================================
# Compare two pyramid datasets
# ============================================================

compare_population_pyramids <- function(
    pyramid_1,
    pyramid_2,
    label_1 = "Group 1",
    label_2 = "Group 2",
    relative = TRUE
) {

    comparison_df <- pyramid_1 %>%

        select(
            sex,
            age_bin,
            median_1 = median
        ) %>%

        left_join(

            pyramid_2 %>%

                select(
                    sex,
                    age_bin,
                    median_2 = median
                ),

            by = c(
                "sex",
                "age_bin"
            )
        )

    if (relative) {

        comparison_df <- comparison_df %>%

            mutate(

                difference =
                    (median_1 - median_2) /
                    ((median_1 + median_2) / 2)
            )

    } else {

        comparison_df <- comparison_df %>%

            mutate(
                difference =
                    median_1 - median_2
            )
    }

    comparison_df <- comparison_df %>%

        mutate(

            difference_plot =
                ifelse(
                    sex == "Male",
                    -difference,
                    difference
                ),

            comparison =
                paste(
                    label_1,
                    "vs",
                    label_2
                )
        )

    return(comparison_df)
}

# ============================================================
# Plot pyramid comparison
# ============================================================

plot_pyramid_comparison <- function(
    comparison_df,
    midpoint = 0
) {

    comparison_df <- comparison_df %>%

        mutate(
            age_bin = factor(
                age_bin,
                levels = rev(levels(age_bin))
            )
        )

    ggplot(
        comparison_df,
        aes(
            x = difference_plot,
            y = age_bin,
            fill = difference
        )
    ) +

        geom_col() +

        scale_x_continuous(
            labels = abs
        ) +

        scale_fill_gradient2(
            low = "#3b4cc0",
            mid = "white",
            high = "#b40426",
            midpoint = midpoint,
            name = "Difference"
        ) +

        labs(
            x = "Relative demographic difference",
            y = "Age group"
        ) +

        theme_minimal() +

        theme(
            panel.grid.minor = element_blank()
        )
}
