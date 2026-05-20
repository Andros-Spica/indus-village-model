aggregate_age_structure <- function(
    tidy_df,
    group_vars = NULL,
    smooth = FALSE
) {

    grouping <- c(group_vars, "sex", "age_bin")

    tidy_df |>
        group_by(across(all_of(grouping))) |>
        summarise(
            mean = mean(count),
            median = median(count),
            q25 = quantile(count, 0.25),
            q75 = quantile(count, 0.75),
            .groups = "drop"
        ) |>
        group_by(
        residence_rule,
        sex
    ) |>

    arrange(age_bin) |>

    mutate(

        median =
            zoo::rollmean(
                median,
                k = 5,
                fill = "extend"
            ),

        q25 =
            zoo::rollmean(
                q25,
                k = 3,
                fill = "extend"
            ),

        q75 =
            zoo::rollmean(
                q75,
                k = 3,
                fill = "extend"
            )
    )
}
