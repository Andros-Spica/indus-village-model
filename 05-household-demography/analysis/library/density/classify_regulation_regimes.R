classify_regulation_regimes <- function(
    trajectories,
    overshoot_magnitude_threshold = 0.25,
    overshoot_time_threshold = 0.10,
    oscillatory_threshold = 0.25,
    stress_threshold = 0.5
) {

  trajectories |>

    mutate(

      regulation_regime = case_when(

        survival == "Extinction"
          ~ "Collapse",

        overshoot_magnitude > overshoot_magnitude_threshold &
          time_above_capacity > overshoot_time_threshold
          ~ "Overshooting",

        equilibrium_pressure_mean > stress_threshold &
          equilibrium_pressure_cv > oscillatory_threshold &
          final_pressure > stress_threshold
          ~ "Overshoot-prone",

        equilibrium_pressure_mean > stress_threshold &
          equilibrium_pressure_cv <= oscillatory_threshold &
          final_pressure > stress_threshold
          ~ "Stress-prone",

        equilibrium_pressure_mean > stress_threshold &
          equilibrium_pressure_cv > oscillatory_threshold &
          final_pressure <= stress_threshold
          ~ "Overshoot-prone\n(collapsing)",

        equilibrium_pressure_mean > stress_threshold &
          equilibrium_pressure_cv <= oscillatory_threshold &
          final_pressure <= stress_threshold
          ~ "Stress-prone\n(collapsing)",

        equilibrium_pressure_mean <= stress_threshold &
          equilibrium_pressure_cv > oscillatory_threshold &
          final_pressure > stress_threshold
          ~ "Oscillatory\n(growing)",

        equilibrium_pressure_cv <= oscillatory_threshold &
          equilibrium_pressure_mean <= stress_threshold &
          final_pressure > stress_threshold
          ~ "Stable\n(growing)",

        equilibrium_pressure_cv > oscillatory_threshold &
          equilibrium_pressure_mean <= stress_threshold &
          final_pressure <= stress_threshold
          ~ "Oscillatory",

        equilibrium_pressure_cv <= oscillatory_threshold &
          equilibrium_pressure_mean <= stress_threshold &
          final_pressure <= stress_threshold
          ~ "Stable",

        .default = "Unclassified"
      )

    ) |>
    mutate(
      regulation_regime = factor(
        regulation_regime,
        levels = c(
          "Collapse",
          "Stable",
          "Stable\n(growing)",
          "Oscillatory",
          "Oscillatory\n(growing)",
          "Stress-prone\n(collapsing)",
          "Stress-prone",
          "Overshoot-prone\n(collapsing)",
          "Overshoot-prone",
          "Overshooting",
          "Unclassified"
        )
      )
    )

}

simplify_regulation_regime <- function(
    data
) {

    stopifnot("regulation_regime" %in% names(data))

    data |>
        mutate(
          regulation_regime_simple = dplyr::case_when(
                regulation_regime %in% c(
                    "Stable",
                    "Stable\n(growing)"
                ) ~ "Stable",

                regulation_regime %in% c(
                    "Oscillatory",
                    "Oscillatory\n(growing)",
                    "Overshoot-prone",
                    "Overshoot-prone\n(collapsing)"
                ) ~ "Oscillatory",

                regulation_regime %in% c(
                    "Stress-prone",
                    "Stress-prone\n(collapsing)"
                ) ~ "Stress-prone",

                regulation_regime == "Overshooting" ~
                    "Overshooting",

                regulation_regime == "Collapse" ~
                    "Collapse",

                TRUE ~
                    "Unclassified"
            ),
            regulation_regime_simple = factor(
                regulation_regime_simple,
                levels = c(
                    "Stable",
                    "Oscillatory",
                    "Stress-prone",
                    "Overshooting",
                    "Collapse",
                    "Unclassified"
                )
            )
        )
}
