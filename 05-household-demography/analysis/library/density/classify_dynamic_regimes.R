classify_regulation_regimes <- function(
    trajectories,
    overshoot_magnitude_threshold = 0.25,
    overshoot_time_threshold = 0.10,
    oscillatory_threshold = 0.25
) {

  trajectories |>

    mutate(

      regulation_regime = case_when(

        survival == "Extinction"
          ~ "Collapse",

        overshoot_magnitude > overshoot_magnitude_threshold &
          time_above_capacity > overshoot_time_threshold
          ~ "Overshooting",

        equilibrium_pressure_cv > oscillatory_threshold
          ~ "Oscillatory",

        TRUE
          ~ "Stable"
      )

    )

}
