compute_density_metrics <- function(
    trajectories,
    burn_in_fraction = 0.5
) {

    trajectories |>
    group_by(run_number) |>
    mutate(
      # last registered pressure
      final_pressure =
        last(pressure),
      # maximum population size reached during the simulation
      peak_population =
        max(totalIndividuals),
      # maximum pressure reached during the simulation
      peak_pressure =
        max(pressure),
      # magnitude of overshoot relative to carrying capacity
      overshoot_magnitude =
        max(pressure) - 1,
      # mean population size during the equilibrium phase
      equilibrium_population_mean =
        mean(totalIndividuals[step >= max(step) * burn_in_fraction]),
      # coefficient of variation of population size during the equilibrium phase
      equilibrium_population_variance =
        var(totalIndividuals[step >= max(step) * burn_in_fraction]),
      # coefficient of variation of population size during the equilibrium phase (relative to mean)
      equilibrium_population_cv =
        equilibrium_population_variance / equilibrium_population_mean,
      # mean pressure during the equilibrium phase
      equilibrium_pressure_mean =
        mean(pressure[step >= max(step) * burn_in_fraction]),
      # coefficient of variation of pressure during the equilibrium phase
      equilibrium_pressure_variance =
        var(pressure[step >= max(step) * burn_in_fraction]),
      # coefficient of variation of pressure during the equilibrium phase (relative to mean)
      equilibrium_pressure_cv =
        equilibrium_pressure_variance / equilibrium_pressure_mean,
      # ratio of mean pressure to mean population size during the equilibrium phase.
      # This can be interpreted as a measure of the "pressure per individual" during the equilibrium phase.
      equilibrium_pressure_to_population_ratio =
        equilibrium_pressure_mean / equilibrium_population_mean,
      # coefficient of variation of the pressure-to-population ratio during the equilibrium phase.
      # This can be interpreted as a measure of the variability in "pressure per individual" during the equilibrium phase.
      equilibrium_pressure_to_population_ratio_cv =
        sqrt(equilibrium_pressure_variance / equilibrium_pressure_mean^2 + equilibrium_population_variance / equilibrium_population_mean^2),
      # standard error of the coefficient of variation of the pressure-to-population ratio during the equilibrium phase.
      equilibrium_pressure_to_population_ratio_se =
        equilibrium_pressure_to_population_ratio_cv / sqrt(sum(step >= max(step) * burn_in_fraction)),
      # time spent above carrying capacity during the equilibrium phase (as a fraction of total time steps in the equilibrium phase)
      time_above_capacity =
        sum(pressure[step >= max(step) * burn_in_fraction] > 1) / sum(step >= max(step) * burn_in_fraction)
    ) |>
    ungroup()
}
