# Preprocess data for analysis

# This script defines functions to preprocess the output data from the NetLogo simulations.

#Define versions with density-dependent mortality for later use in determining the appropriate capacity variable and classification thresholds
  versions_with_density <- c(
    "v1.2",
    "v1.2.1",
    "v1.2.2",
    "v1.4",
    "v1.4.1",
    "v1.4.2"
  )
  versions_with_capacity_demand <- c(
    "v1.2.1",
    "v1.2.2",
    "v1.4.1",
    "v1.4.2"
  )
  versions_with_labour_capacity <- c(
  "v1.2.2",
  "v1.4.2"
  )

# Rename columns for consistency and clarity
rename_columns <- function(df) {
  lookup <- c(
    "X.run.number." = "run_number",
    "type.of.experiment" = "type_of_experiment",
    "exp.number" = "exp_number",
    "coale.demeny.region" = "coale_demeny_region",
    "residence.rule" = "residence_rule",
    "X.step." = "step",
    "max.iterations" = "max_iterations",
    "max.population" = "max_population",
    "maturity.age" = "maturity_age",
    "initial.num.households" = "initial_num_households",
    "household.initial.age.distribution" = "household_initial_age_distribution",
    "cdmlt.level" = "cdmlt_level",
    "c1.fert" = "c1_fert",
    "mu.fert" = "mu_fert",
    "sigma1.fert" = "sigma1_fert",
    "sigma2.fert" = "sigma2_fert",
    "max.couple.count.distribution" = "max_couple_count_distribution",
    "acceptable.kinship.degree.for.couples" = "acceptable_kinship_degree_for_couples",
    "c1.women" = "c1_women",
    "mu.women" = "mu_women",
    "sigma1.women" = "sigma1_women",
    "sigma2.women" = "sigma2_women",
    "c1.men" = "c1_men",
    "mu.men" = "mu_men",
    "sigma1.men" = "sigma1_men",
    "sigma2.men" = "sigma2_men",
    "carrying.capacity" = "carrying_capacity",
    "carrying.capacity.demand" = "carrying_capacity_demand",
    "density.effect.steepness" = "density_effect_steepness",
    "density.effect.scaling.factor" = "density_effect_scaling_factor",
    "labour.available" = "labour_available",
    "labour.required" = "labour_required",
    "laboured.share" = "laboured_share",
    "carrying.capacity.effective" = "carrying_capacity_effective",
    "labour.demand.per.capita" = "labour_demand_per_capita"
  )
  df |>
    rename_with(~ lookup[.x], .cols = intersect(names(lookup), names(df)))
}

# Safe renaming logic function
safe_rename <- function(.data, old, preferred, alternative) {
  .data %>% 
    rename_with(
      ~ if_else(preferred %in% colnames(.data), alternative, preferred),
      .cols = all_of(old)
    )
}

# Remove duplicate columns in the original dataset (NetLogo and/or implementation particularities)
remove_duplicate_columns <- function(df) {
  # handle special cases (due to mistake in setting Behaviour Space experiment)
  if ("par_carrying.capacity" %in% names(df)) {
    df <- df |>
      
      safe_rename(
        old  = "par_carrying.capacity",
        preferred = "carrying.capacity",
        alternative = "carrying.capacity.1"
      )
  }
  if ("par_density.effect.scaling.factor" %in% names(df)) {
    df <- df |>
      safe_rename(
        old  = "par_density.effect.scaling.factor",
        preferred = "density.effect.scaling.factor",
        alternative = "density.effect.scaling.factor.1"
      )
  }
  if ("par_labour.demand.per.capita" %in% names(df)) {
    df <- df |>
      safe_rename(
        old  = "par_labour.demand.per.capita",
        preferred = "labour.demand.per.capita",
        alternative = "labour.demand.per.capita.1"
      )
  }

  df <- df |>
    # filter out duplicate parameter columns ending with ".1" (R's read.csv behavior when duplicate column names are present)
    select(-matches("\\.1$")) |>
    # filter out duplicate parameter columns starting with "par_"
    select(-matches("^par_"))
  
  df
}

# NetLogo range extraction

extract_netlogo_range <- function(char_vector) {

  split_values <- gsub("\\[|\\]", "", char_vector) |>

    strsplit(" ")

  tibble(
    min = sapply(split_values, \(x) as.numeric(x[1])),
    max = sapply(split_values, \(x) as.numeric(x[2]))
  )
}

# Format categorical variables as factors with specified levels for consistent ordering in analysis and visualization
format_categorical_variables <- function(
  df,
  model_version
  ) {
  df <- df |>
    mutate(
      # Convert categorical variables to factors with specified levels for consistent ordering in analysis and visualization
      residence_rule = factor(
        residence_rule,
        levels = c(
          "matrilocal-matrilineal",
          "patrilocal-patrilineal"
        )
      ),
      coale_demeny_region = factor(
        coale_demeny_region,
        levels = c(
          "north",
          "south",
          "east",
          "west"
        )
      )
    )

  if (model_version %in% versions_with_density) {
    df <- df |>
      mutate(
        survival = factor(
          survival,
          levels = c(
            "Extinction",
            "Persistence\n(low pressure)",
            "Persistence\n(high pressure)",
            "Overshooting"
          )
        )
      )
  } else {
    df <- df |>
      mutate(
        survival = factor(
          survival,
          levels = c(
            "Extinction",
            "Persistence",
            "Growth"
          )
        )
      )
  }
  df
}

# Survival classification

classify_survival <- function(
  population,
  threshold
) {
  case_when(
    population <= 0 ~ "Extinction",
    population >= threshold ~ "Growth",
    TRUE ~ "Persistence"
  )
}

classify_survival_density <- function(
  population, 
  threshold
  ) {
  case_when(
    is.na(threshold) ~ "Extinction",
    threshold <= 0 & population <= 0 ~ "Extinction",
    threshold <= 0 & population > 0 ~ "Undefined",
    population <= 0 ~ "Extinction",
    population <= 0.5 * threshold ~ "Persistence\n(low pressure)",
    population < threshold ~ "Persistence\n(high pressure)",
    population >= threshold ~ "Overshooting"
  )
}

# Pressure calculation for versions with density-dependent mortality

compute_pressure <- function(
  population,
  capacity
) {
  population / capacity
}

# Main preprocessing function

preprocess_simdata <- function(
  df,
  model_version,
  growth_threshold = 5000,
  overshooting_threshold = 0,
  full_trajectory = FALSE,
  trajectory_id_vars = c(
    # unique combinations of these variables id single runs
    "run_number",
    #"SEED",
    "residence_rule"#,
    #"coale_demeny_region"
  ),
  flag_version = TRUE
) {

  # Preprocess the simulation data for analysis. This includes renaming columns, extracting parameter ranges, and classifying survival outcomes.
  # Args:
  #   df: The raw simulation data as a data frame.
  #   model_version: A string indicating the version of the simulation or preprocessing (used for tracking and analysis).
  #   growth_threshold: A numeric value used to distinguish "Persistence" from "Growth" in the survival classification (default is 5000). In versions with density-dependent mortality, this threshold will be set automatically to the carrying capacity to classify survival outcomes.
  #   full_trajectory: A boolean indicating whether to classify survival based on the full trajectory (TRUE) or just the final population size (FALSE, default).
  #   flag_version: A boolean indicating whether to add a version column to the output data frame (default is TRUE).
  # Returns:
  #   A preprocessed data frame ready for analysis.

  # Step 1: Rename columns for consistency and clarity
  df <- df |>
    remove_duplicate_columns() |>
    rename_columns()
  
  # Step 2: Add a run_id column for uniquely identifying each simulation run, which will be useful for grouping and analysis later on. 
  # This is especially important because run_number is not unique once multiple batches of simulations are combined.
  if (!full_trajectory) {
    # When dealing with end states, each row represents a unique run
    df <- df |> 
      mutate(run_unique_id = row_number())
  } else {
    # When dealing with full trajectories, rows with identical id variables share a run
    df <- df |> 
      group_by(across(all_of(trajectory_id_vars))) |> 
      mutate(run_unique_id = cur_group_id()) |> 
      ungroup()
  }

  # Step 3a. Determine the appropriate capacity variable for versions with density-dependent mortality. 
  # This is necessary because different versions of the simulation may use either "carrying_capacity_effective" or "carrying_capacity" 
  # to represent the carrying capacity, which is crucial for calculating pressure and classifying survival outcomes in those versions.
  capacity_var <- NULL
  if (model_version %in% versions_with_density[!versions_with_density %in% versions_with_labour_capacity]) {

      if (!"carrying_capacity" %in% colnames(df)) {
          stop(
              paste(
                "For model versions",
                paste0(versions_with_density[!versions_with_density %in% versions_with_labour_capacity], collapse = "|"),
                "the data frame must contain a column named 'carrying_capacity'."
              )
          )
      }
      capacity_var <- "carrying_capacity"

  } else if (model_version %in% versions_with_labour_capacity) {
      # Check deactivated because effective carrying capacity is being temporarily 
      # recalculated in R and not imported from NetLogo (see step 3b)
      # if (!"carrying_capacity_effective" %in% colnames(df)) {
      #     stop(
      #         paste(
      #           "For model versions",
      #            paste0(versions_with_labour_capacity, collapse = "|"),
      #           "the data frame must contain a column named 'carrying_capacity_effective'."
      #         )
      #     )
      # }
      capacity_var <- "carrying_capacity_effective"

  } else if (model_version %in% versions_with_density) {
      stop(
          "For model versions with density-dependent mortality, the data frame must contain either 'carrying_capacity' or 'carrying_capacity_effective' column."
      )
  } else {
      # For versions without density-dependent mortality, the growth_threshold will be used for survival classification
      capacity_var <- NULL
  }

  # Step 3b: Select totalIndividuals or calculate carrying capacity demand and effective carrying capacity when applicable
  demand_var <- NULL
  if (model_version %in% versions_with_capacity_demand) {
    if (!"carrying_capacity_demand" %in% names(df)) {

      if (full_trajectory) {
        endstates_tmp <- df |>
          group_by(across(all_of(trajectory_id_vars))) |>
          slice_tail(n = 1) |>
          ungroup() |>
          compute_carrying_capacity_demand()

        df <- df |>
          left_join(
            endstates_tmp |>
              select(
                all_of(trajectory_id_vars),
                carrying_capacity_demand
              ),
            by = trajectory_id_vars
          )

      } else {
        df <- df |>
          compute_carrying_capacity_demand()
      }

    }
    # if (full_trajectory) {

    #   endstates_tmp <- df |>
    #     group_by(across(all_of(trajectory_id_vars))) |>
    #     slice_tail(n = 1) |>
    #     ungroup() |>
    #     compute_carrying_capacity_demand()

    #   if (model_version %in% versions_with_labour_capacity) {
    #     endstates_tmp <- endstates_tmp |>
    #       compute_effective_carrying_capacity()
        
    #     df <- df |>
    #       left_join(
    #         endstates_tmp |>
    #           select(
    #             all_of(trajectory_id_vars),
    #             carrying_capacity_demand,
    #             carrying_capacity_effective,
    #             labour_available,
    #             labour_required,
    #             laboured_share
    #           ),
    #         by = trajectory_id_vars
    #       )
    #   } else {
    #     df <- df |>
    #     left_join(
    #       endstates_tmp |>
    #         select(
    #           all_of(trajectory_id_vars),
    #           carrying_capacity_demand
    #         ),
    #       by = trajectory_id_vars
    #     )
    #   }

    # } else {
    #   df <- df |>
    #     compute_carrying_capacity_demand()

    #   if (model_version %in% versions_with_labour_capacity) {
    #     df <- df |>
    #       compute_effective_carrying_capacity()
    #   }
    # }
    demand_var <- "carrying_capacity_demand"
  } else {
    demand_var <- "totalIndividuals"
  }

  # Step 3c: effective carrying capacity
  if (model_version %in% versions_with_labour_capacity) {

    required_vars <- c(
      "labour_available",
      "labour_required",
      "laboured_share",
      "carrying_capacity_effective"
    )

    if (!all(required_vars %in% names(df))) {

      if (full_trajectory) {
        endstates_tmp <- df |>
          group_by(across(all_of(trajectory_id_vars))) |>
          slice_tail(n = 1) |>
          ungroup() |>
          compute_effective_carrying_capacity()

        df <- df |>
          left_join(
            endstates_tmp |>
              select(
                all_of(trajectory_id_vars),
                all_of(required_vars)
              ),
            by = trajectory_id_vars
          )

      } else {
        df <- df |>
          compute_effective_carrying_capacity()
      }

    }
  }

  # Step 4: Extract parameter ranges from NetLogo's string representation of ranges (e.g., "[min max]") and add them as separate columns
  # This is necessary for parameters like "household_initial_age_distribution" and "max_couple_count_distribution" which are represented as ranges in the NetLogo model. The extracted min and max values will be used in the analysis to understand the parameter space explored in the simulations.
  # The function `extract_netlogo_range` takes a character vector of the form "[min max]", removes the brackets, splits the string by space, and converts the first and second elements to numeric values representing the minimum and maximum of the range, respectively. These values are then added as new columns to the data frame for easier analysis and interpretation.
  
  age_range <- extract_netlogo_range(
    df$household_initial_age_distribution
  )

  couple_range <- extract_netlogo_range(
    df$max_couple_count_distribution
  )

  df <- df |>
    mutate(
      household_initial_age_distribution_min = age_range$min,
      household_initial_age_distribution_max = age_range$max,
      max_couple_count_distribution_min = couple_range$min,
      max_couple_count_distribution_max = couple_range$max
    )
  
  # Step 5: Create a log10 version of the total population size and household count for better visualization and analysis. This transformation is common in demographic studies where population sizes can vary over several orders of magnitude, making it easier to visualize trends and patterns in the data.
  df <- df |>
    mutate(
      household_structure_ratio = totalIndividuals / totalHouseholds,
      log_totalIndividuals = log10(totalIndividuals + 1),
      log_totalHouseholds = log10(totalHouseholds + 1)
    )
  
  if (model_version %in% versions_with_labour_capacity) {
    df <- df |>
      mutate(
        log_carrying_capacity_effective = log10(carrying_capacity_effective + 1),
        labour_ratio = labour_available / labour_required
      )
  }

  # Step 6: Classify survival outcomes based on version and the total population size at the end of the simulation or throughout the trajectory, depending on the `full_trajectory` flag. 
  # This classification will help in understanding the conditions under which populations go extinct, persist, or grow, which is crucial for analyzing the effects of different parameters and rules in the simulations.

  if (!full_trajectory) {
    # Classify survival based on the final population size at the end of the simulation
    if (model_version %in% versions_with_density) {
      df <- df |>
        mutate(
          survival = classify_survival_density(
            !!!rlang::syms(demand_var),
            (1 + overshoot_magnitude_threshold) * (!!!rlang::syms(capacity_var))
          )
        )
    } else {
    df <- df |>
      mutate(
        survival = classify_survival(
          totalIndividuals,
          growth_threshold
        )
      )
    }
  } else {
    if (model_version %in% versions_with_density) {
      endstates <- df |>
        group_by(across(all_of(trajectory_id_vars))) |>
        summarise(
          endstate_total_individuals = dplyr::last(totalIndividuals),
          endstate_total_households = dplyr::last(totalHouseholds),
          endstate_carrying_capacity_demand = dplyr::last(.data[[demand_var]]),
          endstate_capacity = dplyr::last(.data[[capacity_var]]),
          .groups = "drop"
        ) |>
        mutate(
          survival = classify_survival_density(
            endstate_total_individuals,
            endstate_capacity
          )
        ) |>
        ungroup()

      df <- df |>
        left_join(endstates, by = trajectory_id_vars)
    } else{
      df <- df |>
      group_by(across(all_of(trajectory_id_vars))) |>
      arrange(step, .by_group = TRUE) |>
      mutate(
        endstate_total_individuals = last(totalIndividuals),
        endstate_total_households = last(totalHouseholds),
        survival = classify_survival(
          endstate_total_individuals,
          growth_threshold
        )
      ) |>
      ungroup()
    }
  }

  # Step 7: Convert categorical variables to factors with specified levels for consistent ordering in analysis and visualization. This is important for ensuring that when we analyze or visualize the data, the categories are ordered in a meaningful way (e.g., "Extinction" before "Persistence" before "Growth", or "matrilocal-matrilineal" before "patrilocal-patrilineal"). This step enhances the interpretability of the results and ensures that any plots or summaries reflect the intended order of categories.
  df <- df |>
    format_categorical_variables(
      model_version = model_version
      )

  # Step 8: Add a version column to the data frame for tracking and analysis purposes. This is useful for distinguishing between different versions of the simulation or preprocessing steps, especially when comparing results across multiple runs or iterations of the analysis. The `flag_version` parameter allows the user to control whether this column is added, providing flexibility in how the data is organized and analyzed.
  if (flag_version) {
      df <- df |>
      mutate(model_version = model_version)
  }

  # Step 9: Calculate pressure for versions with density-dependent mortality
  if (model_version %in% versions_with_density) {
    df <- df |>
      mutate(
        pressure = compute_pressure(
          .data[[demand_var]], 
          .data[[capacity_var]]
          )
      )
  }

  # Return the preprocessed data frame ready for analysis
  df
}

