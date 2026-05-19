# Preprocess data for analysis

# This script defines functions to preprocess the output data from the NetLogo simulations.

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
    "sigma2.men" = "sigma2_men"
  )
  df |>
    rename_with(~ lookup[.x], .cols = intersect(names(lookup), names(df)))
}

# Remove duplicate columns in the original dataset (NetLogo and/or implementation particularities)
remove_duplicate_columns <- function(df) {
  df |>
    # filter out duplicate parameter columns ending with ".1" (R's read.csv behavior when duplicate column names are present)
    select(-matches("\\.1$")) |>
    # filter out duplicate parameter columns starting with "par_"
    select(-matches("^par_"))
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

# Main preprocessing function

preprocess_simdata <- function(
  df,
  version,
  growth_threshold = 5000,
  full_trajectory = FALSE,
  flag_version = TRUE
) {

  # Preprocess the simulation data for analysis. This includes renaming columns, extracting parameter ranges, and classifying survival outcomes.
  # Args:
  #   df: The raw simulation data as a data frame.
  #   version: A string indicating the version of the simulation or preprocessing (used for tracking and analysis).
  #   growth_threshold: A numeric value used to classify "Growth" in the survival classification (default is 5000).
  #   full_trajectory: A boolean indicating whether to classify survival based on the full trajectory (TRUE) or just the final population size (FALSE, default).
  #   flag_version: A boolean indicating whether to add a version column to the output data frame (default is TRUE).
  # Returns:
  #   A preprocessed data frame ready for analysis.

  # Step 1: Rename columns for consistency and clarity
  
  df <- df |>
    rename_columns() |>
    remove_duplicate_columns()
  
  # Step 2: Extract parameter ranges from NetLogo's string representation of ranges (e.g., "[min max]") and add them as separate columns
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
  
  # Step 3: Create a log10 version of the total population size for better visualization and analysis. This transformation is common in demographic studies where population sizes can vary over several orders of magnitude, making it easier to visualize trends and patterns in the data.
  df <- df |>
    mutate(
      log_totalIndividuals = log10(totalIndividuals + 1)
    )

  # Step 4: Classify survival outcomes based on the total population size at the end of the simulation or throughout the trajectory, depending on the `full_trajectory` flag. This classification will help in understanding the conditions under which populations go extinct, persist, or grow, which is crucial for analyzing the effects of different parameters and rules in the simulations.

  if (!full_trajectory) {
    df <- df |>
      mutate(
        survival = classify_survival(
          totalIndividuals,
          growth_threshold
        )
      )
  } else {
    df <- df |>
      group_by(run_number) |>
      mutate(
        endstate_total_individuals = last(totalIndividuals),
        survival = classify_survival(
          endstate_total_individuals,
          growth_threshold
        )
      ) |>
      ungroup()
  }

  # Step 5: Convert categorical variables to factors with specified levels for consistent ordering in analysis and visualization. This is important for ensuring that when we analyze or visualize the data, the categories are ordered in a meaningful way (e.g., "Extinction" before "Persistence" before "Growth", or "matrilocal-matrilineal" before "patrilocal-patrilineal"). This step enhances the interpretability of the results and ensures that any plots or summaries reflect the intended order of categories.
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
      ),
      survival = factor(
        survival,
        levels = c(
          "Extinction",
          "Persistence",
          "Growth"
        )
      )
    )

  # Step 6: Optionally add a version column to the data frame for tracking and analysis purposes. This is useful for distinguishing between different versions of the simulation or preprocessing steps, especially when comparing results across multiple runs or iterations of the analysis. The `flag_version` parameter allows the user to control whether this column is added, providing flexibility in how the data is organized and analyzed.
  if (flag_version) {
      df <- df |>
      mutate(version = version)
  }
  
  # Return the preprocessed data frame ready for analysis
  df
}

