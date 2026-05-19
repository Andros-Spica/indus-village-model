library(randtoolbox)
library(dplyr)
library(readr)
source("library/parameter_ranges.R")

# Global settings

VERSION <- "v1.1"
N_SAMPLES <- 2000
SEED <- 123
GENERATE_DIAGNOSTICS <- TRUE

# Load version-specific ranges

if (is.null(sobol_parameter_ranges)) {
  stop("Unknown model version.")
}
sobol_parameter_ranges <- parameter_ranges[[VERSION]]

INTEGER_PARAMETERS <- c(
  "initial-num-households",
  "household_initial_age_distribution_min",
  "household_initial_age_distribution_max",
  "max_couple_count_distribution_min",
  "max_couple_count_distribution_max",
  "acceptable-kinship-degree-for-couples",
  "cdmlt-level",
  "amenorrhea-period-in-days"
)

# Helper function

get_values_from_sobol <- function(
  sobol_matrix,
  param_ranges,
  param_index
) {
  qunif(
    sobol_matrix[, param_index],
    min = param_ranges[[param_index]][1],
    max = param_ranges[[param_index]][2]
  )
}

# Generate Sobol matrix

set.seed(SEED)
k <- length(sobol_parameter_ranges)

sobol_matrix <- sobol(
  n = N_SAMPLES,
  dim = k,
  scrambling = 3
)

# Transform distributions, format data frame

params_sobol <- data.frame()

for (i in seq_along(sobol_parameter_ranges)) {
  param_name <- names(sobol_parameter_ranges)[i]
  values <- get_values_from_sobol(
    sobol_matrix,
    sobol_parameter_ranges,
    i
  )

  if (param_name %in% INTEGER_PARAMETERS) {
    values <- round(values)
  }

  params_sobol[[param_name]] <- values
}

# Save sample

output_file <- paste0(
  "data/",
  gsub("\\.", "", VERSION),
  "_sobol_parameter_values.RData"
)

save(params_sobol, file = output_file)

# Export NetLogo experiments

save_experiment <- function(
  experiment_df,
  experiment_row,
  version_name
) {

  dir.create(
    paste0("experiments/", version_name),
    recursive = TRUE,
    showWarnings = FALSE
  )

  write(
    paste(
      unlist(
        experiment_df[experiment_row, ],
        use.names = FALSE
      )
    ),
    file = paste0(
      "experiments/",
      version_name,
      "/",
      experiment_row,
      ".txt"
    )
  )
}

for (i in 1:nrow(params_sobol)) {
  save_experiment(
    params_sobol,
    i,
    VERSION
  )
}

# Diagnostic plots

if (GENERATE_DIAGNOSTICS) {
  dir.create(
    "diagnostics",
    showWarnings = FALSE
  )

  png(
    paste0(
      "diagnostics/",
      VERSION,
      "_sobol_param_pairsPlot.png"
    ),
    width = 3000,
    height = 3000
  )

  pairs(params_sobol, pch = "·")

  dev.off()
}
