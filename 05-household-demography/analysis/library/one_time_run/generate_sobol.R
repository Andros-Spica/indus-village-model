library(randtoolbox)
library(dplyr)
library(readr)
source("library/parameter_ranges.R")

# Global settings

MODEL_VERSION <- "v1.1"
N_SAMPLES <- 2000
SUFFIX <- "" # suffix used for distinguishing reduced or alternative batches
SEED <- 123
GENERATE_DIAGNOSTICS <- TRUE

# Load version-specific ranges

sobol_parameter_ranges <- parameter_ranges[[MODEL_VERSION]]

if (is.null(sobol_parameter_ranges)) {
  stop("Unknown model version: ", MODEL_VERSION)
}

normalise_parameter_name <- function(x) {
  gsub(
    "[-.]",
    "_",
    x
  )
}

integer_parameter_keys <- c(
  "initial_num_households",
  "household_initial_age_distribution_min",
  "household_initial_age_distribution_max",
  "max_couple_count_distribution_min",
  "max_couple_count_distribution_max",
  "acceptable_kinship_degree_for_couples",
  "cdmlt_level",
  "amenorrhea_period_in_days"
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
  dim = k
)

# Transform distributions, format data frame

# 1. Initialize as an empty list instead of a data frame
params_list <- list()

for (i in seq_along(sobol_parameter_ranges)) {
  param_name <- names(sobol_parameter_ranges)[i]
  values <- get_values_from_sobol(
    sobol_matrix,
    sobol_parameter_ranges,
    i
  )
  
  if (
    normalise_parameter_name(param_name) %in%
      integer_parameter_keys
  ) {
    values <- round(values)
  }
  
  # 2. Assign to the list
  params_list[[param_name]] <- values
}

# 3. Convert the complete list to a data frame at the end
params_sobol <- as.data.frame(params_list)

# Save sample

output_file <- paste0(
  "data/",
  gsub("\\.", "", MODEL_VERSION),
  SUFFIX,
  "_sobol_parameter_values.RData"
)

save(params_sobol, file = output_file)

# Export NetLogo experiments

save_experiment <- function(
  experiment_df,
  experiment_row,
  version_name,
  suffix
) {

  experiment_directory <- file.path(
    "experiments",
    paste0(version_name, suffix)
  )

  dir.create(
    experiment_directory,
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
      suffix,
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
    MODEL_VERSION,
    SUFFIX
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
      MODEL_VERSION,
      SUFFIX,
      "_sobol_param_pairsPlot.png"
    ),
    width = 3000,
    height = 3000
  )

  pairs(params_sobol, pch = "·")

  dev.off()
}
