######################################################################
# Export Coale-Demeny model life tables for the Household Demography
# NetLogo models
#
# Corrects two problems in the original export script:
#   1. Male files are generated from male, not female, life tables.
#   2. Abridged interval probabilities are converted to single-year
#      probabilities without an age-index shift.
#
# Annualisation method
# --------------------
# demogR supplies abridged life tables. Its `lx` values are survival
# probabilities to exact age x. For each mortality level, this script:
#
#   1. transforms lx to cumulative hazard H(x) = -log(lx);
#   2. linearly interpolates H between the exact ages supplied by demogR;
#   3. derives the probability of dying between exact ages x and x + 1:
#
#          q_x = 1 - exp(-(H(x + 1) - H(x))).
#
# Linear cumulative-hazard interpolation is equivalent to assuming a
# constant force of mortality within each original abridged age interval.
# It preserves the demogR survival values exactly at the supplied age
# boundaries.
#
# Beyond the final exact age tabulated by demogR, the final open-interval
# central death rate (`nmx`) is used as a constant hazard. The last model
# age is assigned q_x = 1 as a terminal guard, so no individual can age
# beyond the length of the NetLogo lookup table.
#
# Output format
# -------------
# Eight files are written:
#
#   cdmltwF.txt  cdmltwM.txt
#   cdmlteF.txt  cdmlteM.txt
#   cdmltsF.txt  cdmltsM.txt
#   cdmltnF.txt  cdmltnM.txt
#
# Each file contains:
#
#   age  1  2  ...  25
#   0    q  q  ...  q
#   ...
#   150  1  1  ...  1
#
# This is compatible with the existing NetLogo loader:
#   item 0  = age
#   item 1  = mortality level 1
#   ...
#   item 25 = mortality level 25
#
# Package used in the original project:
#   demogR 0.6.0
######################################################################

# --------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------

EXPECTED_DEMOGR_VERSION <- "0.6.0"

OUTPUT_DIR <- "../demoTables"
MODEL_AGES <- 0:150

OVERWRITE_EXISTING <- TRUE
BACKUP_EXISTING <- FALSE
GENERATE_DIAGNOSTICS <- TRUE

# The final lookup-table age receives certain annual mortality. This is
# a computational guard against indexing beyond MODEL_AGES in NetLogo.
TERMINAL_MORTALITY <- 1

# Numerical tolerance used in validation.
TOLERANCE <- 1e-12

# --------------------------------------------------------------------
# Dependencies and package-version check
# --------------------------------------------------------------------

if (!requireNamespace("demogR", quietly = TRUE)) {
  stop(
    "Package 'demogR' is required. Install it with:\n",
    "install.packages('demogR')"
  )
}

installed_demogr_version <- as.character(
  utils::packageVersion("demogR")
)

if (!identical(
  installed_demogr_version,
  EXPECTED_DEMOGR_VERSION
)) {
  warning(
    "This project originally used demogR ",
    EXPECTED_DEMOGR_VERSION,
    ", but the installed version is ",
    installed_demogr_version,
    ". Inspect the generated diagnostics before running simulations."
  )
}

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# --------------------------------------------------------------------
# Coale-Demeny regional functions and output codes
# --------------------------------------------------------------------

CD_REGIONS <- list(
  w = list(
    name = "west",
    generator = demogR::cdmltw
  ),
  e = list(
    name = "east",
    generator = demogR::cdmlte
  ),
  s = list(
    name = "south",
    generator = demogR::cdmlts
  ),
  n = list(
    name = "north",
    generator = demogR::cdmltn
  )
)

SEXES <- c("F", "M")
LEVELS <- 1:25

# --------------------------------------------------------------------
# Validation helpers
# --------------------------------------------------------------------

assert_numeric_vector <- function(
  x,
  expected_length,
  object_name
) {
  if (!is.numeric(x)) {
    stop(object_name, " must be numeric.")
  }

  if (length(x) != expected_length) {
    stop(
      object_name,
      " has length ",
      length(x),
      "; expected ",
      expected_length,
      "."
    )
  }

  invisible(TRUE)
}

assert_probability_vector <- function(
  x,
  object_name,
  tolerance = TOLERANCE
) {
  if (anyNA(x) || any(!is.finite(x))) {
    stop(object_name, " contains missing or non-finite values.")
  }

  if (
    any(x < -tolerance) ||
    any(x > 1 + tolerance)
  ) {
    stop(object_name, " contains values outside [0, 1].")
  }

  invisible(TRUE)
}

validate_cd_object <- function(
  cd_object,
  region,
  sex
) {
  required_elements <- c(
    "age",
    "width",
    "lx",
    "nqx",
    "nmx"
  )

  missing_elements <- setdiff(
    required_elements,
    names(cd_object)
  )

  if (length(missing_elements) > 0) {
    stop(
      "Coale-Demeny object for ",
      region,
      ", sex ",
      sex,
      " is missing: ",
      paste(missing_elements, collapse = ", "),
      "."
    )
  }

  age_nodes <- as.numeric(cd_object$age)
  interval_widths <- as.numeric(cd_object$width)

  if (
    length(age_nodes) < 2 ||
    anyNA(age_nodes) ||
    any(!is.finite(age_nodes)) ||
    is.unsorted(age_nodes, strictly = TRUE)
  ) {
    stop(
      "Invalid age vector for ",
      region,
      ", sex ",
      sex,
      "."
    )
  }

  assert_numeric_vector(
    interval_widths,
    length(age_nodes),
    paste0(region, " ", sex, " widths")
  )

  expected_dimensions <- c(
    length(LEVELS),
    length(age_nodes)
  )

  for (matrix_name in c("lx", "nqx", "nmx")) {
    matrix_value <- cd_object[[matrix_name]]

    if (!is.matrix(matrix_value)) {
      matrix_value <- as.matrix(matrix_value)
    }

    if (!identical(
      dim(matrix_value),
      expected_dimensions
    )) {
      stop(
        matrix_name,
        " for ",
        region,
        ", sex ",
        sex,
        " has dimensions ",
        paste(dim(matrix_value), collapse = " x "),
        "; expected ",
        paste(expected_dimensions, collapse = " x "),
        "."
      )
    }
  }

  invisible(TRUE)
}

# --------------------------------------------------------------------
# Convert one abridged mortality level to annual probabilities
# --------------------------------------------------------------------

annualise_cd_level <- function(
  cd_object,
  level,
  model_ages = MODEL_AGES,
  terminal_mortality = TERMINAL_MORTALITY,
  tolerance = TOLERANCE
) {
  if (
    length(level) != 1 ||
    !level %in% LEVELS
  ) {
    stop("level must be one integer from 1 to 25.")
  }

  if (
    length(model_ages) < 1 ||
    anyNA(model_ages) ||
    any(model_ages < 0) ||
    any(model_ages != floor(model_ages)) ||
    is.unsorted(model_ages, strictly = TRUE) ||
    !identical(
      model_ages,
      seq.int(min(model_ages), max(model_ages))
    )
  ) {
    stop(
      "model_ages must be a contiguous, strictly increasing vector ",
      "of non-negative integer ages."
    )
  }

  age_nodes <- as.numeric(cd_object$age)
  lx_nodes <- as.numeric(
    cd_object$lx[level, ]
  )
  nmx_nodes <- as.numeric(
    cd_object$nmx[level, ]
  )

  assert_numeric_vector(
    lx_nodes,
    length(age_nodes),
    "lx"
  )

  assert_numeric_vector(
    nmx_nodes,
    length(age_nodes),
    "nmx"
  )

  assert_probability_vector(
    lx_nodes,
    "lx",
    tolerance
  )

  if (any(lx_nodes <= 0)) {
    stop(
      "lx must remain positive at all exact ages. ",
      "Zero survival cannot be log-transformed."
    )
  }

  if (
    abs(lx_nodes[1] - 1) > tolerance
  ) {
    warning(
      "The first lx value is ",
      lx_nodes[1],
      " rather than 1. The schedule will be normalised ",
      "to survival at the first supplied age."
    )

    lx_nodes <- lx_nodes / lx_nodes[1]
  }

  # Numerical noise should not make survival increase with age.
  if (
    any(diff(lx_nodes) > tolerance)
  ) {
    stop(
      "lx increases with age for mortality level ",
      level,
      "."
    )
  }

  # Transform exact-age survival to cumulative hazard.
  cumulative_hazard_nodes <- -log(lx_nodes)

  # Guard against tiny floating-point reversals.
  cumulative_hazard_nodes <- cummax(
    cumulative_hazard_nodes
  )

  # q_x requires survival at x and x + 1.
  exact_model_ages <- seq.int(
    min(model_ages),
    max(model_ages) + 1L
  )

  last_tabulated_age <- max(age_nodes)

  within_tabulated_range <-
    exact_model_ages <= last_tabulated_age

  cumulative_hazard <- numeric(
    length(exact_model_ages)
  )

  # Linear interpolation of cumulative hazard preserves lx at every
  # tabulated exact age and implies a constant force of mortality within
  # each original abridged interval.
  cumulative_hazard[
    within_tabulated_range
  ] <- stats::approx(
    x = age_nodes,
    y = cumulative_hazard_nodes,
    xout = exact_model_ages[
      within_tabulated_range
    ],
    method = "linear",
    ties = "ordered",
    rule = 2
  )$y

  # Extend the final open age interval using its central death rate as a
  # constant hazard.
  open_interval_hazard <- tail(
    nmx_nodes[
      is.finite(nmx_nodes) &
        nmx_nodes >= 0
    ],
    1
  )

  if (length(open_interval_hazard) != 1) {
    stop(
      "Could not determine a finite non-negative hazard ",
      "for the final open age interval."
    )
  }

  beyond_tabulated_range <-
    !within_tabulated_range

  if (any(beyond_tabulated_range)) {
    cumulative_hazard[
      beyond_tabulated_range
    ] <-
      tail(cumulative_hazard_nodes, 1) +
      open_interval_hazard *
      (
        exact_model_ages[
          beyond_tabulated_range
        ] -
        last_tabulated_age
      )
  }

  annual_hazard_increment <- diff(
    cumulative_hazard
  )

  if (
    any(
      annual_hazard_increment <
        -tolerance
    )
  ) {
    stop(
      "Interpolated cumulative hazard decreases with age ",
      "for mortality level ",
      level,
      "."
    )
  }

  annual_hazard_increment <- pmax(
    annual_hazard_increment,
    0
  )

  # Stable computation of:
  # 1 - exp(-annual_hazard_increment)
  annual_qx <- -expm1(
    -annual_hazard_increment
  )

  annual_qx <- pmin(
    1,
    pmax(0, annual_qx)
  )

  # Ensure the final model age cannot survive into an age that is not
  # represented by the NetLogo lookup table.
  annual_qx[
    model_ages == max(model_ages)
  ] <- terminal_mortality

  names(annual_qx) <- model_ages

  assert_probability_vector(
    annual_qx,
    paste0(
      "annual qx, level ",
      level
    ),
    tolerance
  )

  annual_qx
}

# --------------------------------------------------------------------
# Build one NetLogo-compatible table
# --------------------------------------------------------------------

build_netlogo_table <- function(
  region_code,
  sex,
  model_ages = MODEL_AGES
) {
  if (!region_code %in% names(CD_REGIONS)) {
    stop(
      "Unknown region code: ",
      region_code,
      "."
    )
  }

  if (!sex %in% SEXES) {
    stop("sex must be 'F' or 'M'.")
  }

  region <- CD_REGIONS[[region_code]]

  cd_object <- region$generator(
    sex = sex
  )

  validate_cd_object(
    cd_object,
    region = region$name,
    sex = sex
  )

  annual_qx_matrix <- vapply(
    LEVELS,
    function(level) {
      annualise_cd_level(
        cd_object = cd_object,
        level = level,
        model_ages = model_ages
      )
    },
    numeric(length(model_ages))
  )

  output <- data.frame(
    age = model_ages,
    annual_qx_matrix,
    check.names = FALSE
  )

  names(output) <- c(
    "age",
    as.character(LEVELS)
  )

  if (!identical(
    output$age,
    model_ages
  )) {
    stop("Output ages are misaligned.")
  }

  probability_matrix <- as.matrix(
    output[, -1, drop = FALSE]
  )

  assert_probability_vector(
    as.numeric(probability_matrix),
    paste0(
      region$name,
      " ",
      sex,
      " output probabilities"
    )
  )

  if (
    !all(
      abs(
        probability_matrix[
          nrow(probability_matrix),
          ] -
          TERMINAL_MORTALITY
      ) <= TOLERANCE
    )
  ) {
    stop(
      "The terminal mortality guard was not applied ",
      "to every level."
    )
  }

  output
}

# --------------------------------------------------------------------
# File handling
# --------------------------------------------------------------------

output_filename <- function(
  region_code,
  sex
) {
  file.path(
    OUTPUT_DIR,
    paste0(
      "cdmlt",
      region_code,
      sex,
      ".txt"
    )
  )
}

existing_output_files <- unlist(
  lapply(
    names(CD_REGIONS),
    function(region_code) {
      vapply(
        SEXES,
        function(sex) {
          output_filename(
            region_code,
            sex
          )
        },
        character(1)
      )
    }
  ),
  use.names = FALSE
)

existing_output_files <- existing_output_files[
  file.exists(existing_output_files)
]

if (
  length(existing_output_files) > 0 &&
  !OVERWRITE_EXISTING
) {
  stop(
    "Output files already exist and OVERWRITE_EXISTING is FALSE:\n",
    paste(existing_output_files, collapse = "\n")
  )
}

backup_directory <- NA_character_

if (
  length(existing_output_files) > 0 &&
  BACKUP_EXISTING
) {
  backup_directory <- file.path(
    OUTPUT_DIR,
    paste0(
      "backup_cdmlt_",
      format(
        Sys.time(),
        "%Y%m%d_%H%M%S"
      )
    )
  )

  dir.create(
    backup_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  copied <- file.copy(
    from = existing_output_files,
    to = backup_directory,
    overwrite = FALSE
  )

  if (!all(copied)) {
    stop(
      "One or more existing table files could not be backed up."
    )
  }
}

write_netlogo_table <- function(
  table,
  filename
) {
  utils::write.table(
    table,
    file = filename,
    sep = " ",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    na = "NA",
    eol = "\n"
  )

  # Read the file back using R's parser and verify its structure.
  reloaded <- utils::read.table(
    filename,
    header = TRUE,
    sep = "",
    check.names = FALSE
  )

  if (!identical(
    dim(reloaded),
    dim(table)
  )) {
    stop(
      "Reloaded file has dimensions ",
      paste(dim(reloaded), collapse = " x "),
      "; expected ",
      paste(dim(table), collapse = " x "),
      ": ",
      filename
    )
  }

  if (!identical(
    names(reloaded),
    names(table)
  )) {
    stop(
      "Reloaded file has unexpected column names: ",
      filename
    )
  }

  if (
    max(
      abs(
        as.matrix(reloaded) -
        as.matrix(table)
      )
    ) > 1e-10
  ) {
    stop(
      "Values changed during write/read validation: ",
      filename
    )
  }

  invisible(filename)
}

# --------------------------------------------------------------------
# Generate all eight tables
# --------------------------------------------------------------------

generated_tables <- list()
manifest_rows <- list()

for (region_code in names(CD_REGIONS)) {
  region_name <- CD_REGIONS[[region_code]]$name

  for (sex in SEXES) {
    message(
      "Generating ",
      region_name,
      ", sex ",
      sex,
      "..."
    )

    table <- build_netlogo_table(
      region_code = region_code,
      sex = sex
    )

    filename <- output_filename(
      region_code,
      sex
    )

    write_netlogo_table(
      table,
      filename
    )

    table_key <- paste0(
      region_code,
      sex
    )

    generated_tables[[table_key]] <- table

    manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
      region_code = region_code,
      region = region_name,
      sex = sex,
      filename = basename(filename),
      n_ages = nrow(table),
      n_levels = ncol(table) - 1L,
      minimum_qx = min(
        as.matrix(
          table[, -1, drop = FALSE]
        )
      ),
      maximum_qx = max(
        as.matrix(
          table[, -1, drop = FALSE]
        )
      ),
      stringsAsFactors = FALSE
    )
  }
}

# --------------------------------------------------------------------
# Cross-table validation
# --------------------------------------------------------------------

for (region_code in names(CD_REGIONS)) {
  female_table <- generated_tables[[paste0(region_code, "F")]]

  male_table <- generated_tables[[paste0(region_code, "M")]]

  female_probabilities <- as.matrix(
    female_table[, -1, drop = FALSE]
  )

  male_probabilities <- as.matrix(
    male_table[, -1, drop = FALSE]
  )

  maximum_sex_difference <- max(
    abs(
      female_probabilities -
      male_probabilities
    )
  )

  if (
    maximum_sex_difference <= TOLERANCE
  ) {
    stop(
      "Female and male mortality tables are identical for region ",
      CD_REGIONS[[region_code]]$name,
      ". This indicates a generation error."
    )
  }

  message(
    "Validated sex difference for ",
    CD_REGIONS[[region_code]]$name,
    ": maximum absolute difference = ",
    signif(maximum_sex_difference, 6)
  )
}

manifest <- do.call(
  rbind,
  manifest_rows
)

manifest$demogR_version <-
  installed_demogr_version

manifest$annualisation_method <-
  "linear interpolation of cumulative hazard; open interval extended with final nmx"

manifest$terminal_age <-
  max(MODEL_AGES)

manifest$terminal_mortality <-
  TERMINAL_MORTALITY

manifest$md5 <- unname(
  tools::md5sum(
    file.path(
      OUTPUT_DIR,
      manifest$filename
    )
  )
)

manifest_file <- file.path(
  OUTPUT_DIR,
  "cdmlt_export_manifest.csv"
)

utils::write.csv(
  manifest,
  file = manifest_file,
  row.names = FALSE
)

# --------------------------------------------------------------------
# Optional diagnostic plots
# --------------------------------------------------------------------

generate_diagnostic_plots <- function(
  generated_tables,
  regions,
  filename
) {
  grDevices::pdf(
    filename,
    width = 8,
    height = 6,
    onefile = TRUE
  )

  old_par <- graphics::par(
    no.readonly = TRUE
  )

  on.exit(
    {
      graphics::par(old_par)
      grDevices::dev.off()
    },
    add = TRUE
  )

  graphics::par(
    mar = c(4.2, 4.5, 2.0, 1.0),
    las = 1
  )

  # West level 8: female versus male.
  west_f <- generated_tables$wF
  west_m <- generated_tables$wM

  graphics::plot(
    west_f$age,
    west_f[["8"]],
    type = "l",
    lwd = 2,
    xlim = c(0, 100),
    ylim = c(
      0,
      max(
        west_f[["8"]],
        west_m[["8"]]
      )
    ),
    xlab = "Age (years)",
    ylab = "Annual probability of dying",
    main = "Coale-Demeny West, level 8"
  )

  graphics::lines(
    west_m$age,
    west_m[["8"]],
    lwd = 2,
    lty = 2
  )

  graphics::legend(
    "topleft",
    legend = c("Female", "Male"),
    lwd = 2,
    lty = c(1, 2),
    bty = "n"
  )

  # Female regional comparison, level 8.
  region_codes <- names(regions)
  line_types <- seq_along(region_codes)

  first_region <- region_codes[1]
  first_table <- generated_tables[[paste0(first_region, "F")]]

  all_region_values <- unlist(
    lapply(
      region_codes,
      function(region_code) {
        generated_tables[[paste0(region_code, "F")]][["8"]]
      }
    )
  )

  graphics::plot(
    first_table$age,
    first_table[["8"]],
    type = "l",
    lwd = 2,
    lty = line_types[1],
    xlim = c(0, 100),
    ylim = c(0, max(all_region_values)),
    xlab = "Age (years)",
    ylab = "Annual probability of dying",
    main = "Female mortality by region, level 8"
  )

  if (length(region_codes) > 1) {
    for (i in 2:length(region_codes)) {
      region_code <- region_codes[i]
      table <- generated_tables[[paste0(region_code, "F")]]

      graphics::lines(
        table$age,
        table[["8"]],
        lwd = 2,
        lty = line_types[i]
      )
    }
  }

  graphics::legend(
    "topleft",
    legend = vapply(
      region_codes,
      function(region_code) {
        regions[[region_code]]$name
      },
      character(1)
    ),
    lwd = 2,
    lty = line_types,
    bty = "n"
  )

  # West female levels.
  selected_levels <- c(1, 3, 8, 25)
  selected_line_types <- seq_along(
    selected_levels
  )

  graphics::plot(
    west_f$age,
    west_f[[as.character(
      selected_levels[1]
    )]],
    type = "l",
    lwd = 2,
    lty = selected_line_types[1],
    xlim = c(0, 100),
    ylim = c(0, 1),
    xlab = "Age (years)",
    ylab = "Annual probability of dying",
    main = "Female Coale-Demeny West levels"
  )

  if (length(selected_levels) > 1) {
    for (i in 2:length(selected_levels)) {
      level <- selected_levels[i]

      graphics::lines(
        west_f$age,
        west_f[[as.character(level)]],
        lwd = 2,
        lty = selected_line_types[i]
      )
    }
  }

  graphics::legend(
    "topleft",
    legend = paste(
      "Level",
      selected_levels
    ),
    lwd = 2,
    lty = selected_line_types,
    bty = "n"
  )

  invisible(filename)
}

if (GENERATE_DIAGNOSTICS) {
  diagnostic_file <- file.path(
    OUTPUT_DIR,
    "cdmlt_export_diagnostics.pdf"
  )

  generate_diagnostic_plots(
    generated_tables = generated_tables,
    regions = CD_REGIONS,
    filename = diagnostic_file
  )
}

# --------------------------------------------------------------------
# Final report
# --------------------------------------------------------------------

message("")
message("Coale-Demeny export completed.")
message(
  "Output directory: ",
  normalizePath(
    OUTPUT_DIR,
    winslash = "/",
    mustWork = TRUE
  )
)
message(
  "Manifest: ",
  normalizePath(
    manifest_file,
    winslash = "/",
    mustWork = TRUE
  )
)

if (
  !is.na(backup_directory)
) {
  message(
    "Previous files backed up to: ",
    normalizePath(
      backup_directory,
      winslash = "/",
      mustWork = TRUE
    )
  )
}

if (GENERATE_DIAGNOSTICS) {
  message(
    "Diagnostics: ",
    normalizePath(
      diagnostic_file,
      winslash = "/",
      mustWork = TRUE
    )
  )
}

message("")
message(
  "IMPORTANT: these tables differ structurally from the legacy export."
)
message(
  "All simulation results using the previous tables must be treated ",
  "as generated by a different mortality implementation."
)
