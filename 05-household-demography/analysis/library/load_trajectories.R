load_trajectories <- function(
  model_version, 
  suffix = "",
  selected_variables = NULL,
  ignored_variables = NULL,
  step_start = NULL,
  step_end = NULL) {

  if (suffix != "") {
    suffix <- paste0("-", suffix)
  }

  version_paths <- list(
    "v1.1"   = paste0("simdata/v1.1/household-demography-main exp-trajectories", suffix, "-table.csv"),
    "v1.2"   = paste0("simdata/v1.2/household-demography-main_density exp-trajectories", suffix, "-table.csv"),
    "v1.2.1" = paste0("simdata/v1.2.1/household-demography-main_density-K-demand exp-trajectories", suffix, "-table.csv"),
    "v1.2.2" = paste0("simdata/v1.2.2/household-demography-main_density-K-demand-supply exp-trajectories", suffix, "-table.csv"),
    "v1.3"   = paste0("simdata/v1.3/household-demography-daily-main exp-trajectories", suffix, "-table.csv"),
    "v1.4"   = paste0("simdata/v1.4/household-demography-daily-main_density exp-trajectories", suffix, "-table.csv"),
    "v1.4.1" = paste0("simdata/v1.4.1/household-demography-daily-main_density-K-demand exp-trajectories", suffix, "-table.csv"),
    "v1.4.2" = paste0("simdata/v1.4.2/household-demography-daily-main_density-K-demand-supply exp-trajectories", suffix, "-table.csv")
  )

  paths <- version_paths[[model_version ]]
  if (is.null(paths)) {
    stop("Unknown version and/or suffix.")
  }

  # Ensure data.table is installed
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required for high-performance memory management.")
  }

  # NetLogo column headers often contain special characters like brackets e.g., "[step]"
  step_col_name <- "X.step."

  # Initialize fread arguments
  fread_select <- NULL
  fread_drop <- NULL

  # Scenario A: User provided a whitelist of variables to keep
  if (!is.null(selected_variables)) {
    fread_select <- selected_variables
    # Ensure [step] is included for row-filtering if needed
    if (!is.null(step_start) || !is.null(step_end)) {
      if (!step_col_name %in% fread_select) {
        fread_select <- c(step_col_name, fread_select)
      }
    }
  } 
  # Scenario B: User provided a blacklist of variables to ignore
  else if (!is.null(ignored_variables)) {
    fread_drop <- ignored_variables
    # Force [step] to NEVER be dropped during stream loading so we can filter rows
    if (step_col_name %in% fread_drop) {
      fread_drop <- setdiff(fread_drop, step_col_name)
    }
  }

  # Fast, memory-mapped file reading
  # check.names = FALSE preserves NetLogo's exact bracket syntax like "[step]"
  dt <- data.table::fread(
    file = paths,
    skip = 6,
    select = fread_select,
    drop = fread_drop,
    check.names = TRUE
  )

  # Row filtering based on simulation step span
  if (!is.null(step_start)) {
    dt <- dt[get(step_col_name) >= step_start]
  }
  if (!is.null(step_end)) {
    dt <- dt[get(step_col_name) <= step_end]
  }

  # Return as a standard data.frame (or keep as data.table for speed)
  return(as.data.frame(dt))
}