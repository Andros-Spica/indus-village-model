load_trajectories <- function(version, suffix = "") {

  if (suffix != "") {

    suffix <- paste0("-", suffix)

  }

  version_paths <- list(

    "v1.1" = paste0("simdata/v1.1/household-demography-main exp-trajectories", suffix, "-table.csv"),

    "v1.2" = paste0("simdata/v1.2/household-demography-main exp-trajectories", suffix, "-table.csv"),

    "v1.2.1" = paste0("simdata/v1.2.1/household-demography-main exp-trajectories", suffix, "-table.csv"),

    "v1.2.2" = paste0("simdata/v1.2.2/household-demography-main exp-trajectories", suffix, "-table.csv"),

    "v1.3" = paste0("simdata/v1.3/household-demography-main exp-trajectories", suffix, "-table.csv"),

    "v1.4" = paste0("simdata/v1.4/household-demography-main exp-trajectories", suffix, "-table.csv"),

    "v1.4.1" = paste0("simdata/v1.4.1/household-demography-main exp-trajectories", suffix, "-table.csv"),

    "v1.4.2" = paste0("simdata/v1.4.2/household-demography-main exp-trajectories", suffix, "-table.csv")

  )

  paths <- version_paths[[version]]

  if (is.null(paths)) {

    stop("Unknown version and/or suffix.")

  }

  read.csv(paths, skip = 6)

}

