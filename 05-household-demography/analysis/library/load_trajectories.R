load_trajectories <- function(version) {

  version_paths <- list(

    "v1.1" = "simdata/v1.1/household-demography-main exp-trajectories-table.csv",

    "v1.2" = "simdata/v1.2/household-demography-main exp-trajectories-table.csv",

    "v1.2.1" = "simdata/v1.2.1/household-demography-main exp-trajectories-table.csv",

    "v1.2.2" = "simdata/v1.2.2/household-demography-main exp-trajectories-table.csv",

    "v1.3" = "simdata/v1.3/household-demography-main exp-trajectories-table.csv",

    "v1.4" = "simdata/v1.4/household-demography-main exp-trajectories-table.csv",

    "v1.4.1" = "simdata/v1.4.1/household-demography-main exp-trajectories-table.csv",

    "v1.4.2" = "simdata/v1.4.2/household-demography-main exp-trajectories-table.csv"

  )

  paths <- version_paths[[version]]

  if (is.null(paths)) {

    stop("Unknown version.")

  }

  read.csv(paths, skip = 6)

}

