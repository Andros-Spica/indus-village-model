load_endstates <- function(version) {

  version_paths <- list(

    "v1.1" = list(
      matri = "simdata/v1.1/household-demography-main exp-endstates-matri-table.csv",
      patri = "simdata/v1.1/household-demography-main exp-endstates-patri-table.csv"
    ),

    "v1.2" = list(
      matri = "simdata/v1.2/household-demography-main_density exp-endstates-matri-table.csv",
      patri = "simdata/v1.2/household-demography-main_density exp-endstates-patri-table.csv"
    ),

    "v1.2.1" = list(
      matri = "simdata/v1.2.1/household-demography-main_density-K-demand exp-endstates-matri-table.csv",
      patri = "simdata/v1.2.1/household-demography-main_density-K-demand exp-endstates-patri-table.csv"
    ),

    "v1.2.2" = list(
      matri = "simdata/v1.2.2/household-demography-main_density-K-demand-supply exp-endstates-matri-table.csv",
      patri = "simdata/v1.2.2/household-demography-main_density-K-demand-supply exp-endstates-patri-table.csv"
    ),

    "v1.3" = list(
      matri = "simdata/v1.3/household-demography-daily-main exp-endstates-matri-table.csv",
      patri = "simdata/v1.3/household-demography-daily-main exp-endstates-patri-table.csv"
    ),

    "v1.4" = list(
      matri = "simdata/v1.4/household-demography-daily-main_density exp-endstates-matri-table.csv",
      patri = "simdata/v1.4/household-demography-daily-main_density exp-endstates-patri-table.csv"
    ),

    "v1.4.1" = list(
      matri = "simdata/v1.4.1/household-demography-daily-main_density-K-demand exp-endstates-matri-table.csv",
      patri = "simdata/v1.4.1/household-demography-daily-main_density-K-demand exp-endstates-patri-table.csv"
    ),

    "v1.4.2" = list(
      matri = "simdata/v1.4/household-demography-daily-main_density-K-demand-supply exp-endstates-matri-table.csv",
      patri = "simdata/v1.4/household-demography-daily-main_density-K-demand-supply exp-endstates-patri-table.csv"
    )
  )

  paths <- version_paths[[version]]

  if (is.null(paths)) {
    stop("Unknown version.")
  }

  matri <- read.csv(paths$matri, skip = 6)
  patri <- read.csv(paths$patri, skip = 6)

  bind_rows(matri, patri)
}
