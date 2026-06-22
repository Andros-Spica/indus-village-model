# Labour contribution of all individuals in a column of age structures
# # Example of age structure from netlogo output:
# # "[30 31 21 18 35 16 30 ... 38 35 26 14 11 11 9 ]"

get_available_labour_vectorized <- function(age_string_column) {

  age_lists <- parse_age_list(age_string_column)

  purrr::map_dbl(age_lists, function(ages) {

    if (length(ages) == 0 || any(is.na(ages))) {
      return(0)
    }

    labour_vec <- dplyr::case_when(
      ages < 15 ~ ((ages / 15)^10),
      ages > 45 ~ ((45 / ages)^10),
      TRUE      ~ 1
    )

    sum(labour_vec)
  })
}


compute_effective_carrying_capacity <- function(df) {

    stopifnot(
        "totalIndividuals" %in% names(df),
        "womenAgeStructure" %in% names(df),
        "menAgeStructure" %in% names(df),
        "carrying_capacity" %in% names(df),
        "labour_demand_per_capita" %in% names(df)
    )

    df |>
        dplyr::mutate(
            labour_available = if_else(
                totalIndividuals > 0,
                get_available_labour_vectorized(womenAgeStructure) +
                get_available_labour_vectorized(menAgeStructure),
                0
            ),
            labour_required = carrying_capacity * labour_demand_per_capita,
            labour_share = if_else(
                labour_required > 0,
                pmin(1, labour_available / labour_required),
                1
            ),
            carrying_capacity_effective =
                carrying_capacity * labour_share
        )
}