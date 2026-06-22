# # Example of age structure from netlogo output:
# # "[30 31 21 18 35 16 30 ... 38 35 26 14 11 11 9 ]"

get_carrying_capacity_demand_vectorized <- function(age_string_column) {
    # Parse the entire column into a list of numeric vectors
    age_lists <- parse_age_list(age_string_column)
    
    # Map over each row's vector, apply the math, and sum them up
    purrr::map_dbl(age_lists, function(ages) {
        if (length(ages) == 0 || any(is.na(ages))) return(0) # Safe guard for empty strings
        
        # Calculate demand for all ages in this row instantly
        demand_vec <- case_when(
            ages < 15 ~ ((ages / 15) ^ 1.2),
            ages > 45 ~ ((45 / ages) ^ 2),
            TRUE ~ 1
        )
        sum(demand_vec)
    })
}

compute_carrying_capacity_demand <- function(df) {

    stopifnot(
        "totalIndividuals" %in% names(df),
        "womenAgeStructure" %in% names(df),
        "menAgeStructure" %in% names(df)
    )

    df |>
        mutate(
            carrying_capacity_demand = if_else(
                totalIndividuals > 0,
                get_carrying_capacity_demand_vectorized(womenAgeStructure) + 
                get_carrying_capacity_demand_vectorized(menAgeStructure),
                0
            )
        )
}
