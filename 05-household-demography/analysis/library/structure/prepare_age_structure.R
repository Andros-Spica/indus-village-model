prepare_age_structure <- function(
    df,
    age_breaks = seq(0, 100, 5),
    normalize = FALSE,
    group_vars = NULL
) {

  if (is.null(group_vars)) {
    group_vars <- character(0)
  }

  all_bins <- cut(
    age_breaks[-length(age_breaks)],
    breaks = age_breaks,
    right = FALSE,
    include.lowest = TRUE
  )

  tidy <- df |>

    select(all_of(group_vars), "run_unique_id", menAgeStructure, womenAgeStructure) |>

    pivot_longer(
      cols = c(menAgeStructure, womenAgeStructure),
      names_to = "sex",
      values_to = "age_list"
    ) |>

    mutate(
      sex = ifelse(sex == "menAgeStructure", "Male", "Female"),
      ages = parse_age_list(age_list)
    ) |>

    unnest(ages) |>

    mutate(
      age_bin = cut(
        ages,
        breaks = c(
          age_breaks,
          Inf
        ),
        right = FALSE,
        include.lowest = TRUE
      )
    ) |>

    count(
      !!!rlang::syms(group_vars),
      run_unique_id,
      sex,
      age_bin,
      name = "count"
    ) |>

    complete(
      nesting(
        !!!rlang::syms(group_vars),
        run_unique_id,
        sex
      ),
      age_bin = all_bins,
      fill = list(count = 0)
    )

  if (normalize) {
    tidy <- tidy |>
      group_by(run_unique_id) |>
      mutate(count = count / sum(count)) |>
      ungroup()
  }

  return(tidy)
}

parse_age_list <- function(age_string_column) {
    # 1. Strip brackets globally from the whole column
    cleaned <- stringr::str_remove_all(age_string_column, "[|]")
    
    # 2. Extract all numbers as a list of character vectors per row
    #    Using [0-9.]+ handles decimals safely if they exist
    list_of_ages <- stringr::str_extract_all(cleaned, "[0-9.]+")
    
    # 3. Convert all character strings inside the nested list to numeric
    #    Using map preserves the 1-row-to-1-list structure safely
    purrr::map(list_of_ages, as.numeric)
}
# parse_age_list <- function(age_string) {
#   age_string %>%
#     gsub("\\[|\\]", "", .) %>%
#     trimws() %>%
#     strsplit("\\s+") %>%
#     unlist() %>%
#     as.numeric()
# }
