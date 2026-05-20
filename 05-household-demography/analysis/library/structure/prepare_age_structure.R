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

    mutate(run_id = row_number()) |>

    select(all_of(group_vars), "run_id", menAgeStructure, womenAgeStructure) |>

    pivot_longer(
      cols = c(menAgeStructure, womenAgeStructure),
      names_to = "sex",
      values_to = "age_list"
    ) |>

    mutate(
      sex = ifelse(sex == "menAgeStructure", "Male", "Female"),
      ages = map(age_list, parse_age_list)
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
      run_id,
      sex,
      age_bin,
      name = "count"
    ) |>

    complete(
      nesting(
        !!!rlang::syms(group_vars),
        run_id,
        sex
      ),
      age_bin = all_bins,
      fill = list(count = 0)
    )

  if (normalize) {
    tidy <- tidy |>
      group_by(run_id) |>
      mutate(count = count / sum(count)) |>
      ungroup()
  }

  return(tidy)
}

parse_age_list <- function(age_string) {
  age_string %>%
    gsub("\\[|\\]", "", .) %>%
    trimws() %>%
    strsplit("\\s+") %>%
    unlist() %>%
    as.numeric()
}
