extract_importance <- function(rf_model) {

  importance(rf_model) |>
    as.data.frame() |>
    tibble::rownames_to_column("parameter")
}
