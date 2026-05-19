load_or_build_rf_models <- function(
    model_version,
    sensitivity_data = NULL,
    overwrite = FALSE
) {
    source("library/sensitivity/build_rf_models.R")

  model_file <- paste0(
    "data/rf_models/",
    gsub("\\.", "", model_version),
    "_rf_models.RData"
  )

  if (
    file.exists(model_file) &&
    !overwrite
  ) {

    load(model_file)

  } else {

    message(
        paste(
            "No existing random forest models found for model version:",
            model_version,
            "or overwrite is set to TRUE. Building new models."
        )
    )
    
    if (is.null(sensitivity_data)) {
        stop("sensitivity_data must be provided to build random forest models.")
    }

    rf_models <- build_rf_models(
      sensitivity_data = sensitivity_data,
      model_version = model_version
    )

    save(
      rf_models,
      file = model_file
    )
  }

  rf_models
}
