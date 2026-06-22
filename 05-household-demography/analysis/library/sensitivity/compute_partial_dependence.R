compute_partial_dependence <- function(
    rf_model,
    data,
    parameter,
    response_variable,
    response_class = NULL
) {

  response_type <- NULL
  if (response_variable != "survival") {
      response_type <- "regression"
      data <- data |>
          filter(
              survival != "Extinction" #!is.na(.data[[response_variable]])
          )
  } else {
      response_type <- "classification"
  }

  if (response_type == "classification" && is.null(response_class)) {
      stop("response_class must be provided for classification models.")
  }

  if (response_type == "regression" && !is.null(response_class)) {
      warning("response_class is ignored for regression models.")
  }

  if (response_type == "classification") {
      if (!response_class %in% rf_model$classes) {
          stop(
              paste(
                  "response_class",
                  response_class,
                  "is not a valid class in the random forest model."
              )
          )
      }
  }

  if (!parameter %in% colnames(data)) {
      stop(
          paste(
              "parameter",
              parameter,
              "is not a column in the data."
          )
      )
  }

  if (!parameter %in% names(rf_model$forest$xlevels)) {
      stop(
          paste(
              "parameter",
              parameter,
              "is not a predictor variable in the random forest model."
          )
      )
  }
  
  if (!all.equal(
    as.numeric(data[[response_variable]]), 
    as.numeric(unname(rf_model$y))
    )) {
      stop(
          paste(
              "response_variable",
              response_variable,
              "does not match the response variable in the random forest model."
          )
      )
  }

  if (response_type == "classification" && !is.factor(data[[response_variable]])) {
      stop(
          paste(
              "response_variable",
              response_variable,
              "must be a factor in the data for classification models."
          )
      )
  }

  if (response_type == "regression" && !is.numeric(data[[response_variable]])) {
      stop(
          paste(
              "response_variable",
              response_variable,
              "must be numeric in the data for regression models."
          )
      )
  }

  if (response_type == "classification" && !is.factor(rf_model$y)) {
      stop(
          paste(
              "The response variable in the random forest model must be a factor for classification models."
          )
      )
  }

  if (response_type == "regression" && !is.numeric(rf_model$y)) {
      stop(
          paste(
              "The response variable in the random forest model must be numeric for regression models."
          )
      )
  }

  if (response_type == "classification") {
      pd <- pdp::partial(
          object = rf_model,
          pred.var = parameter,
          train = data,
          which.class = response_class,
          prob = TRUE
      )
  } else {
      pd <- pdp::partial(
          object = rf_model,
          pred.var = parameter,
          train = data
      )
  }

  pd$parameter <- parameter
  pd$response_variable <- response_variable
  pd$response_type <- response_type

  pd
}
