fit_rf_model <- function(
    df,
    response_variable,
    n_trees = 500,
    SEED = 123
) {

  predictors <- setdiff(
    names(df),
    response_variable
  )

  formula <- as.formula(
    paste(
      response_variable,
      "~",
      paste(predictors, collapse = " + ")
    )
  )

  set.seed(SEED)

  randomForest::randomForest(
    formula,
    data = df,
    importance = TRUE,
    ntree = n_trees
  )
}
