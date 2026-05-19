compute_partial_dependence <- function(
    rf_model,
    data,
    parameter,
    response_class = NULL
) {

  pd <- pdp::partial(
    object = rf_model,
    pred.var = parameter,
    train = data,
    which.class = response_class,
    prob = TRUE
  )

  pd$parameter <- parameter

  pd
}
