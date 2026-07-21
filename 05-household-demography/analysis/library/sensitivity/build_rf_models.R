build_rf_models <- function(
    sensitivity_data,
    response_variables,
    model_version,
    SEED = 123,
    verbose = TRUE
) {
    source("library/sensitivity/fit_random_forest.R")

    # Check if residence_rule is present in the sensitivity data, which is a structural variable used to split the data for the random forest models
    if (!"residence_rule" %in% colnames(sensitivity_data$matri)) {
        stop("The 'residence_rule' variable is not present in the sensitivity data.")
    }
    # Check if all response variables are present in the sensitivity data
    if (!all(response_variables %in% colnames(sensitivity_data$matri))) {
        stop("Not all response variables are present in the sensitivity data.")
    }

    rf_models = list(
    
        metadata = list(
            model_version = model_version,
            SEED = SEED,
            date = Sys.time(),
            response_variables = response_variables,
            r_version = version$version.string,
            random_forest_package_version = as.character(packageVersion("randomForest"))
        )
    )

    for (response_variable in response_variables) {

        if (verbose) {
            message(
                paste(
                    "Fitting random forest models for response variable:",
                    response_variable
                )
            )
        }
        
        rf_models[[response_variable]] <- list()

        for (res_rule in c("matri", "patri")) {

            if (verbose) {
                message(
                    paste(
                        "  Residence rule:",
                        res_rule
                    )
                )
            }

            dt <- sensitivity_data[[res_rule]]

            if (response_variable != "survival") {
                dt <- sensitivity_data[[res_rule]] |>
                    filter(
                        survival != "Extinction"
                    )
            } # This step should prevent NA values in the response variable, as random forest models cannot handle them

            dt <- dt |>
                select(
                    # Exclude any response variables other than the current response variable
                    -response_variables[response_variables != response_variable],
                    -residence_rule,
                    -model_version
                )
                
            rf_models[[response_variable]][[res_rule]] <- fit_random_forest(
                df = dt,
                response_variable = response_variable,
                SEED = SEED
            )

            if (verbose) {
                message("    Done.")
            }
        }

        if (verbose) {
            message("Finished fitting random forest models for response variable:", response_variable)
        }
    }

    if (verbose) {
        message("Finished fitting random forest models for all response variables.")
    }

    rf_models
}

