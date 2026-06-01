build_rf_models <- function(
    sensitivity_data,
    model_version
) {
    source("library/sensitivity/fit_random_forest.R")

    if (model_version == "v1.1") {

        rf_models <- build_rf_models_v11(sensitivity_data)

    } else if (model_version == "v1.2") {

        rf_models <- build_rf_models_v12(sensitivity_data)
        
    } else if (model_version == "v1.2.1") {

        rf_models <- build_rf_models_v121(sensitivity_data)

    } else if (model_version == "v1.2.2") {

        rf_models <- build_rf_models_v122(sensitivity_data)

    } else if (model_version == "v1.3") {

        rf_models <- build_rf_models_v13(sensitivity_data)

    } else if (model_version == "v1.4") {

        rf_models <- build_rf_models_v14(sensitivity_data)

    } else if (model_version == "v1.4.1") {

        rf_models <- build_rf_models_v141(sensitivity_data)

    } else if (model_version == "v1.4.2") {

        rf_models <- build_rf_models_v142(sensitivity_data)

    } else {

        stop("Invalid model_version specified.")
    }

    rf_models
}

build_rf_models_v11 <- function(sensitivity_data) {

    response_variables = c(
            "log_totalIndividuals",
            "survival"
            )

    rf_models_v11 = list(
    
        metadata = list(
            model_version = "v1.1",
            date = Sys.time(),
            response_variables = response_variables,
            r_version = version$version.string,
            random_forest_package_version = as.character(packageVersion("randomForest"))
        ),

        regr_log_totalIndividuals = list(
            matri = fit_rf_model(
                sensitivity_data$matri |>
                select(
                    # Exclude any response variables other than log_totalIndividuals
                    -response_variables[response_variables != "log_totalIndividuals"],
                    -residence_rule
                ),
                "log_totalIndividuals"
            ),
            patri = fit_rf_model(
                sensitivity_data$patri |>
                select(
                    # Exclude any response variables other than log_totalIndividuals
                    -response_variables[response_variables != "log_totalIndividuals"],
                    -residence_rule
                ),
                "log_totalIndividuals"
            )
        ),
    
        class_survival = list(
            matri = fit_rf_model(
                sensitivity_data$matri |>
                select(
                    # Exclude any response variables other than survival
                    -response_variables[response_variables != "survival"],
                    -residence_rule
                ),
                "survival"
            ),
            patri = fit_rf_model(
                sensitivity_data$patri |>
                select(
                    # Exclude any response variables other than survival
                    -response_variables[response_variables != "survival"],
                    -residence_rule
                ),
                "survival"
            )
        )
    )
  
    rf_models_v11
}

build_rf_models_v12 <- function(sensitivity_data) {

    response_variables = c(
            "log_totalIndividuals",
            "pressure",
            "survival"
            )

    rf_models_v12 = list(
    
        metadata = list(
            model_version = "v1.2",
            date = Sys.time(),
            response_variables = response_variables,
            r_version = version$version.string,
            random_forest_package_version = as.character(packageVersion("randomForest"))
        ),

        regr_log_totalIndividuals = list(
            matri = fit_rf_model(
                sensitivity_data$matri |>
                select(
                    # Exclude any response variables other than log_totalIndividuals
                    -response_variables[response_variables != "log_totalIndividuals"],
                    -residence_rule
                ),
                "log_totalIndividuals"
            ),
            patri = fit_rf_model(
                sensitivity_data$patri |>
                select(
                    # Exclude any response variables other than log_totalIndividuals
                    -response_variables[response_variables != "log_totalIndividuals"],
                    -residence_rule
                ),
                "log_totalIndividuals"
            )
        ),

        regr_pressure = list(
            matri = fit_rf_model(
                sensitivity_data$matri |>
                select(
                    # Exclude any response variables other than pressure
                    -response_variables[response_variables != "pressure"],
                    -residence_rule
                ),
                "pressure"
            ),
            patri = fit_rf_model(
                sensitivity_data$patri |>
                select(
                    # Exclude any response variables other than pressure
                    -response_variables[response_variables != "pressure"],
                    -residence_rule
                ),
                "pressure"
            )
        ),
    
        class_survival = list(
            matri = fit_rf_model(
                sensitivity_data$matri |>
                select(
                    # Exclude any response variables other than survival
                    -response_variables[response_variables != "survival"],
                    -residence_rule
                ),
                "survival"
            ),
            patri = fit_rf_model(
                sensitivity_data$patri |>
                select(
                    # Exclude any response variables other than survival
                    -response_variables[response_variables != "survival"],
                    -residence_rule
                ),
                "survival"
            )
        )
    )
  
    rf_models_v12
}
