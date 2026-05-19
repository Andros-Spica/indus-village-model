load_or_build_pd_results <- function(
    model_version,
    overwrite = FALSE,
    sensitivity_data = NULL,
    rf_models = NULL,
    response_class = NULL,
    parameters = NULL,
    response_variables = c(
        "log_totalIndividuals",
        "survival"
    )
) {
    source("library/sensitivity/build_pd_results.R")

    # Define file path for pd results
    pd_file <- paste0(
        "data/pd/",
        gsub("\\.", "", model_version),
        "_pd_results.RData"
    )

    # Check if pd results file exists
    pd_file_exists <- file.exists(pd_file)

    if (pd_file_exists && !overwrite) {

        message(
            paste(
                "Loading existing partial dependence results for model version:",
                model_version
            )
        )
        
        load(pd_file)
    } else {
        message(
            paste(
                "No existing partial dependence results found for model version:",
                model_version,
                "or overwrite is set to TRUE. Building new results and plots."
            )
        )

        # Check that sensitivity_data and parameters are provided
        if (is.null(sensitivity_data)) {
            stop("sensitivity_data must be provided to build pd results.")
        }
        if (is.null(parameters)) {
            stop("parameters must be provided to build pd results.")
        }

        # Check that rf_models is provided if not loading existing results
        if (is.null(rf_models) && (!pd_file_exists || overwrite)) {
            stop("rf_models must be provided to build pd results.")
        }

        pd_results <- list()

        for (parameter in parameters) {

            # If pd results don't exist or we're overwriting results, build them
            message(
                paste(
                    "Building partial dependence results for parameter:",
                    parameter
                )
            )
            
            pd_results[[parameter]] <- build_pd_results(
                sensitivity_data = sensitivity_data,
                rf_models = rf_models,
                parameter = parameter,
                response_class = response_class
            )
        }
    }
    save(
    pd_results,
    file = pd_file
    )
    
    pd_results
  
}
