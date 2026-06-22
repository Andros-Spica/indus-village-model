load_or_build_pd_results <- function(
    model_version,
    overwrite = FALSE,
    sensitivity_data = NULL,
    rf_models = NULL,
    parameters = NULL,
    response_variable = NULL,
    response_class = NULL,
    verbose = TRUE
) {
    source("library/sensitivity/build_pd_results.R")

    # Define file path for pd results
    pd_file <- paste0(
        "data/pd/",
        gsub("\\.", "", model_version),
        "_",
        response_variable,
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
                "or overwrite is set to TRUE. Building new results."
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

        pd <- list(
            metadata = list(
                model_version = model_version,
                date = Sys.time(),
                parameters = parameters,
                response_variable = response_variable,
                response_class = response_class,
                r_version = version$version.string
            ),

            results = list()
        )

        if (verbose) {
            message(
                paste(
                    "Processing response variable:",
                    response_variable
                )
            )
        }
        
        for (parameter in parameters) {

            # If pd results don't exist or we're overwriting results, build them
            if (verbose) {
                message(
                    paste(
                        "Building partial dependence results for parameter:",
                        parameter
                    )
                )
            }
            
            pd[[parameter]] <- build_pd_results(
                sensitivity_data = sensitivity_data,
                rf_models = rf_models[[response_variable]],
                parameter = parameter,
                response_variable = response_variable,
                response_class = response_class
            )
        }
        save(
        pd,
        file = pd_file
        )
    }
    
    pd
  
}
