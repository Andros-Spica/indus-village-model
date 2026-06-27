save_trajectories <- function(
    trajectories,
    model_version,
    suffix = NULL
) {
    output_file <- paste0(
    "data/",
    gsub("\\.", "", model_version),
    "_trajectories",
    ifelse(!is.null(suffix), paste0("_", suffix), ""),
    ".RData"
    )

    saveRDS(trajectories, file = output_file)
}
