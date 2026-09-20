sort_gz <- function(simdata_dir = "simdata") {
    # Define file routing rules
    routing_map <- list(
    "v1.1"   = c(
        "household-demography-main[ .]exp-trajectories-table", 
        "household-demography-main[ .]exp-trajectories-symmetric-table", 
        "household-demography-main[ .]exp-trajectories-cdmlt-level-table",
        "household-demography-main[ .]exp-trajectories-c1-fert-table",
        "household-demography-main[ .]exp-endstates-patri-table",
        "household-demography-main[ .]exp-endstates-matri-table"),
    "v1.2"   = c(
        "household-demography-main_density[ .]exp-trajectories-table",
        "household-demography-main_density[ .]exp-trajectories-density-effect-scaling-factor-table",
        "household-demography-main_density[ .]exp-endstates-patri-table",
        "household-demography-main_density[ .]exp-endstates-matri-table"
        ),
    "v1.2.1" = c(
        "household-demography-main_density-K-demand[ .]exp-trajectories-table",
        "household-demography-main_density-K-demand[ .]exp-endstates-patri-table",
        "household-demography-main_density-K-demand[ .]exp-endstates-matri-table"
        ),
    "v1.2.2" = c(
        "household-demography-main_density-K-demand-supply[ .]exp-trajectories-table",
        "household-demography-main_density-K-demand-supply[ .]exp-trajectories-labour-demand-per-capita-table",
        "household-demography-main_density-K-demand-supply[ .]exp-endstates-patri-table",
        "household-demography-main_density-K-demand-supply[ .]exp-endstates-matri-table"
        ),
    "v1.3"   = c(
        "household-demography-daily-main[ .]exp-trajectories-table",
        "household-demography-daily-main[ .]exp-trajectories-symmetric-table",
        "household-demography-daily-main[ .]exp-trajectories-cdmlt-level-table",
        "household-demography-daily-main[ .]exp-trajectories-c1-fert-table",
        "household-demography-daily-main[ .]exp-endstates-patri-table",
        "household-demography-daily-main[ .]exp-endstates-matri-table"
        ),
    "v1.4"   = c(
        "household-demography-daily-main_density[ .]exp-trajectories-table",
        "household-demography-daily-main_density[ .]exp-trajectories-density-effect-scaling-factor-table",
        "household-demography-daily-main_density[ .]exp-endstates-patri-table",
        "household-demography-daily-main_density[ .]exp-endstates-matri-table"
        ),
    "v1.4.1" = c(
        "household-demography-daily-main_density-K-demand[ .]exp-trajectories-table",
        "household-demography-daily-main_density-K-demand[ .]exp-endstates-patri-table",
        "household-demography-daily-main_density-K-demand[ .]exp-endstates-matri-table"
        ),
    "v1.4.2" = c(
        "household-demography-daily-main_density-K-demand-supply[ .]exp-trajectories-table",
        "household-demography-daily-main_density-K-demand-supply[ .]exp-trajectories-labour-demand-per-capita-table",
        "household-demography-daily-main_density-K-demand-supply[ .]exp-trajectories-labour-demand-per-capita-v2-table",
        "household-demography-daily-main_density-K-demand-supply[ .]exp-endstates-patri-table",
        "household-demography-daily-main_density-K-demand-supply[ .]exp-endstates-matri-table"
        )
    )

    # Move files to matching subdirectories
    for (version_dir in names(routing_map)) {
    target_dir <- file.path(simdata_dir, version_dir)
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
    
    for (pattern in routing_map[[version_dir]]) {
        matched_files <- list.files(simdata_dir, pattern = pattern, full.names = TRUE)
        # Ignore files already inside subdirectories
        top_level_files <- matched_files[!file.info(matched_files)$isdir & dirname(matched_files) == simdata_dir]
        
        if (length(top_level_files) > 0) {
            clean_name <- sub("-main\\.exp-", "-main exp-", basename(top_level_files))
            file.rename(top_level_files, file.path(target_dir, clean_name))
        }
    }
    }
}

sort_gz("simdata")
