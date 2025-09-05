reformat_nlrx_dataset <- function(dataset, default_parameters) {
  
  require(dplyr)
  
  # Simplify names:
  dataset <- dataset %>%
    rename_with( ~ gsub("\\-", "_", oldnames)) %>%
    rename_with( ~ gsub("\\[", "", oldnames)) %>%
    rename_with( ~ gsub("\\]", "", oldnames))
  
  # Calculate relative metrics:
  total_number_patches <- default_parameters$`gui_area-width` * default_parameters$`gui_area-height`
  
  dataset <- dataset %>% 
    mutate(dairy_hut_density_percent = 100 * dairy_hut_count / total_number_patches,
           mean_occupation_layers_per_hut = total_occupation_layers / dairy_hut_count)
  
  return(dataset)
}

reformat_bs_dataset <- function(dataset, default_parameters) {
  
  require(dplyr)
  
  # Rename run number and step:
  dataset <- dataset %>%
    rename(run_number = X.run.number.) %>%
    rename(step = X.step.)
  
  # Calculate relative metrics:
  total_number_patches <- default_parameters$`gui_area-width` * default_parameters$`gui_area-height`
  
  dataset <- dataset %>% 
    mutate(dairy_hut_density_percent = 100 * `dairy-hut-count` / total_number_patches,
           mean_occupation_layers_per_hut = `total-occupation-layers` / `dairy-hut-count`)
  
  return(dataset)
}

get_end_states <- function(dataset, parameter_config) {
  
  endstates_dataset <- dataset %>% 
    group_by(parameter_config) %>%
    filter(step == max (step))
  
  return(endstates_dataset)
}
