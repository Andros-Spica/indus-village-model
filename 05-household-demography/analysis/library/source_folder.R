# Source - https://stackoverflow.com/a/20083589
# Posted by Ricardo Saporta
# Retrieved 2026-06-11, License - CC BY-SA 3.0

## finds all .R files within a folder and soruces them
source_folder <- function(folder_name, verbose=FALSE, show_warnings=TRUE) { 
  files <- list.files(folder_name, full.names=TRUE)

  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]

  if (!length(files) && show_warnings)
    warning("No R files in ", folder_name)

  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}
