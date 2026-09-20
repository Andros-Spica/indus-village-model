recombine_all_parts <- function(root_dir = "simdata", cleanup = FALSE) {
  if (!dir.exists(root_dir)) {
    stop(paste("Directory", root_dir, "does not exist."))
  }

  # Locate all chunked part files recursively
  part_files <- list.files(
    path = root_dir,
    pattern = "\\.part[0-9]+$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(part_files) == 0) {
    message("No chunked .part files found in '", root_dir, "'. Everything is ready to go.")
    return(invisible(NULL))
  }

  # Derive the original base file path by stripping the .part extension
  base_files <- unique(sub("\\.[Pp][Aa][Rr][Tt][0-9]+$", "", part_files))

  message(sprintf("Found %d file(s) requiring reassembly in '%s'.", length(base_files), root_dir))

  for (base_file in base_files) {
    message("Reconstructing: ", basename(base_file), " ...")

    # Match all chunks corresponding to this specific file
    pattern <- paste0("^", rex::escape(basename(base_file)), "\\.[Pp][Aa][Rr][Tt][0-9]+$")
    file_parts <- sort(list.files(
      path = dirname(base_file),
      pattern = pattern,
      full.names = TRUE
    ))

    # Binary stream stitch (64MB memory buffer)
    out_con <- file(base_file, "wb")
    chunk_buffer_size <- 64 * 1024 * 1024 # 64 MB

    for (part in file_parts) {
      in_con <- file(part, "rb")
      while (length(buf <- readBin(in_con, "raw", n = chunk_buffer_size)) > 0) {
        writeBin(buf, out_con)
      }
      close(in_con)
    }
    close(out_con)

    message("Finished: ", basename(base_file))

    # Optional: Delete parts after successful reassembly
    if (isTRUE(cleanup)) {
      unlink(file_parts)
      message("Deleted chunk parts for: ", basename(base_file))
    }
  }

  message("All files successfully reassembled.")
}

# Run the reassembly
recombine_all_parts("simdata", cleanup = FALSE)
