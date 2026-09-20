# Install R.utils if not already present
if (!requireNamespace("R.utils", quietly = TRUE)) install.packages("R.utils")

# Find all CSV files in current folder and subfolders
csv_files <- list.files(path = "simdata", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Compress each CSV into .csv.gz (removes original .csv once compressed)
for (f in csv_files) {
  message("Compressing: ", f)
  R.utils::gzip(f, overwrite = TRUE, remove = TRUE)
}
