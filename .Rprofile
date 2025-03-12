# Ensure required package is installed
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", quiet = TRUE)
}

library(here)

input_path = here("data")
output_path = here("replication_R/output")

Sys.setenv(INPUT_PATH = input_path)
Sys.setenv(output_path = output_path)

# Define a single function to retrieve datasets
get_data <- function(filename) {
  input_path <- Sys.getenv("INPUT_PATH")  # Retrieve the environment variable
  
  if (input_path == "") {
    stop("Error: INPUT_PATH is not set. Please define it first.")
  }
  
  file_path <- file.path(input_path, filename)  # Construct full path
  
  if (!file.exists(file_path)) {
    stop(paste("Error: The file", file_path, "does not exist."))
  }
  
  return(file_path)
}

output_folder <- function(filename) {
  input_path <- Sys.getenv("OUTPUT_PATH")  # Retrieve the environment variable
  
  if (output_path == "") {
    stop("Error: OUTPUT_PATH is not set. Please define it first.")
  }
  
  file_path <- file.path(output_path, filename)  # Construct full path
  
  if (!file.exists(file_path)) {
    stop(paste("Error: The file", file_path, "does not exist."))
  }
  
  return(file_path)
}

# Confirmation message
message("✅ Rprofile loaded: Use `get_data('filename.csv')` to load datasets.")
message("✅ Rprofile loaded: Use `output_folder(filename.tex)` to export files")
