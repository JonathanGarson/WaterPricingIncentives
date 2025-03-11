# This script convert the data of the authors in csv, a non-propietary format.

library(data.table)
library(haven)

files <- list.files(path = "data/input_primary/", pattern = "\\.dta$", full.names = TRUE)
for (file in files){
  df <- read_dta(file)  # Read the .dta file
  csv_file <- sub("\\.dta$", ".csv", file)  # Replace .dta with .csv
  fwrite(df, file = csv_file, row.names = FALSE)  # Save as .csv
  message(paste("Converted:", file, "➡", csv_file))
}