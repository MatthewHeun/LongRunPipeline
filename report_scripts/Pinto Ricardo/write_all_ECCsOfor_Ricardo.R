# This script reads and writes to Excel all of the
# ECCs in the long-run database.
# Seeing all of the ECCs is helpful for debugging purposes.

output_dir <- file.path("~/OneDrive - University of Leeds/Fellowship 1960-2015 PFU database research/Output Data/LongRun Database")

tar_read(PSUTLR) |>
  dplyr::mutate(
    WorksheetNames = paste(Dataset, Year, EnergyType, LastStage, sep = "-")
  ) |>
  Recca::write_ecc_to_excel(path = file.path(output_dir, "matrix_check_Ricardo.xlsx"),
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)
