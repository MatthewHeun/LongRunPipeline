

# For debugging: tar_make(callr_function = NULL, use_crew = FALSE, as_job = FALSE)
# and
# (1) insert browser() calls for functions in PFUPipeline2
# (2) set breakpoints in functions from other packages.

# Get file paths ---------------------------------------------------------------

electricity_file <- file.path("data", "world_electricity_finalrevisited_v19_energy.xlsx")
transport_file <- file.path("data", "EnergyRecords_Transport_extract_V19.xlsx")
