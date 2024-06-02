
# This is the targets pipeline

list(

  # Initialize -----------------------------------------------------------------

  ## ElectricityFilePath
  targets::tar_target_raw(
    "ElectricityFilePath",
    electricity_file,
    format = "file"),

  ## RailFilePath
  targets::tar_target_raw(
    "RailFilePath",
    rail_file,
    format = "file"
  ),

  ## LoadLRData
  tar_target(
    LoadLRData,
    command = load_long_run_data(paths = c(ElectricityFilePath, RailFilePath))
  )
)
