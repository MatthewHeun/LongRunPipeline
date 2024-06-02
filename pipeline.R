
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


  # Load long-run data ---------------------------------------------------------

  ## LRData
  tar_target(
    LRData,
    load_long_run_data(paths = c(ElectricityFilePath, RailFilePath))
  ),


  # Translate data -------------------------------------------------------------
  tar_target(
    TranslatedData,
    translate_to_clpfu(LRData)
  )

)
