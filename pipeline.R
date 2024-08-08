
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
  ## TranslatedData
  tar_target(
    TranslatedData,
    translate_to_clpfu(LRData)
  ),


  # Separate last stage final and last stage useful versions -------------------
  ## SeparatedFU
  tar_target(
    SeparatedFU,
    separate_last_stages(TranslatedData)
  ),


  # Add matrix names -----------------------------------------------------------
  ## WithMatnames
  tar_target(
    WithMatnames,
    add_psut_matnames(SeparatedFU)
  ),


  # Convert to PSUT format -----------------------------------------------------
  ## PSUTLR
  tar_target(
    PSUTLR,
    make_lr_psut(WithMatnames)
  ),


  # Check energy balances ------------------------------------------------------
  ## BalancedBeforeLR
  targets::tar_target(
    Balanced,
    PSUTLR |>
      dplyr::filter(Dataset == "world_electricity", Year == 1900) |>
      Recca::verify_SUT_energy_balance_with_units()
  )



)
