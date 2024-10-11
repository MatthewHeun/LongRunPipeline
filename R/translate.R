#' Convert Long-Run data format to the CL-PFU database format
#'
#' @param .df A data frame read by the `LoadLRData` target.
#'
#' @return A translated data frame.
#'
#' @export
translate_to_clpfu <- function(.df,
                               e_dot = IEATools::iea_cols$e_dot,
                               energy = "Energy",
                               exergy = "Exergy",
                               etype = IEATools::energy_types$e,
                               xtype = IEATools::energy_types$x,
                               in_quantity = "in_Quantity",
                               out_quantity = "out_Quantity") {

  .df |>
    dplyr::filter(!startsWith(out_Hierarchy, "Losses")) |>
    dplyr::mutate(
      # Get rid of unneeded columns
      in_Group = NULL,
      out_Group = NULL,
      in_Hierarchy = NULL,
      out_Hierarchy = NULL,
      Country = NULL,
      Continent = NULL,
      # Adjust energy and exergy strings
      property = dplyr::case_when(
        property == "Energy" ~ etype,
        property == "Exergy" ~ xtype,
        TRUE ~ NA_character_
      ),
      "{in_quantity}" := dplyr::case_when(
        in_Unit == "GWh" ~ in_Quantity * 3.6, # Convert GWh to TJ
        TRUE ~ NA_real_
      ),
      "{out_quantity}" := dplyr::case_when(
        out_Unit == "GWh" ~ out_Quantity * 3.6, # Convert GWhr to TJ
        TRUE ~ NA_real_
      ),
      # Eliminate unit columns in favor of a single unit column
      in_Unit = NULL,
      out_Unit = NULL,
      Unit = "TJ"
    ) |>
    dplyr::rename(
      EnergyType = property,
      # Pick up "World" as the Country column
      Country = Region,
      Dataset = dataset
    ) |>
    # Eliminate rows where both in_Quantity and out_Quantity are 0
    dplyr::filter(in_Quantity != 0 & out_Quantity != 0) |>
    tidyr::pivot_longer(dplyr::all_of(c("in_Quantity", "out_Quantity")),
                        names_to = "direction",
                        values_to = e_dot)
}
