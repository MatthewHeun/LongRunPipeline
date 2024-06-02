#' Convert Long-Run data format to the CL-PFU database format
#'
#' @param .df A data frame read by the `LoadLRData` target.
#'
#' @return A translated data frame.
#'
#' @export
translate_to_clpfu <- function(.df) {
  .df |>
    dplyr::mutate(
      property = dplyr::case_match(
        .data[[property]] == "Energy" ~ "E",
        .data[[property]] == "Exergy" ~ "X",
        TRUE ~ NA_character_
      )
    )
}
