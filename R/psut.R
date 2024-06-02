#' Add matrix names for the PSUT format
#'
#' @param .df A data frame from the TranslatedData target
#' @param matnames The name of the column that holds matrix names
#' @param R,U,U_feed,V,Y Names for matrices
#'
#' @return A data frame with an additional matnames column
#'
#' @export
add_psut_matnames <- function(.df,
                              R = IEATools::psut_cols$R,
                              U = IEATools::psut_cols$U,
                              U_feed = IEATools::psut_cols$U_feed,
                              V = IEATools::psut_cols$V,
                              Y = IEATools::psut_cols$Y,
                              matnames = IEATools::mat_meta_cols$matnames,
                              rownames = IEATools::mat_meta_cols$rownames,
                              colnames = IEATools::mat_meta_cols$colnames,
                              rowtypes = IEATools::mat_meta_cols$rowtypes,
                              coltypes = IEATools::mat_meta_cols$coltypes,
                              industry = IEATools::row_col_types$industry,
                              product = IEATools::row_col_types$product) {

  # U and V matrices are easy to identify based on
  # in and out quantities

  UV <- .df |>
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        direction == "in_Quantity" ~ U_feed,
        direction == "out_Quantity" ~ V,
        TRUE ~ NA_character_
      ),
      "{rownames}" := dplyr::case_when(
        .data[[matnames]] == U_feed ~ in_Name,
        .data[[matnames]] == V ~ t_Name
      ),
      "{colnames}" := dplyr::case_when(
        .data[[matnames]] == U_feed ~ t_Name,
        .data[[matnames]] == V ~ out_Name
      ),
      "{rowtypes}" := dplyr::case_when(
        .data[[matnames]] == U_feed ~ product,
        .data[[matnames]] == V ~ industry
      ),
      "{coltypes}" := dplyr::case_when(
        .data[[matnames]] == U_feed ~ industry,
        .data[[matnames]] == V ~ product
      )
    )
  # Y matrix entries are identified by rows where
  # the out_Sector column is not "Unspecified"
  Y <- .df |>
    dplyr::filter(out_Sector != "Unspecified") |>
    dplyr::mutate(
      "{matnames}" := Y,
      "{rownames}" := in_Name,
      "{colnames}" := out_Sector,
      "{rowtypes}" := product,
      "{coltypes}" := industry
    )
  # R matrix entries are identified by rows where
  # the t_Type starts with Primary
  R <- .df |>
    dplyr::filter(startsWith(.data[["t_Type"]], "Primary")) |>
    dplyr::mutate(
      "{matnames}" := R,
      "{rownames}" := RCLabels::paste_pref_suff(pref = "Resources",
                                                suff = in_Name,
                                                notation = RCLabels::of_notation),
      "{colnames}" := in_Name,
      "{rowtypes}" := industry,
      "{coltypes}" := product
    )
  # Now stack the data frames and return
  dplyr::bind_rows(R, UV, Y)
}
