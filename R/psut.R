
#' Separate last stage final from last stage useful
#'
#' The Mexer database format includes a `LastStage` column
#' that specifies whether the last stage of the ECC is final or useful energy.
#' This function separates the raw data in Rodrigo format
#' into last stage final and last stage useful versions
#'
#' @param .df A data frame, the `TranslatedData` target.
#'
#' @return A data frame with a new `LastStage` column filled with "Final" or "Useful"
#'
#' @export
separate_last_stages <- function(.df,
                                 last_stage = IEATools::iea_cols$last_stage,
                                 t_Type = "t_Type",
                                 final = IEATools::last_stages$final,
                                 useful = IEATools::last_stages$useful,
                                 final_to_useful = "Final to useful") {

  # Create the last stage Final version
  ls_final <- .df |>
    # Eliminate the final-to-useful transformations
    dplyr::filter(.data[[t_Type]] != final_to_useful) |>
    dplyr::mutate(
      "{last_stage}" := final
    )

  # Create the last stage Useful version
  ls_useful <- .df |>
    dplyr::mutate(
      "{last_stage}" := useful
    )

  dplyr::bind_rows(ls_final, ls_useful)
}


#' Add matrix names for the PSUT format
#'
#' @param .df A data frame from the TranslatedData target.
#' @param R,U,U_feed,V,Y Matrix names.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes,industry,product See IEATools::mat_meta_cols.
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
                              e_dot = IEATools::iea_cols$e_dot,
                              last_stage = IEATools::iea_cols$last_stage,
                              final = IEATools::last_stages$final,
                              useful = IEATools::last_stages$useful,
                              matnames = IEATools::mat_meta_cols$matnames,
                              matvals = IEATools::mat_meta_cols$matvals,
                              rownames = IEATools::mat_meta_cols$rownames,
                              colnames = IEATools::mat_meta_cols$colnames,
                              rowtypes = IEATools::mat_meta_cols$rowtypes,
                              coltypes = IEATools::mat_meta_cols$coltypes,
                              industry = IEATools::row_col_types$industry,
                              product = IEATools::row_col_types$product,
                              in_name = "in_Name",
                              t_name = "t_Name",
                              t_type = "t_Type",
                              t_group = "t_Group",
                              t_efficiency = "t_Efficiency",
                              direction = "direction",
                              out_name = "out_Name",
                              in_sector = "in_Sector",
                              out_sector = "out_Sector",
                              in_quantity = "in_Quantity",
                              primary = "Primary") {

  browser()

  # R matrix entries are identified by rows where
  # the t_Type starts with Primary and
  # the direction is "in_Quantity".
  R_mats <- .df |>
    dplyr::filter(startsWith(.data[[t_type]], primary),
                  .data[[direction]] == in_quantity) |>
    dplyr::mutate(
      "{matnames}" := R,
      "{rownames}" := RCLabels::paste_pref_suff(pref = "Resources",
                                                suff = in_Name,
                                                notation = RCLabels::of_notation),
      "{colnames}" := in_name,
      "{rowtypes}" := industry,
      "{coltypes}" := product
    )
  # U and V matrices are easy to identify based on
  # in and out quantities
  UV_mats <- .df |>
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        direction == "in_Quantity" ~ U_feed,
        direction == "out_Quantity" ~ V,
        TRUE ~ NA_character_
      ),
      "{rownames}" := dplyr::case_when(
        .data[[matnames]] == U_feed ~ .data[[in_name]],
        .data[[matnames]] == V ~ .data[[t_name]],
        TRUE ~ NA_character_
      ),
      "{colnames}" := dplyr::case_when(
        .data[[matnames]] == U_feed ~ .data[[t_name]],
        .data[[matnames]] == V ~ .data[[out_name]],
        TRUE ~ NA_character_
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
  # Calculate Y matrices when last stage is final
  Y_final_mats <- .df |>
    dplyr::filter(.data[[out_sector]] != "Unspecified",
                  .data[[direction]] == in_quantity,
                  .data[[t_type]] == "Gross Final to Final",
                  .data[[last_stage]] == final) |>
    dplyr::mutate(
      "{matnames}" := Y,
      "{rownames}" := .data[[out_name]],
      "{colnames}" := .data[[out_sector]],
      "{rowtypes}" := product,
      "{coltypes}" := industry
    )
  # Calculate Y matrices when last stage is useful
  Y_useful_mats <- .df |>
    dplyr::filter(.data[[out_sector]] != "Unspecified",
                  .data[[direction]] == in_quantity,
                  .data[[t_type]] == "Final to Useful",
                  .data[[last_stage]] == useful) |>
    dplyr::mutate(
      "{matnames}" := Y,
      "{rownames}" := .data[[out_name]],
      "{colnames}" := .data[[out_sector]],
      "{rowtypes}" := product,
      "{coltypes}" := industry
    )

  # Now stack the data frames and return
  dplyr::bind_rows(R_mats, UV_mats, Y_final_mats, Y_useful_mats) |>
    dplyr::rename(
      "{matvals}" := e_dot
    ) |>
    dplyr::mutate(
      .data[[direction]] := NULL,
      .data[[in_name]] := NULL,
      .data[[in_sector]] := NULL,
      .data[[t_type]] := NULL,
      .data[[t_group]] := NULL,
      .data[[t_name]] := NULL,
      .data[[t_efficiency]] := NULL,
      .data[[out_name]] := NULL,
      .data[[out_sector]] := NULL
    ) |>
    # Eliminate duplicated input energy rows.
    unique()
}


#' Convert expanded data frame to a PSUT data frame
#'
#' @param .df A data frame with matrix names, row/col names, and row/col types
#'
#' @return A `matsindf` data frame of PSUT matrices.
#'
#' @export
make_lr_psut <- function(.df,
                         matnames = IEATools::mat_meta_cols$matnames,
                         matvals = IEATools::mat_meta_cols$matvals,
                         rownames = IEATools::mat_meta_cols$rownames,
                         colnames = IEATools::mat_meta_cols$colnames,
                         rowtypes = IEATools::mat_meta_cols$rowtypes,
                         coltypes = IEATools::mat_meta_cols$coltypes,
                         industry = IEATools::row_col_types$industry,
                         product = IEATools::row_col_types$product) {

  S_units_df <- .df |>
    calc_S_units(matnames = matnames,
                 rownames = rownames,
                 colnames = colnames)
  .df |>
    dplyr::group_by(Dataset, Country, Year, EnergyType, matnames) |>
    matsindf::collapse_to_matrices(matnames = matnames,
                                   matvals = matvals,
                                   rownames = rownames,
                                   colnames = colnames,
                                   rowtypes = rowtypes,
                                   coltypes = coltypes,
                                   matrix_class = "Matrix") |>
    tidyr::pivot_wider(names_from = matnames, values_from = matvals) |>
    dplyr::mutate(
      U_EIOU = matsbyname::hadamardproduct_byname(U_feed, 0),
      U = U_feed,
      r_EIOU = U_EIOU
    ) |>
    dplyr::left_join(S_units_df, by = c("Dataset", "Country", "Year", "EnergyType"))
}


calc_S_units <- function(.df,
                         matnames = IEATools::mat_meta_cols$matnames,
                         matvals = IEATools::mat_meta_cols$matvals,
                         rownames = IEATools::mat_meta_cols$rownames,
                         colnames = IEATools::mat_meta_cols$colnames,
                         rowtypes = IEATools::mat_meta_cols$rowtypes,
                         coltypes = IEATools::mat_meta_cols$coltypes,
                         unit = IEATools::row_col_types$unit,
                         product = IEATools::row_col_types$product,
                         R = IEATools::psut_cols$R,
                         U = IEATools::psut_cols$U,
                         U_feed = IEATools::psut_cols$U_feed,
                         V = IEATools::psut_cols$V,
                         Y = IEATools::psut_cols$Y,
                         s_units = IEATools::psut_cols$s_units) {
  # At this point, we have a data frame of matnames and row/col names.
  # Use these to create the S_units column of vectors.
  .df |>
    dplyr::mutate(
      rownames = dplyr::case_when(
        .data[[matnames]] == R ~ .data[[colnames]],
        .data[[matnames]] == U_feed ~ .data[[rownames]],
        .data[[matnames]] == V ~ .data[[colnames]],
        .data[[matnames]] == Y ~ .data[[rownames]]
      ),
      matvals = 1,
      colnames = "TJ",
      rowtypes = product,
      coltypes = unit,
      matnames = s_units,
      Unit = NULL
    ) |>
    unique() |>
    dplyr::group_by(Country, Year, EnergyType, Dataset, matnames) |>
    matsindf::collapse_to_matrices(matnames = matnames,
                                   matvals = matvals,
                                   rownames = rownames,
                                   colnames = colnames,
                                   rowtypes = rowtypes,
                                   coltypes = coltypes,
                                   matrix_class = "Matrix") |>
    tidyr::pivot_wider(names_from = matnames, values_from = matvals)
}





