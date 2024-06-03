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
                              matnames = IEATools::mat_meta_cols$matnames,
                              matvals = IEATools::mat_meta_cols$matvals,
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
  dplyr::bind_rows(R, UV, Y) |>
    dplyr::rename(
      "{matvals}" := E_dot
    ) |>
    dplyr::mutate(
      direction = NULL,
      in_Name = NULL,
      in_Sector = NULL,
      t_Type = NULL,
      t_Group = NULL,
      t_Name = NULL,
      t_Efficiency = NULL,
      out_Name = NULL,
      out_Sector = NULL
    )
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

  browser()

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





