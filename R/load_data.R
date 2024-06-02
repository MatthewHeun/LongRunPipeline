#' Load long-run database data
#'
#' @param paths A string vector of file paths for long-run database datasets
#'
#' @return A data frame with all loaded files.
#'
#' @export
load_long_run_data <- function(paths) {
  lapply(paths, function(this_path) {
    readxl::read_excel(this_path)
  }) |>
    dplyr::bind_rows()
}
