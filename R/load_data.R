load_long_run_data <- function(paths) {
  lapply(paths, function(this_path) {
    readxl::read_excel(this_path)
  }) |>
    dplyr::bind_rows()
}
