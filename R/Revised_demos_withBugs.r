
#' Make a new reg column while deleting the RegistrationDate col
#'
#' @param .df Data frame to add new reg col
#' @param .date The cutoff date for new reg
#' @return
#' @export
#'
#' @examples
ana_mk_new_reg <- function(.df,.date='2020-11-03'){
  .df |>
    collapse::ftransform(NewReg = RegistrationDate >= lubridate::as_date(.date)) |>
    collapse::fselect(-RegistrationDate)
}

