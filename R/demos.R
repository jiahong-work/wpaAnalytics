




wpa_mk_new_reg <- function(.df){
  .df |>
    ftransform(NewReg = RegistrationDate >= lubridate::as_date()) |>
    fselect(-RegistrationDate)
}

