um_tow_means <- function(.model, .turnout) {
  collapse::fsum(.model * .turnout) /
    collapse::fsum(.turnout)
}
ana_model_avg_by_demo <- function(.df, .demos, .models) {
  purrr::map(.demos,
             ~ {
               collapse::collap(.df,
                                by = as.formula(paste("~", .x)),
                                FUN = collapse::fmean,
                                cols = .models) %>%
                 dplyr::mutate(Demo = .x) %>%
                 dplyr::rename(Level = !!rlang::sym(.x)) %>%
                 dplyr::mutate(Level = as.character(Level)) %>%
                 dplyr::select(Demo, Level, dplyr::everything())
             }) %>%
    dplyr::bind_rows()
}

ana_model_sum_by_demo <- function(.df, .demos, .models) {
  purrr::map(.demos,
             ~ {
               collapse::collap(.df,
                                by = as.formula(paste("~", .x)),
                                FUN = collapse::fsum,
                                cols = .models) %>%
                 dplyr::mutate(Demo = .x) %>%
                 dplyr::rename(Level = !!rlang::sym(.x)) %>%
                 dplyr::mutate(Level = as.character(Level)) %>%
                 dplyr::select(Demo, Level, dplyr::everything())
             }) %>%
    dplyr::bind_rows()
}

ana_model_w_avg_by_demo <- function(.df, .demos, .models, .turnout) {
  purrr::map(.demos,
             ~ {
               .df %>%
                 dplyr::group_by(!!rlang::sym(.x)) %>%
                 dplyr::summarise(dplyr::across(
                   .cols = dplyr::all_of(.models),
                   .fns = ~ um_tow_means(.x, !!rlang::sym(.turnout))
                 )) %>%
                 dplyr::mutate(Demo = .x) %>%
                 dplyr::rename(Level = !!rlang::sym(.x)) %>%
                 dplyr::mutate(Level = as.character(Level)) %>%
                 dplyr::select(Demo, Level, dplyr::everything())
             }) %>%
    dplyr::bind_rows()
}

ana_model_w_sum_by_demo <- function(.df, .demos, .models, .turnout) {
  purrr::map(.demos,
             ~ {
               .df %>%
                 dplyr::group_by(!!rlang::sym(.x)) %>%
                 dplyr::summarise(dplyr::across(
                   .cols = dplyr::all_of(.models),
                   .fns = ~ um_mod_sums(.x, !!rlang::sym(.turnout))
                 )) %>%
                 dplyr::mutate(Demo = .x) %>%
                 dplyr::rename(Level = !!rlang::sym(.x)) %>%
                 dplyr::mutate(Level = as.character(Level)) %>%
                 dplyr::select(Demo, Level, dplyr::everything())
             }) %>%
    dplyr::bind_rows()
}

ana_export_models_by_demo <- function(.df, .demos, .models, .turnout, .path=fs::path_wd()){
  raw_avgs <-
    ana_model_avg_by_demo(.df = .df,
                          .demos = .demos,
                          .models = .models)
  raw_sums <-
    ana_model_sum_by_demo(.df = .df,
                          .demos = .demos,
                          .models = .models)
  tow_avgs <-
    ana_model_w_avg_by_demo(
      .df = .df,
      .demos = .demos,
      .models = .models,
      .turnout = .turnout
    )
  tow_sums <-
    ana_model_w_sum_by_demo(
      .df = .df,
      .demos = .demos,
      .models = .models,
      .turnout = .turnout
    )
  build_excel_workbook(
    .dfs = list(raw_avgs, raw_sums ,tow_avgs, tow_sums),
    .sheets = c("means", "sums","turnout weighted mean", "turnout weighted sums"),
    .fname = .path
  )
}


