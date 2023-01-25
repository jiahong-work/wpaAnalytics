combine_datasets <-
  function(.scoring_data,
           # dataframe of scoring data with regid
           .model_data,
           # dataframe of model scores with regid
           .scoring_cols,
           # column names of scoring data (no regid)
           .model_cols
           # column names of model data (no regid)
           ) {
           dplyr::left_join(.model_data,
                            collapse::get_vars(.scoring_data,
                                               c("rnc_regid", .scoring_cols))) %>%
             collapse::get_vars(c(.model_cols, .scoring_cols))
           }


#' Grab the top `n` features from a sorted data frame and combine.
#'
#' @param .df list. List of data frames containing feature importances
#' @param .models character vector.
grab_top_n_features <- function(.df, .models, .n = 25) {
  id_df <- dplyr::tibble(id = seq_along(.models) %>% as.character(),
                         models = .models)
  top_n <- purrr::map(.df, ~ .x %>% head(.n))
  dplyr::bind_rows(top_n, .id = "id") %>%
    dplyr::left_join(id_df)
}

plot_feat_imp <- function(.df,
                          .model_name = NULL,
                          .n = NULL,
                          .font_size = 38) {
  n <- .n %||% nrow(.df)
  .df %>%
    dplyr::slice_head(n = n) %>%
    ggplot2::ggplot(ggplot2::aes(val, reorder(key, val))) +
    ggplot2::geom_col(fill = "#C8102E", color = "#ffffff") +
    ggplot2::facet_wrap( ~ models, scales = "free") +
    ggplot2::labs(x = "Importance",
                  y = NULL,
                  subtitle = .model_name) +
    cowplot::theme_half_open(font_family = "Franklin Gothic Book",
                             font_size = .font_size)
}


run_feat_imp <-
  function(.df, .models, .score_cols, .samples = 5e3) {
    purrr::map(.models,
               ~ {
                 smp <- collapse::get_vars(.df, c(.x, .score_cols)) %>%
                   collapse::num_vars() %>%
                   dplyr::slice_sample(n = .samples)
                 cor_smp <- corrr::correlate(smp)
                 cor_smp %>%
                   dplyr::slice(1) %>%
                   tidyr::gather(key, val, -term) %>%
                   tidyr::drop_na(val) %>%
                   dplyr::mutate(abs = abs(val)) %>%
                   dplyr::arrange(dplyr::desc(abs)) %>%
                   collapse::fselect(key, val)
               })
  }
preproc_scoring <- function(.scoring, .corr_cutoff = .90, .n_size = NULL) {
  cli::cli_alert_info("Preprocessing scoring data")
  cli::cli_alert_info("Be patient this might take a while")
  scoring <- fst::read_fst(.scoring) %>%
    dataPreparation::fast_filter_variables(level = 2) %>%
    dataPreparation::fast_round(digits = 3) %>%
    collapse::qTBL()

  if(!is.null(.n_size)){
    scoring <- dplyr::slice_sample(scoring, n = .n_size)
  }

  cli::cli_alert_info("Imputing missing data")
  collapse::num_vars(scoring) <- collapse::num_vars(scoring) %>%
    collapse::replace_NA(-1)
  cli::cli_alert_info("Normalizing scoring data and applying correlation filter")
  rec_obj <- recipes::recipe( ~ ., data = scoring) %>%
    recipes::update_role(rnc_regid, new_role = "idcol") %>%
    recipes::step_zv(recipes::all_numeric_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_zv(recipes::all_numeric_predictors()) %>%
    recipes::step_nzv(recipes::all_numeric_predictors())
  scoring <- recipes::prep(rec_obj, scoring) %>% recipes::juice()
  scoring
}

discover_top_features <- function(.scoring_file,
                                  .model_file,
                                  .clean_names,
                                  .top_feats = 250,
                                  .n_size = NULL) {
  scoring <- preproc_scoring(.scoring = .scoring_file, .n_size = .n_size)
  cln <- vroom::vroom(.clean_names)
  # combine modeling and scoring data ----
  models <- rio::import(.model_file)
  model_cols <- setdiff(colnames(models), "rnc_regid")
  score_cols <- setdiff(colnames(scoring), "rnc_regid")
  df <- combine_datasets(scoring, models, score_cols, model_cols)

  feat_imp <- run_feat_imp(df, model_cols, score_cols)
  m_df <- grab_top_n_features(feat_imp, model_cols, .top_feats) %>%
    dplyr::mutate(models = stringr::str_replace_all(models, "_", " "))
  feat_cleaner(m_df, cln)
}



export_feature_importances <- function(.df,
                                       .model_col = "models",
                                       .n_feats = 10,
                                       .size = 38,
                                       .export_path = fs::path_wd()) {

  plot_s <- .df %>%
    dplyr::mutate(val = as.numeric(val)) %>%
    dplyr::mutate(key = stringr::str_replace_all(key,"_"," ")) %>%
    dplyr::group_split(!!rlang::sym(.model_col)) %>%
    purrr::map(~ plot_feat_imp(
      .df = .x,
      .n = .n_feats,
      .font_size = .size
    ))

  fi_models <-
    dplyr::group_split(.df, !!rlang::sym(.model_col), .keep = TRUE) %>%
    purrr::map_chr(~ dplyr::pull(.x, models) %>% unique())


  plot_s %>%
    purrr::walk2(fi_models,
                 ~ {
                   ggplot2::ggsave(
                     glue::glue("{.export_path}/fi_{.y}.png"),
                     .x,
                     # plot
                     device = ragg::agg_png(
                       width = 6,
                       height = 6,
                       units = "in",
                       scaling = 1,
                       res = 800
                     )
                   )
                   dev.off()
                 })

}


feat_cleaner <- function(.df, .clean) {
  .df %>%
    dplyr::mutate(var = key) %>%
    dplyr::left_join(.clean %>% mutate(var = tolower(var))) %>%
    dplyr::mutate(key = ifelse(is.na(clean_names), var, clean_names)) %>%
    dplyr::select(id, key, val, models, desc = description)
}

