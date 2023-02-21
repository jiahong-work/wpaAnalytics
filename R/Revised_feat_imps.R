#' Combine the modeling scores with the scoring data
#'
#' @param .scoring_df dataframe of scoring data with regid
#' @param .model_df dataframe of model scores with regid
#' @param .scoring_cols column names of scoring data (no regid)
#' @param .model_cols column names of model data (no regid)

#'
#' @return Data frame
#' @export
ana_combine_datasets <-
  function(.scoring_df,
           .model_df,
           .scoring_cols,
           .model_cols  ) {
    dplyr::left_join(.model_df,
                     collapse::get_vars(.scoring_df,
                                        c("rnc_regid", .scoring_cols))) %>%
      collapse::get_vars(c(.model_cols, .scoring_cols))
  }



#' Grab the top `n` features from a sorted data frame and combine.
#'
#' @param .df list. List of data frames containing feature importances
#' @param .models character vector.
ana_grab_top_n_features <- function(.df, .models, .n = 25) {
  id_df <- dplyr::tibble(id = seq_along(.models) %>% as.character(),
                         models = .models)
  top_n <- purrr::map(.df, ~ .x %>% head(.n))
  dplyr::bind_rows(top_n, .id = "id") %>%
    dplyr::left_join(id_df)
}


#' Plot feature importance
#'
#' @param .df Data frame that contains all the feature importances, usually generated frim ana_grab_top_n_features
#' @param .model_name The model name of which you want to name the subtitle
#' @param .n Top n models that you want to map feature importances. If null, then the function will print all.
#' @param .font_size Font size of the feature importance charts
#'
#' @return ggplot object
#' @export
ana_plot_feat_imp <- function(.df,
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


#' Calculate the feature importance of each scoring data when predicting a modeling 
#'
#' @param .df Data frame that contains both the modeling scores and the scoring data
#' @param .model_cols Modeling scores of which we want to draw feature importance
#' @param .score_cols Scoring data, which we want to display their importances in predicting teh model scores
#' @param .samples Number of samples that we want to use to calculate the feature importance. Defaults to `5e3`
#'
#' @return Data frame
#' @export
#'#' 
ana_run_feat_imp <-
  function(.df, .model_cols, .score_cols, .samples = 5e3) {
    purrr::map(.model_cols,
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


#' Preprocessing the scoring data
#'
#' @param .scoring_path the file path(in fst format) of the scoring data
#' @param .n_size Size of the scoring data that you want to preprocess and use. 
#'                If Null, the function will process the whole scoring data. Defaults to `NULL`.

#' @param .corr_cutoff This parameter is never used, I have removed it.
#' 
#'
#' @return Data frame, scoring data that have been preprocessed
#' @export
#'
#' @examples
#' ana_preproc_scoring(
#' .scoring_path='~/AnalyticsShare/AZ/OurAmerica_24G/Data/Scoring_Data_AZ.fst',
#' .n_size=NULL)
ana_preproc_scoring <- function(.scoring_path, 
                            #.corr_cutoff = .90, 
                            .n_size = NULL) {
  cli::cli_alert_info("Preprocessing scoring data")
  cli::cli_alert_info("Be patient this might take a while")
  scoring <- fst::read_fst(.scoring_path) %>%
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






#' Pipeline that combines all feature importance functions and export a cleaned feature importance table
#'
#' @param .scoring_path The file path of the scoring data
#' @param .model_path The file path of the modeling scores
#' @param .clean_names The dictionary table that contains all the cleaned names of the scoring data columns
#' @param .top_feats Top n important features that you want to grab
#' @param .n_size Size of the scoring data that you want to preprocess and use. 
#'                If Null, the function will process the whole scoring data. Defaults to `NULL`.#'
#' @return Data frame with top n important features of each model score
#' @export
#'
#' @examples
ana_discover_top_features <- function(.scoring_path,
                                      .model_path,
                                  .clean_names,
                                  .top_feats = 250,
                                  .n_size = NULL) {
  scoring <- ana_preproc_scoring(.scoring = .scoring_path, .n_size = .n_size)
  cln <- vroom::vroom(.clean_names)
  # combine modeling and scoring data ----
  models <- rio::import(.model_path)
  model_cols <- dplyr::setdiff(colnames(models), "rnc_regid")
  score_cols <- dplyr::setdiff(colnames(scoring), "rnc_regid")
  df <- ana_combine_datasets(scoring, models, score_cols, model_cols)
  
  feat_imp <- ana_run_feat_imp(df, model_cols, score_cols)
  m_df <- ana_grab_top_n_features(feat_imp, model_cols, .top_feats) %>%
    dplyr::mutate(models = stringr::str_replace_all(models, "_", " "))
  ana_feat_cleaner (m_df, cln)
}



#' Clean the feature names of the feature importance table, need to be used within ana_discover_top_features
#'
#' @param .df Data frame, a feature importance table inherited during ana_discover_top_features
#' @param .clean Data frame,a dictionary of clean feature names 
#'
#' @return Data frame
#' @export
#'
#' @examples
ana_feat_cleaner <- function(.df, .clean) {
  .df %>%
    dplyr::mutate(var = key) %>%
    dplyr::left_join(.clean %>% mutate(var = tolower(var))) %>%
    dplyr::mutate(key = ifelse(is.na(clean_names), var, clean_names)) %>%
    dplyr::select(id, key, val, models, desc = description)
}




#' Export each model's feature importance plots to a local folder
#'
#' @param .df Data frame, a feature importance table inherited from the ana_discover_top_features function
#' @param .model_col The column that contains all the cleaned feature names 
#' @param .n_feats Number of features that you want to dispplay on the plot
#' @param .size Font size
#' @param .export_path File path to where you want to export the feature importance plots
#'
#' @return PNG picture of each model's top n important features, exported to the .export_path
#' @export
#'
#' @examples
ana_export_feature_importances <- function(.df,
                                       .model_col = "models",
                                       .n_feats = 10,
                                       .size = 38,
                                       .export_path = fs::path_wd()) {
  
  plot_s <- .df %>%
    dplyr::mutate(val = as.numeric(val)) %>%
    dplyr::mutate(key = stringr::str_replace_all(key,"_"," ")) %>%
    dplyr::group_split(!!rlang::sym(.model_col)) %>%
    purrr::map(~ ana_plot_feat_imp(
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

