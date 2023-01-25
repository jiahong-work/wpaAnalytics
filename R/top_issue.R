normalize <- function(.x) {
  (.x - collapse::fmin(.x)) / (collapse::fmax(.x) - collapse::fmin(.x))
}

ana_pull_top_issues <- function(.conx,
                                .f_cols,
                                .f_conds,
                                .db = "bob",
                                .tbl = "commonvariables",
                                .score_tbl = "dsTopIssueScores") {
  top <- pull_data(
    conn = .conx,
    .db = .db,
    .sql_tbl = .tbl,
    .cols = .f_cols,
    .conds = .f_conds
  ) %>%
    dplyr::select(rnc_regid) %>%
    dplyr::left_join(get_tbl(wpai, .db = .db, .tbl = .score_tbl)) %>%
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), normalize)) %>%
    dplyr::select(-State)
  top$topissue <- ana_get_top_issue(top)
  top
}


ana_get_unique_targets <-
  function(.df,
           .top_ten,
           .universes,
           .issue_col = "topissue",
           .core_col = "core") {
    mask <- .df %>%
      # dplyr::filter(!!rlang::sym(.issue_col) %in% .top_ten) %>%
      collapse::get_vars(.universes) %>%
      collapse::qM() %>%
      matrixStats::rowSums2() %>%
      magrittr::is_greater_than(0)
    .df %>%
      #  dplyr::filter(!!rlang::sym(.issue_col) %in% .top_ten) %>%
      dplyr::filter(mask) %>%
      dplyr::count(!!rlang::sym(.core_col), name = "count")
  }

ana_get_top_issue <- function(.df) {
  only_nums <-
    collapse::qM(collapse::num_vars(.df %>% collapse::replace_NA(0)))
  colnames(only_nums)[max.col(only_nums, ties.method = "random")]
}
