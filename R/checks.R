val_names <- function(.dataframe, .cols) {
  assertthat::assert_that(all(.cols %in% colnames(.dataframe)),
                          msg = ".cols not in .dataframe")
}
val_univ_numeric <- function(.dataframe, .universes){
  assertthat::assert_that(
    collapse::fncol(collapse::num_vars(collapse::get_vars(.dataframe, .universes))) ==
      collapse::fncol(collapse::get_vars(.dataframe, .universes)),
    msg = "Not all universe columns are numeric"
  )
}

msg_missing_cols <- function(.df, .cols) {
  if (!all(.cols %in% colnames(.df))) {
    cli::cli_abort(c("One or more of {.var {.cols}} not in data frame."))
  }
}

ana_report_core_action_turnout <-
  function(.df,
           .us_prefix = "R",
           .them_prefix = "D",
           .neutral_prefix = NULL,
           .to_col = "Turnout_Score") {
    cau <- wpa_grab_core_action_stats(.df,
                                      .universes = wpa_replace_universe_prefix(
                                        .us = .us_prefix,
                                        .them = .them_prefix,
                                        .neutral = .neutral_prefix
                                      ))

    to_counts <- cau %>%
      dplyr::slice(1:10) %>%
      dplyr::pull(count) %>%
      collapse::fsum()

    est_to <- collapse::get_elem(.df, .to_col) %>% collapse::fsum()
    cli::cli_h2("Core Action Universe Counts")
    print(cau %>% knitr::kable(format = "simple"))
    cli::cli_rule()
    cli::cli_h2("Turnout Stats")
    cli::cli_rule()
    cli::cli_alert_info("Core Action Universe Turnout: {scales::comma(to_counts,accuracy=1)}")
    cli::cli_alert_info("Expected Turnout: {scales::comma(est_to, accuracy=1)}")
    cli::cli_alert_info("Net Difference: {scales::comma(to_counts - est_to, accuracy = 1)}")
    cli::cli_alert_info("% Off Expected Turnout: {scales::percent((to_counts - est_to)/est_to, accuracy = 1)}")
  }

