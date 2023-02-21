#' Check if a list of cols exist in a data frame
#'
#' @param .df Data frame that will be evaluated 
#' @param .cols Column names that shall be tested whether they are in the data frame
#'
#' @return Logical. Return TRUE if the col exists, return an error message if not
#' @export
#'
#' @examples 
#' val_names(df,'lo')
#' 
ana_val_names <- function(.df, .cols) {
  assertthat::assert_that(all(.cols %in% colnames(.df)),
                          msg = ".cols not in .df")
}


#' Check if a list of cols exist in a data frame
#'
#' @param .df Data frame that will be evaluated 
#' @param .cols Column names that shall be tested whether they are in the data frame
#'
#' @return Logical. Return TRUE if the col exists, return an error message if not
#' @export
#'
ana_msg_missing_cols <- function(.df, .cols) {
  if (!all(.cols %in% colnames(.df))) {
    cli::cli_abort(c("One or more of {.var {.cols}} not in data frame."))
  }
}



#' Check if all tested columns in the data frame are numeric
#'
#' @param .df Data frame that will be evaluated 
#' @param .cols Column names that shall be tested whether they are numeric or not
#'
#' @return Logical. Return TRUE if all columns are numeric, return an error message if not
#' @export
#'
#' @examples 
#' val_names(df,'CongressionalDistricts_NextELection')
#' 
ana_val_univ_numeric <- function(.df, .cols){
  assertthat::assert_that(
    collapse::fncol(collapse::num_vars(collapse::get_vars(.df, .cols))) ==
      collapse::fncol(collapse::get_vars(.df, .cols)),
    msg = "Not all universe columns are numeric"
  )
}




#' 
#'
#' @param .df data frame that will be operated on
#' @param .us_prefix prefixes that are unique to our candidates
#' @param .them_prefix prefixes that are unique to the opposing candidates
#' @param .neutral_prefix prefixes that are unique to the neutral audience
#' @param .to_col column name of the turnout score
#'
#' @return 
#' a table in console displaying the sum and percent of each core universe, 
#' plus a comparison of voters that are covered vs voters that are actually expected to turnout
#' 
#' @export
#'
#' @examples 
#' ana_report_core_action_turnout(CLF_AZ_CD6_Scores_And_Flags,'Ciscomani','Engel','Congressional','Turnout')
#' 
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
