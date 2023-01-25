# to delete
wpa_get_all_demos <- function(.conx,
                              .f_cols,
                              .f_conds,
                              .new_reg = "2020-11-01") {
  pull_data(conn = .conx,
            .db = "bob",
            .sql_tbl = "commonvariables",
            .cols = .f_cols,
            .conds = .f_conds,
            .subset_cols = c(
              "rnc_regid",
              "State",
              "CountyName",
              "DMA",
              "PrecinctName",
              "PrecinctNumber",
              "CongressionalDistrict_NextElection",
              "SenateDistrict_NextElection",
              "LegislativeDistrict_NextElection",
              "RegistrationAddressZip5",
              "RegistrationAddress1",
              "RegistrationAddressCity",
              "CensusBlock",
              "HouseHoldID",
              "HouseHoldID_Custom",
              "AgeRangeNew",
              "Sex",
              "Race",
              "RegisteredParty",
              "RegisteredParty_RollUp",
              "DTCalcParty",
              "RegistrationDate",
              "LandLine_ReliabilityCode",
              "LandLine",
              "CellPhone_ReliabilityCode",
              "CellPhone"
            ),
            .collect = TRUE
  ) |>
    wpa_mk_precinct()
}


# From the utils-data.R

#' Utility function to filter data frame for single universe
#'
#' @param .data Data frame containing universe columns
#' @param .universe Column name of universe to use as filter
#' @param .target Value to filter universe for. Defaults to `1`
#'
#' @return Data frame
#' @export
#'
#' @examples ana_filter_universe(mtcars, "vs")
#'
ana_filter_universe <- function(.data, .universe, .target = 1) {
  val_names(.data, .universe)
  val_univ_numeric(.data, .universe)
  collapse::fsubset(.data,
                    collapse::get_elem(.data, .universe) == .target)
}




# from the turnout.R
ana_weight_two_models <- function(.turnout, .survey, .wght) {
  (.survey * .wght) + (.turnout * (1 - .wght))
}



ana_report_turnout_survey <-
  function(.data,
           .turnout_col = "Turnout_Score",
           .survey_col = "TurnoutSurvey") {
    to <- collapse::get_elem(.data, .turnout_col)
    survey <- collapse::get_elem(.data, .survey_col)
    cli::cli_h1("Turnout Model and Survey")
    cli::cli_alert_info("Mean TO Survey: {.path {round(collapse::fmean(survey),3)}}")
    cli::cli_alert_info("Mean TO Historical: {.path {round(collapse::fmean(to),3)}}")
    to_gen <- ana_adjust_turnout_by_survey(to, survey)
    cli::cli_rule(left = "Survey & Historical Integrated Turnout")
    cli::cli_alert_info("Mean TO General: {.path {round(collapse::fmean(to_gen),3)}}")
    cli::cli_alert_info("Sum  TO General: {.path {scales::comma(collapse::fsum(to_gen),accuracy=1)}}")
  }


ana_wizard_split_setup <- function(.split_col,
                                   .split_values,
                                   .split_up,
                                   .split_down) {
  dplyr::tibble(!!.split_col := .split_values,
                us_up = .split_up,
                us_down = .split_down)
}



# from setup.R
ana_make_settings_object <- function(.candidates,
                                     .opponents,
                                     .races,
                                     .settings_path = NULL) {
  if (!is.null(.settings_path)) {
    ps <- rio::import(.settings_path)
  } else {
    ps <- rio::import(fs::path(fs::path_wd(), "Data", "project_settings.fst"))
  }

  ps %>%
    dplyr::mutate(candidates = .candidates,
                  opponents = .opponents,
                  races = .races) %>%
    tidyr::nest(ballots = c(candidates, opponents, races))
}



# from define.R
ana_setup <-
  function(.path,
           .universes,
           .turnout_col,
           .us_ballot,
           .them_ballot) {
    list(
      path = .path,
      universes = .universes,
      turnout_col = .turnout_col,
      us_ballot = .us_ballot,
      them_ballot = .them_ballot
    )
  }



ana_desc_favorable <- function(.candidate, .unknown = FALSE) {
  defs <- c(
    glue::glue("People with a favorable opinion of {.candidate}"),
    glue::glue("People with a unfavorable opinion of {.candidate}")
  )
  if (.unknown) {
    return(c(
      defs,
      glue::glue(
        "People that haven't heard of, or have no opinion of, {.candidate}"
      )
    ))
  }
  defs
}


ana_desc_approve <- function(.candidate){
  defs <- c(
    glue::glue("People that approve of the job {.candidate} is doing"),
    glue::glue("People that disapprove of the job {.candidate} is doing")
  )

  defs
}


ana_desc_core_action <- function(.us="Republican",
                                 .them="Democrat"){
  c(glue::glue("{.us} In the Bank targets"),
    glue::glue("{.us} Mobilization targets"),
    glue::glue("{.us} Mobilization targets, 2nd tier"),
    glue::glue("Marginal {.us} voters in need of shoring up"),
    glue::glue("Persuasion targets"),
    glue::glue("Persuasion targets, 2nd tier"),
    glue::glue("Marginal {.them} voters"),
    glue::glue("{.them} Mobilization targets, 2nd tier"),
    glue::glue("{.them} Mobilization targets"),
    glue::glue("{.them} In the Bank targets"),
    glue::glue("People who are very unlikely to vote"),
    glue::glue("People without strong political opinions of vote likelihood"))
}


# from list_columns.R
ana_grab_model_col_names <-
  function(.df,
           .id_col = "rnc_regid",
           .prefix_us = "R",
           .prefix_them = "D",
           .prefix_neutral = NULL,
           .extra_universes = NULL,
           .extra_demos = NULL,
           .drop_turnout = TRUE) {
    all_cols <- colnames(.df)
    demos <- ana_list_all_demos(.extra_demos)
    universes <- ana_list_all_universes(
      .df = .df,
      .prefix_us = .prefix_us,
      .prefix_them = .prefix_them,
      .prefix_neutral = .prefix_neutral,
      .extra = .extra_universes
    )

    models <- setdiff(all_cols,
                      c(.id_col, demos, universes))

    if (isTRUE(.drop_turnout)) {
      return(purrr::discard(models, ~ stringr::str_detect(.x, "Turnout")))
    }

    models

  }
