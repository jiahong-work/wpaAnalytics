
ana_turnout_survey_adj <-
  function(.df,
           .turnout_col,
           .survey_col,
           .impute_groups = c("party", "newreg")) {

    cli::cli_alert_info("Mean TO Survey: {print_mean(collapse::get_elem(.df, .survey_col))}")
    cli::cli_alert_info("Mean TO Historical: {print_mean(collapse::get_elem(.df, .turnout_col))}")

    df <- impute_by_group(.df, .target = .turnout_col, .groups = .impute_groups)

    to_adj <- ana_adjust_turnout_by_survey(
      .turnout = collapse::get_elem(df, .turnout_col),
      .survey = collapse::get_elem(df, .survey_col)
    )
    cli::cli_alert("Mean TO: {print_mean(to_adj)}")
    cli::cli_alert("Sum TO: {print_sum(to_adj)}")
    cli::cli_alert_success("Survey and Historical Integrated")
    to_adj
  }


analytics_turnout <- function(.df,
                              .party_col = "RegisteredParty_RollUp",
                              .turnout_col = "Turnout_Score",
                              .survey_col = NULL,
                              .election_type = "Primary",
                              .new_regdate = "20161108",
                              .base_r_adj = -.2,
                              .base_i_adj = -.2,
                              .base_d_adj = -.2,
                              .us_up = 0.5,
                              .opp_down = -0.5,
                              .opp_up = 0.5,
                              .us_down = -0.5,
                              .hi_up = 0.5,
                              .lo_down = -0.5,
                              .fpath=fs::path_wd()) {
  df <- collapse::ftransform(.df,
                             newreg = (RegistrationDate > lubridate::as_date(.new_regdate)) * 1) %>%
    analytics_assign_party(.party_col = .party_col)
  # Load Turnout model

  if(!is.null(.survey_col)){
    cli::cli_alert_info("Performing turnout survey adjustment")
    to_adj <- ana_turnout_survey_adj(.df = df,
                                     .turnout_col = .turnout_col,
                                     .survey_col = .survey_col
                                     )
    turnout_df <- collapse::ftransform(df, Turnout = to_adj)
  } else {
    cli::cli_alert_info("No turnout survey adjustment")
    turnout_df <- dplyr::rename(df, Turnout = !!rlang::sym(.turnout_col))
    turnout_df <- turnout_df %>% impute_by_group(.target = "Turnout",
                                                 .groups = c(.party_col, "newreg"))
  }

  us_up <-
    apply_wizard_by_party(turnout_df, .us_up, .0, .opp_down, "Turnout_Us_up")
  cli::cli_alert_success("Calculated Us turnout scenario")
  opp_up <-
    apply_wizard_by_party(turnout_df, .us_down, .0, .opp_up, "Turnout_Opp_up")
  cli::cli_alert_success("Calculated Them turnout scenario")
  hi_lo <-
    collapse::fcompute(
      turnout_df,
      rnc_regid = rnc_regid,
      Turnout_High = wizard_formula(Turnout, .hi_up),
      Turnout_Low = wizard_formula(Turnout, .lo_down)
    )
  cli::cli_alert_success("Calculated Hi/Low turnout scenarios")
  df <- list(
    collapse::fselect(turnout_df,
                      rnc_regid,
                      Turnout) %>%
      purrr::set_names(c(
        "rnc_regid", paste0("Turnout", .election_type)
      )),
    us_up,
    opp_up,
    hi_lo
  ) %>%
    purrr::reduce(dplyr::left_join, by = "rnc_regid")
  cli::cli_alert_info("Joined turnout scenarios")
  cli::cli_alert_info("Exporting turnout scenario summaries")
  collapse::fsum(collapse::fselect(df, -rnc_regid)) %>%
    tibble::enframe(name = "scenario", value = "expected") %>%
    dplyr::left_join(
      collapse::fmean(collapse::fselect(df, -rnc_regid)) %>%
        tibble::enframe(name = "scenario", value = "percents"),
      by = "scenario"
    ) %>%
    round_all() %>%
    data.table::fwrite(glue::glue("{.fpath}/Turnout_Scenarios.csv"))
  cli::cli_alert_info("Exporting turnout scenarios")
  fst::write_fst(df, glue::glue("{.fpath}/turnout_scenarios.fst"))

  cli::cli_alert_info("Exporting turnout by party")
  df %>%
    dplyr::left_join(collapse::fselect(turnout_df, rnc_regid, party), by =
                       "rnc_regid") %>%
    collapse::fselect(-rnc_regid) %>%
    collapse::collap(by = ~ party,
                     FUN = list(mean = collapse::fmean, sum = collapse::fsum)) %>%
    round_all() %>%
    dplyr::select(party, starts_with("sum"), starts_with("mean")) %>%
    data.table::fwrite(glue::glue("{.fpath}/Turnout_By_Party.csv"))

  cli::cli_alert_success("Turnout Analytics Complete")
  df
}


analytics_assign_party <- function(.df, .party_col="Party"){
  .df %>%
    collapse::ftransform(
      party = data.table::fcase(
        stringfish::sf_grepl(collapse::get_elem(.df, .party_col), "D"),
        "D",
        stringfish::sf_grepl(collapse::get_elem(.df, .party_col), "R"),
        "R",
        default = "I"
      )
    )
}
analytics_pull_turnout <-
  function(.df,
           .party,
           .party_col = "party",
           .turnout_col = "Turnout",
           .summarize=FALSE){
    mask <- collapse::get_elem(.df, .party_col) %in% .party
    to <- collapse::fsubset(.df, mask) %>%
      collapse::get_elem(.turnout_col)
    if(.summarize){
      return(
        dplyr::tibble(
          mean=collapse::fmean(to),
          sum=collapse::fsum(to)
        )
      )
    }
    to
  }
analytics_to_scenario <- function(.df,
                                  .type = "Base",
                                  .turnout_col = "Turnout",
                                  .party_col = "party") {

  cols <- c(
    "Type",
    .party_col,
    paste0("mean.", .turnout_col),
    paste0("sum.", .turnout_col)
  )
  collapse::collap(
    .df,
    by = as.formula(paste("~", .party_col)),
    FUN = list(mean = collapse::fmean, sum = collapse::fsum),
    cols = .turnout_col
  ) %>%
    dplyr::mutate(Type=.type) %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    purrr::set_names(
      c("Type", "Party", "Mean", "Sum")
    )
}
apply_wizard_by_party <- function(.df, .r, .i, .d, .scene){
  reps <- collapse::fsubset(.df, party == "R") %>%
    collapse::ftransform(AdjTurnout = wizard_formula(Turnout, .r))
  dems <- collapse::fsubset(.df, party == "D") %>%
    collapse::ftransform(AdjTurnout = wizard_formula(Turnout, .d))
  inds <- collapse::fsubset(.df, party == "I") %>%
    collapse::ftransform(AdjTurnout = wizard_formula(Turnout, .i))
  list(reps, inds, dems) %>%
    dplyr::bind_rows() %>%
    collapse::fselect(rnc_regid, AdjTurnout) %>%
    purrr::set_names(c("rnc_regid", .scene))
}


round_all <- function(.df, .digits = 2) {
  dplyr::mutate(.df,
                dplyr::across(
                  .cols = where(is.numeric),
                  .fns = ~ round(.x, .digits)
                ))
}


um_calc_model_expectations <- function(.df, .models, .to_col) {
  df <- collapse::get_vars(.df, c(.models, .to_col))
  dplyr::summarise(df,
                   dplyr::across(
                     dplyr::all_of(.models),
                     list(
                       xxMean = ~um_mod_means(.x),
                       xxTotal = ~um_mod_sums(.x, !!rlang::sym(.to_col)),
                       xxExp = ~um_tow_means(.x, !!rlang::sym(.to_col))
                     )
                   )) %>%
    tidyr::gather(model, val) %>%
    tidyr::separate(model, c("Model","type"), sep = "_xx") %>%
    tidyr::spread(type,val) %>%
    dplyr::select(Model, Mean, Total, Exp)
}


print_mean <- purrr::compose(scales::percent, collapse::fmean)
print_sum <- purrr::compose(scales::comma, collapse::fsum)





ana_adjust_turnout_by_survey <- function(.turnout, .survey) {
  (.survey * .turnout) / (.survey * .turnout + (1 - (.survey * .turnout) ^
                                                  2) * (1 - .turnout))
}




ana_append_party <-
  function(.conx,
           .df,
           .survey_col,
           .filter_cols,
           .filter_conds,
           .sql_db = "bob",
           .sql_tbl = "commonvariables",
           .id_col = "rnc_regid",
           .extra_cols = c("RegisteredParty_RollUp",
                           "DTCalcParty",
                           "AgeRangeNew",
                           "Sex",
                           "RegistrationDate")) {
    party_plus <- pull_data(
      .conx,
      .sql_db,
      .sql_tbl,
      .filter_cols,
      .filter_conds,
      .subset_cols = c(
        .id_col,
        .extra_cols
      ),
      .collect = TRUE
    ) %>%
      dplyr::mutate(
        Party = dplyr::case_when(
          stringfish::sf_grepl(RegisteredParty_RollUp, "Rep") ~ "R",
          stringfish::sf_grepl(RegisteredParty_RollUp, "Dem") ~ "D",
          TRUE ~ "I"
        )
      ) %>%
      dplyr::select(-RegisteredParty_RollUp) %>%
      dplyr::rename(rnc_regid = !!rlang::sym(.id_col))

    party_plus %>%
      dplyr::left_join(.df %>% dplyr::select(dplyr::all_of(c(
        "rnc_regid", .survey_col
      ))))
  }

num_sum <-
  purrr::compose(tibble::enframe, collapse::fsum, collapse::num_vars)

ana_impute_raw_turnout <-
  function(.df,
           .new_reg = "2020-11-01",
           .reg_col = "RegistrationDate",
           .impute_groups = c("DTCalcParty", "NewReg"),
           .to_col = "Turnout_Score") {
    .df %>%
      dplyr::mutate(NewReg = as.numeric(!!rlang::sym(.reg_col) >= .new_reg)) %>%
      impute_by_group(.to_col, .impute_groups) %>%
      dplyr::select(-NewReg)
  }
