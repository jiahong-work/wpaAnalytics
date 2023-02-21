#' Replace the prefix on core action universes
#'
#' @param .us Prefix to replace `R` in core action universes. For example, setting
#' `.us` to "Waters" replaces `R_ITB` and `R_GOTV` with `Waters_ITB` and `Waters_GOTV`.
#' @param .them Prefix to replace `D` in core action universes.
#'
#' @return Character vector
#' @export
#'
#' @examples wpa_replace_universe_prefix(.us = "Smith", .them = "Jones")
#'
ana_replace_universe_prefix <-
  function(.us = "R",
           .them = "D",
           .neutral = NULL) {
    tibble::tibble(
      prefix = c(rep(.us, 4), c("", ""), rep(.them, 4), c("", "")),
      type = c(
        "_ITB",
        "_GOTV",
        "_GOTV_T2",
        "_Shore_Up",
        ifelse(
          is.null(.neutral),
          "Persuasion",
          paste0(.neutral, "_", "Persuasion")
        ),
        ifelse(
          is.null(.neutral),
          "Persuasion_T2",
          paste0(.neutral, "_", "Persuasion_T2")
        ),
        "_Marginals",
        "_GOTV_T2",
        "_GOTV",
        "_ITB",
        ifelse(
          is.null(.neutral),
          "Disengaged",
          paste0(.neutral, "_", "Disengaged")
        ),
        ifelse(
          is.null(.neutral),
          "Dead_Weight",
          paste0(.neutral, "_", "Dead_Weight")
        )
      )
    ) |>
      dplyr::mutate(universe = glue::glue("{prefix}{type}")) |>
      dplyr::pull(universe) |>
      as.character()
  }

#' Create 12 core action universes from a net ballot and turnout column
#'
#' @param .df Data frame containing ballot and turnout columns.
#' @param .us Column containing ballot scores for our candidate.
#' @param .them Column containing ballot scores for opponent.
#' @param .turnout Column containing voter turnout scores.
#' @param .net_hi Cutoff for net ballot to assign voters in `_ITB` universes.
#' @param .net_lo Cutoff for net ballot to assign voters in `_Shore_Up` and `_Marginal`
#' universes.
#' @param .to_hi Cutoff for high turnout.
#' @param .to_midhi Cutoff for persuasion universe.
#' @param .to_midlo Cutoff splitting `GOTV` and `GOTV_2`
#' @param .to_lo Cutoff for low turnout. Values below this are assigned to `Dead_Weight`.
#' @param .prefixes Vector of two prefixes to replace `R_` and `D_` in the core action
#' universes.
#' @param .func Either `mutate` or `transmute`. If `mutate` returns all columns passed
#' in `.df` plus new core action universes. `transmute` returns only the new core
#' action universes
#'
#' @return Data frame with 12 core action universes appended.
#' @export
#'
ana_mk_12_universes <-
  function(.df,
           .us,
           .them,
           .turnout,
           .net_hi = .35,
           .net_lo = .2,
           .to_hi = .9,
           .to_midhi = .65,
           .to_midlo = .50,
           .to_lo = .15,
           .us_prefix = "R",
           .them_prefix = "D",
           .neutral_prefix = NULL,
           .func = "mutate") {
    all_universes <- c(
      "R_ITB",
      "R_GOTV",
      "R_GOTV_T2",
      "R_Shore_Up",
      "Persuasion",
      "Persuasion_T2",
      "D_Marginals",
      "D_GOTV_T2",
      "D_GOTV",
      "D_ITB",
      "Disengaged",
      "Dead_Weight"
    )
    
    func <- switch (.func,
                    "mutate" = collapse::ftransform,
                    "transmute" = collapse::fcompute)
    
    net <-
      collapse::get_elem(.df, .us) - collapse::get_elem(.df, .them)
    to <- collapse::get_elem(.df, .turnout)
    df <- func(
      .df,
      R_ITB = net > .net_hi & to >= .to_hi,
      R_GOTV = net > .net_hi & (to > .to_lo & to < .to_hi),
      R_GOTV_T2 = (net > .net_lo &
                     net <= .net_hi) &
        (to < .to_midlo & to > .to_lo),
      R_Shore_Up = (net > .net_lo &
                      net <= .net_hi) & (to >= .to_midlo),
      Persuasion = (abs(net) <= .net_lo) & to >= .to_midhi,
      Persuasion_T2 = (abs(net) <= .net_lo) &
        (to >= .to_midlo & to < .to_midhi),
      D_Marginals = (net < (.net_lo * -1) &
                       net >= (.net_hi * -1)) & to >= .to_midlo,
      D_GOTV_T2 = (net < (.net_lo * -1) &
                     net >= (.net_hi * -1)) &
        (to < .to_midlo & to > .to_lo),
      D_GOTV = net < (.net_hi * -1) & (to > .to_lo & to < .to_hi),
      D_ITB = net < (.net_hi * -1) & to >= .to_hi,
      Disengaged = abs(net) <= .net_lo &
        (to < .to_midlo & to > .to_lo),
      Dead_Weight = to <= .to_lo
    ) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(all_universes),
        as.numeric
      ))
    
    u_total <-
      collapse::fsum(matrixStats::rowSums2(collapse::qM(collapse::get_vars(df, all_universes))))
    assertthat::assert_that(u_total == collapse::fnrow(df),
                            msg = "Overlapping universes"
    )
    
    if(.func == "transmute") {
      df <- dplyr::bind_cols(dplyr::select(.df, rnc_regid), df)
    }
    
    # if (!is.null(.prefixes)) {
    #   assertthat::assert_that(length(.prefixes) == 2,
    #                           msg = "`.prefixes` must be a vector of length 2")
    new_universes <- wpa_replace_universe_prefix(
      .us_prefix,
      .them_prefix,
      .neutral_prefix
    )
    df <- data.table::setnames(df, old = all_universes, new = new_universes)
    return(df)
    #}
    #df
  }


#' Generate core action universe endpoint definitions
#'
#' @param .net_ballot Character name of column to use for net ballot. Defaults to `Net`
#' @param .turnout_col Character name of column to use for turnout column. Defaults to `Turnout`
#' @param .net_hi Cutoff for net ballot to assign voters in `_ITB` universes.
#' @param .net_lo Cutoff for net ballot to assign voters in `_Shore_Up` and `_Marginal`
#' universes.
#' @param .to_hi Cutoff for high turnout.
#' @param .to_midhi Cutoff for persuasion universe.
#' @param .to_midlo Cutoff splitting `GOTV` and `GOTV_2`
#' @param .to_lo Cutoff for low turnout. Values below this are assigned to `Dead_Weight`.
#' @param .us_prefix Prefix for universes favoring our candidate. Defaults to `R`
#' @param .them_prefix Prefix for universes favoring opponent. Defaults to `D`
#' @param .neutral_prefix Prefix for neutral columns like Persuasion and Disengaged. Defaults to `NULL`
#'
#' @return Data frame
#' @export
#'
#' @examples
#' wpa_def_12_universes()
#'
ana_def_12_universes <- function(.net_ballot = "Net",
                                 .turnout_col = "Turnout",
                                 .net_hi = .35,
                                 .net_lo = .2,
                                 .to_hi = .9,
                                 .to_midhi = .65,
                                 .to_midlo = .50,
                                 .to_lo = .15,
                                 .us_prefix = "R",
                                 .them_prefix = "D",
                                 .neutral_prefix = NULL) {
  tibble::tibble(
    .net_ballot = .net_ballot,
    .turnout_col = .turnout_col,
    .net_hi = .net_hi,
    .net_lo = .net_lo,
    .to_hi = .to_hi,
    .to_midhi = .to_midhi,
    .to_midlo = .to_midlo,
    .to_lo = .to_lo
  )
  defs <- list(
    glue::glue("{.net_ballot} > {.net_hi} & turnout >= {.to_hi}"),
    glue::glue(
      "{.net_ballot} > {.net_hi} & (turnout > {.to_lo} & turnout < {.to_hi})"
    ),
    glue::glue(
      "({.net_ballot} > {.net_lo} & {.net_ballot} <= {.net_hi}) & (turnout < {.to_midlo} & turnout > {.to_lo})"
    ),
    glue::glue(
      "({.net_ballot} > {.net_lo} & {.net_ballot} <= {.net_hi}) & (turnout > {.to_midlo})"
    ),
    glue::glue("(abs({.net_ballot}) <= {.net_lo}) & turnout >= {.to_midhi}"),
    glue::glue(
      "(abs({.net_ballot}) <= {.net_lo}) & (turnout >= {.to_midlo} & turnout < {.to_midhi})"
    ),
    glue::glue(
      "({.net_ballot} < ({.net_lo} * -1) & {.net_ballot} >= ({.net_hi} * -1)) & turnout >= {.to_midlo}"
    ),
    glue::glue(
      "({.net_ballot} < ({.net_lo} * -1) & {.net_ballot} >= ({.net_hi} * -1)) & (turnout < {.to_midlo} & turnout > {.to_lo})"
    ),
    glue::glue(
      "{.net_ballot} < ({.net_hi} * -1) & (turnout > {.to_lo} & turnout < {.to_hi})"
    ),
    glue::glue("{.net_ballot} < ({.net_hi} * -1) & turnout >= {.to_hi}"),
    glue::glue(
      "abs({.net_ballot}) <= {.net_lo} & (turnout < {.to_midlo} & turnout > {.to_lo})"
    ),
    glue::glue("turnout <= {.to_lo}")
  )
  dplyr::tibble(
    universe = wpa_replace_universe_prefix(
      .us = .us_prefix,
      .them = .them_prefix,
      .neutral = .neutral_prefix
    ),
    def = unlist(defs)
  )
}


#' Utility function to get core action universe counts and percentages
#'
#' @param .df Data frame containing the core action universes.
#' @param .universes Character vector of core action universe names, recommended to use along with 
#'                   wpa_replace_universe_prefix
#' .
#
#' @return Data frame
#' @export
#'
#' @examples wpa_grab_core_action_stats(.df=CLF_AZ_CD6_Scores_And_Flags,
#'                                      .universes=wpa_replace_universe_prefix('Ciscomani','Engel','Congressional'))
ana_grab_core_action_stats <- function(.df,
                                       .universes = wpa_replace_universe_prefix("R", "D")) {
  collapse::get_vars(.df, .universes) |>
    collapse::fsum() |>
    tibble::enframe("universe", "count") |>
    dplyr::mutate(pct = count / sum(count)) |>
    dplyr::mutate(rescaled_pct = count/sum(count[1:10])) |>
    dplyr::mutate(rescaled_pct = c(rescaled_pct[1:10],0,0))
}




#' Grab the core action universe counts and export them to the 'Reports' folder
#'
#' @param .df Data frame containing the ballot and turnout columns
#' @param .net_ballot Character name of column to use for net ballot. Defaults to `Net` 
#' @param .turnout_col Character name of column to use for turnout column. Defaults to `Turnout`
#' @param .net_hi Cutoff for net ballot to assign voters in `_ITB` universes.
#' @param .net_lo Cutoff for net ballot to assign voters in `_Shore_Up` and `_Marginal`
#' @param .to_hi Cutoff for high turnout.
#' @param .to_midhi Cutoff for persuasion universe.
#' @param .to_midlo Cutoff splitting `GOTV` and `GOTV_2`
#' @param .to_lo Cutoff for low turnout. Values below this are assigned to `Dead_Weight`.
#' @param .us_prefix Prefix for universes favoring our candidate. Defaults to `R`
#' @param .them_prefix Prefix for universes favoring opponent. Defaults to `D`
#' @param .neutral_prefix Prefix for neutral columns like Persuasion and Disengaged. Defaults to `NULL`
#' @param .path  The root working directory of your project, the parent folder of the 'Reports' folder
#' @param .suffix The suffix you want to append to the name of the core action count file, Defaults to `NULL`
#'
#' @return Data frame
#' @export
#'
#' @examples
#'  wpa_export_core_action_counts(.df=CLF_AZ_CD6_Scores_And_Flags,
#'                                .net_ballot='NetBallot_Generic',
#'                                .turnout_col='TUrnout',
#'                                .net_hi = .35,
#'                                .net_lo = .2,
#'                                .to_hi = .9,
#'                                .to_midhi = .65, 
#'                                .to_midlo = .50,
#'                                .to_lo = .15,
#'                                .us_prefix = "R", 
#'                                .them_prefix = "D",
#'                                .neutral_prefix = 'Generic',
#'                                .path = fs::path_wd(),
#'                                .suffix = 'Generic'))
ana_export_core_action_counts <- function(.df,
                                          .net_ballot = "Net",
                                          .turnout_col = "Turnout",
                                          .net_hi = .35,
                                          .net_lo = .2,
                                          .to_hi = .9,
                                          .to_midhi = .65,
                                          .to_midlo = .50,
                                          .to_lo = .15,
                                          .us_prefix = "R",
                                          .them_prefix = "D",
                                          .neutral_prefix = NULL,
                                          .path = fs::path_wd(),
                                          .suffix = NULL) {
  cau_defs <- wpa_def_12_universes(
    .net_ballot = .net_ballot,
    .turnout_col = .turnout_col,
    .net_hi = .net_hi,
    .net_lo = .net_lo,
    .to_hi = .to_hi,
    .to_midhi = .to_midhi,
    .to_midlo = .to_midlo,
    .to_lo = .to_lo,
    .us_prefix = .us_prefix,
    .them_prefix = .them_prefix,
    .neutral_prefix = .neutral_prefix
  )
  cau_counts <- wpa_grab_core_action_stats(.df = .df,
                                           .universes = wpa_replace_universe_prefix(.us = .us_prefix,
                                                                                    .them = .them_prefix,
                                                                                    .neutral = .neutral_prefix))
  tot_to <- collapse::fsum(collapse::get_elem(.df, .turnout_col))
  cau_to <-
    cnt_turnout_by_universe(
      .data = .df,
      .universes = wpa_replace_universe_prefix(.us = .us_prefix,
                                               .them = .them_prefix,
                                               .neutral = .neutral_prefix),
      .turnout = .turnout_col
    ) %>%
    dplyr::transmute(universe = Universes,
                     Turnout,
                     PctExpTurnout = Turnout / tot_to)
  
  cau_export <- cau_defs %>%
    dplyr::left_join(cau_counts) %>%
    dplyr::left_join(cau_to) %>%
    dplyr::select(
      Universe = universe,
      Count = count,
      Pct = pct,
      RescaledPct = rescaled_pct,
      ExpTurnout = Turnout,
      PctExpTurnout,
      UniverseEndPoints = def
    )
  
  file_name <- ifelse(
    is.null(.suffix),
    "core_action_counts.csv",
    paste0("core_action_counts_", tolower(.suffix), ".csv")
  )
  
  fs::dir_create(fs::path(.path, "Reports"))
  data.table::fwrite(cau_export,
                     fs::path(.path, "Reports", file_name))
  cau_export
}



#' Set definitions of the asymmetric core action universes
#'
#' @param .net_ballot Character name of column to use for net ballot. Defaults to `Net` 
#' @param .turnout_col Character name of column to use for turnout column. Defaults to `Turnout`
#' @param .us_itb Cutoff for net ballot to assign voters to our candidate's `_ITB` universes.
#' @param .us_gotv Cutoff for net ballot to assign voters to our candidate's `_GOTV` universes.
#' @param .them_gotv Cutoff for net ballot to assign voters to the opposing candidate's `_GOTV` universes.
#' @param .them_itb Cutoff for net ballot to assign voters to the opposing candidate's `_ITB` universes.
#' @param .us_to_hi Cutoff for high turnout.
#' @param .us_to_midhi Cutoff for persuasion universe.
#' @param .us_to_midlo Cutoff splitting `GOTV` and `GOTV_2`
#' @param .us_to_lo Cutoff for low turnout. Values below this are assigned to `Dead_Weight`.
#' @param .us_prefix Prefix for universes favoring our candidate. Defaults to `R`
#' @param .them_prefix Prefix for universes favoring opponent. Defaults to `D`
#' @param .neutral_prefix Prefix for neutral columns like Persuasion and Disengaged. Defaults to `NULL`
#'
#' @return Tibble object
#' @export
#'
#' @examples
#' wpa_def_asymmetrical_universes(
#' .net_ballot = "NetBallot_Generic",
#' .turnout_col = "Turnout",
#' .us_itb = .8,
#' .us_gotv = .6,
#' .them_gotv = .4,
#' .them_itb = .2,
#' .us_to_hi = .9,
#' .us_to_midhi = .7,
#' .us_to_midlo = .5,
#' .us_to_lo = .2,
#' .us_prefix = "GenericR",
#' .them_prefix = "GenericD",
#' .neutral_prefix = 'Generic')
#' 
ana_def_asymmetrical_universes <- function(.net_ballot = "Net",
                                           .turnout_col = "Turnout",
                                           .us_itb = .8,
                                           .us_gotv = .6,
                                           .them_gotv = .4,
                                           .them_itb = .2,
                                           .us_to_hi = .9,
                                           .us_to_midhi = .7,
                                           .us_to_midlo = .5,
                                           .us_to_lo = .2,
                                           .us_prefix = "R",
                                           .them_prefix = "D",
                                           .neutral_prefix = NULL) {
  cut_s <- mk_aysm_splits(
    .us_itb = .us_itb,
    .us_gotv = .us_gotv,
    .them_gotv = .them_gotv,
    .them_itb = .them_itb
  )
  
  defs <- list(
    glue::glue("{.net_ballot} > {cut_s$us_itb_lo} & turnout >= {.us_to_hi}"),
    glue::glue(
      "{.net_ballot} > {cut_s$us_itb_lo} & (turnout > {.us_to_lo} & turnout < {.us_to_hi})"
    ),
    glue::glue(
      "({.net_ballot} > {cut_s$us_gotv_lo} & {.net_ballot} <= {cut_s$us_itb_lo}) & (turnout < {.us_to_midlo} & turnout > {.us_to_lo})"
    ),
    glue::glue(
      "({.net_ballot} > {cut_s$us_gotv_lo} & {.net_ballot} <= {cut_s$us_itb_lo}) & (turnout > {.us_to_midlo})"
    ),
    
    glue::glue(
      "({.net_ballot} <= {cut_s$us_gotv_lo} & {.net_ballot} > {cut_s$them_gotv_hi}) & turnout >= {.us_to_midhi}"
    ),
    glue::glue(
      "({.net_ballot} <= {cut_s$us_gotv_lo} & {.net_ballot} > {cut_s$them_gotv_hi}) & (turnout >= {.us_to_midlo} & turnout < {.us_to_midhi})"
    ),
    glue::glue(
      "({.net_ballot} <= {cut_s$them_gotv_hi} & {.net_ballot} > {cut_s$them_itb_hi}) & turnout >= {.us_to_midlo}"
    ),
    glue::glue(
      "({.net_ballot} <= {cut_s$them_gotv_hi} & {.net_ballot} > {cut_s$them_itb_hi}) & (turnout < {.us_to_midlo} & turnout > {.us_to_lo})"
    ),
    glue::glue(
      "{.net_ballot} < {cut_s$them_itb_hi} & (turnout > {.us_to_lo} & turnout < {.us_to_hi})"
    ),
    glue::glue("{.net_ballot} < {cut_s$them_itb_hi} & turnout >= {.us_to_hi}"),
    glue::glue(
      "({.net_ballot} <= {cut_s$us_gotv_lo} & {.net_ballot} > {cut_s$them_gotv_hi}) & (turnout < {.us_to_midlo} & turnout > {.us_to_lo})"
    ),
    glue::glue("turnout <= {.us_to_lo}")
  )
  dplyr::tibble(
    universe = wpa_replace_universe_prefix(
      .us = .us_prefix,
      .them = .them_prefix,
      .neutral = .neutral_prefix
    ),
    def = unlist(defs)
  )
  
}



#' Title
#'
#' @param .us_itb 
#' @param .us_gotv 
#' @param .them_gotv 
#' @param .them_itb 
#'
#' @return
#' @export
#'
#' @examples
ana_mk_aysm_splits(
.us_itb = .8,
.us_gotv = .6,
.them_gotv = .4,
.them_itb = .2)
mk_aysm_splits <- function(.us_itb,
                           .us_gotv,
                           .them_gotv,
                           .them_itb) {
  us_itb <- stats::quantile(seq(1, -1, -.01), .us_itb)
  us_gotv <- stats::quantile(seq(1, -1, -.01), .us_gotv)
  them_gotv <- stats::quantile(seq(1, -1, -.01), .them_gotv)
  them_itb <- stats::quantile(seq(1, -1, -.01), .them_itb)
  list(
    us_itb_lo = us_itb,
    us_gotv_hi = us_itb,
    us_gotv_lo = us_gotv,
    them_gotv_hi = them_gotv,
    them_gotv_lo = them_itb,
    them_itb_hi = them_itb
    
  )
}




#' Create asymmetric core action universes
#'
#' @param .df Data frame containing the ballot and turnout columns
#' @param .us_ballot The ballot score for our candidate
#' @param .them_ballot The ballot score for the opposing candidate
#' @param .turnout_col Character name of column to use for turnout column. Defaults to `Turnout`
#' @param .us_itb Cutoff for net ballot to assign voters to our candidate's `_ITB` universes.
#' @param .us_gotv Cutoff for net ballot to assign voters to our candidate's `_GOTV` universes.
#' @param .them_gotv Cutoff for net ballot to assign voters to the opposing candidate's `_GOTV` universes.
#' @param .them_itb Cutoff for net ballot to assign voters to the opposing candidate's `_ITB` universes.
#' @param .us_to_hi Cutoff for high turnout.
#' @param .us_to_midhi Cutoff for persuasion universe.
#' @param .us_to_midlo Cutoff splitting `GOTV` and `GOTV_2`
#' @param .us_to_lo Cutoff for low turnout. Values below this are assigned to `Dead_Weight`.
#' @param .us_prefix Prefix for universes favoring our candidate. Defaults to `R`
#' @param .them_prefix Prefix for universes favoring opponent. Defaults to `D`
#' @param .neutral_prefix Prefix for neutral columns like Persuasion and Disengaged. Defaults to `NULL`
#' @param .func The function you want to use to mutate the asymmetric core action universes, 
#'              could be 'mutate' if you want to append the universe cols to the original data frame,
#'              or 'transmute' if you want to generate a nmew table with only the new universe cols and rnc_regid. 
#'              Defaults to `mutate`
#'
#' @return Data frame
#' @export
#'
#' @examples
ana_mk_asymmetrical_universes <- function(.df,
                                          .us_ballot,
                                          .them_ballot,
                                          .turnout_col,
                                          .us_itb,
                                          .us_gotv,
                                          .them_gotv,
                                          .them_itb,
                                          .us_to_hi,
                                          .us_to_midhi,
                                          .us_to_midlo,
                                          .us_to_lo,
                                          .them_to_hi = NULL,
                                          .them_to_midhi = NULL,
                                          .them_to_midlo = NULL,
                                          .them_to_lo = NULL,
                                          .us_prefix = "R",
                                          .them_prefix = "D",
                                          .neutral_prefix = NULL,
                                          .func = "mutate") {
  .them_to_hi <- .them_to_hi %||% .us_to_hi
  .them_to_midhi <- .them_to_midhi %||% .us_to_midhi
  .them_to_midlo <- .them_to_midlo %||% .us_to_midlo
  .them_to_lo <- .them_to_lo %||% .us_to_lo
  all_universes <- c(
    "R_ITB",
    "R_GOTV",
    "R_GOTV_T2",
    "R_Shore_Up",
    "Persuasion",
    "Persuasion_T2",
    "D_Marginals",
    "D_GOTV_T2",
    "D_GOTV",
    "D_ITB",
    "Disengaged",
    "Dead_Weight"
  )
  func <- switch (.func,
                  "mutate" = collapse::ftransform,
                  "transmute" = collapse::fcompute)
  
  cut_s <- mk_aysm_splits(
    .us_itb = .us_itb,
    .us_gotv = .us_gotv,
    .them_gotv = .them_gotv,
    .them_itb = .them_itb
  )
  net <-
    collapse::get_elem(.df, .us) - collapse::get_elem(.df, .them)
  turnout <- collapse::get_elem(.df, .turnout)
  df <- func(
    .df,
    R_ITB = net > cut_s$us_itb_lo & turnout >= .us_to_hi,
    R_GOTV = net > cut_s$us_itb_lo &
      (turnout > .us_to_lo & turnout < .us_to_hi),
    R_GOTV_T2 = (net <= cut_s$us_itb_lo &
                   net > cut_s$us_gotv_lo) &
      (turnout < .us_to_midlo & turnout > .us_to_lo),
    R_Shore_Up = (net <= cut_s$us_itb_lo &
                    net > cut_s$us_gotv_lo) &
      (turnout > .us_to_midlo),
    Persuasion = (net <= cut_s$us_gotv_lo &
                    net > cut_s$them_gotv_hi) &
      (turnout > .us_to_midhi),
    Persuasion_T2 = (net <= cut_s$us_gotv_lo &
                       net > cut_s$them_gotv_hi) &
      (turnout < .us_to_midhi & turnout >= .us_to_midlo),
    D_Marginals = (net <= cut_s$them_gotv_hi &
                     net > cut_s$them_gotv_lo) &
      turnout >= .them_to_midlo,
    D_GOTV_T2 = (net <= cut_s$them_gotv_hi &
                   net > cut_s$them_gotv_lo) &
      (turnout < .them_to_midlo & turnout >= .them_to_lo),
    D_GOTV = net < cut_s$them_itb_hi &
      (turnout > .them_to_lo & turnout <= .them_to_hi),
    D_ITB = net < cut_s$them_itb_hi & (turnout >= .them_to_hi),
    Disengaged = (net <= cut_s$us_gotv_lo & net > cut_s$them_gotv_hi) & (turnout < .them_to_midlo & turnout > .them_to_lo),
    Dead_Weight = turnout <= .them_to_lo
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_universes),
                                as.numeric))
  
  # u_total <-
  #   collapse::fsum(matrixStats::rowSums2(collapse::qM(collapse::get_vars(df, all_universes))))
  # assertthat::assert_that(u_total == collapse::fnrow(df),
  #                         msg = "Overlapping universes")
  if (.func == "transmute") {
    df <- dplyr::bind_cols(dplyr::select(.df, rnc_regid), df)
  }
  
  # if (!is.null(.prefixes)) {
  #   assertthat::assert_that(length(.prefixes) == 2,
  #                           msg = "`.prefixes` must be a vector of length 2")
  new_universes <- wpa_replace_universe_prefix(.us_prefix,
                                               .them_prefix,
                                               .neutral_prefix)
  df <-
    data.table::setnames(df, old = all_universes, new = new_universes)
  return(df)
  #}
  #df
}


#' Export asymmetric core action universe counts
#'
#' @param .df Data frame containing the ballot and turnout columns
#' @param .net_ballot Character name of column to use for net ballot. Defaults to `Net` 
#' @param .turnout_col Character name of column to use for turnout column. Defaults to `Turnout`
#' @param .us_itb Cutoff for net ballot to assign voters to our candidate's `_ITB` universes.
#' @param .us_gotv Cutoff for net ballot to assign voters to our candidate's `_GOTV` universes.
#' @param .them_gotv Cutoff for net ballot to assign voters to the opposing candidate's `_GOTV` universes.
#' @param .them_itb Cutoff for net ballot to assign voters to the opposing candidate's `_ITB` universes.
#' @param .us_to_hi Cutoff for high turnout.
#' @param .us_to_midhi Cutoff for persuasion universe.
#' @param .us_to_midlo Cutoff splitting `GOTV` and `GOTV_2`
#' @param .us_to_lo Cutoff for low turnout. Values below this are assigned to `Dead_Weight`.
#' @param .us_prefix Prefix for universes favoring our candidate. Defaults to `R`
#' @param .them_prefix Prefix for universes favoring opponent. Defaults to `D`
#' @param .neutral_prefix Prefix for neutral columns like Persuasion and Disengaged. Defaults to `NULL`
#' @param .path  The root working directory of your project, the parent folder of the 'Reports' folder
#'
#' @return Data frame
#' @export
#'
#' @examples
ana_export_asymmetrical_counts <- function(.df,
                                           .net_ballot = "Net",
                                           .turnout_col = "Turnout",
                                           .us_itb = .8,
                                           .us_gotv = .6,
                                           .them_gotv = .4,
                                           .them_itb = .2,
                                           .us_to_hi = .8,
                                           .us_to_lo = .2,
                                           .us_to_midhi = .6,
                                           .us_to_midlo = .4,
                                           .us_prefix = "R",
                                           .them_prefix = "D",
                                           .neutral_prefix = NULL,
                                           .path = fs::path_wd()) {
  cau_defs <- wpa_def_asymmetrical_universes(
    .net_ballot = .net_ballot,
    .turnout_col = .turnout_col,
    .us_itb = .us_itb,
    .us_gotv = .us_gotv,
    .them_gotv = .them_gotv,
    .them_itb = .them_itb,
    .us_to_lo = .us_to_lo,
    .us_to_hi = .us_to_hi,
    .us_to_midhi = .us_to_midhi,
    .us_to_midlo = .us_to_midlo,
    .us_prefix = .us_prefix,
    .them_prefix = .them_prefix,
    .neutral_prefix = .neutral_prefix
  )
  cau_counts <- wpa_grab_core_action_stats(.df = .df,
                                           .universes = wpa_replace_universe_prefix(.us = .us_prefix,
                                                                                    .them = .them_prefix,
                                                                                    .neutral = .neutral_prefix))
  tot_to <- collapse::fsum(collapse::get_elem(.df, .turnout_col))
  cau_to <-
    cnt_turnout_by_universe(
      .data = .df,
      .universes = wpa_replace_universe_prefix(.us = .us_prefix,
                                               .them = .them_prefix,
                                               .neutral = .neutral_prefix),
      .turnout = .turnout_col
    ) %>%
    dplyr::transmute(universe = Universes,
                     Turnout,
                     PctExpTurnout = Turnout / tot_to)
  
  cau_export <- cau_defs %>%
    dplyr::left_join(cau_counts) %>%
    dplyr::left_join(cau_to) %>%
    dplyr::select(
      Universe = universe,
      Count = count,
      Pct = pct,
      RescaledPct = rescaled_pct,
      ExpTurnout = Turnout,
      PctExpTurnout,
      UniverseEndPoints = def
    )
  fs::dir_create(fs::path(.path, "Reports"))
  data.table::fwrite(cau_export,
                     fs::path(.path, "Reports", "CoreActionCounts.csv"))
  cau_export
}



#' #' Title
#' #'
#' #' @param .ballot_obj An object that contains  a list of different elections(races)
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' ana_group_core_action_cols <- function(.ballot_obj){
#'   obj <- purrr::map(.ballot_obj$race,
#'                     ~ wpa_replace_universe_prefix(
#'                       .us = ana_get_ballot_cols(.ballot_obj, .x)$us_prefix,
#'                       .them = ana_get_ballot_cols(.ballot_obj,.x)$them_prefix,
#'                       .neutral = ana_get_ballot_cols(.ballot_obj, .x)$neutral_prefix
#'                     )
#'   )
#'   names(obj) <- .ballot_obj$race
#'   obj
#' }
