#' Set up the base 
#'
#' @param .path Working directory of the project
#' @param .project Project name
#' @param .candidate Our candidate's name
#' @param .seat The target seat of the election 
#' @param .geo Target geo 
#'
#' @return List
#' @export
#'
#' @examples
#' p_settings <- ana_project_settings(
# .path = p_path,
# .project = "OurAmerica_24G",
# .candidate = "Hobbs",
# .seat = "Gov",
# .geo = "AZ"
# )

ana_project_settings <- function(.path,
                                 .project,
                                 .candidate,
                                 .seat,
                                 .geo) {
  list(
    path = .path,
    project = .project,
    candidate = .candidate,
    seat = .seat,
    geo = .geo
  )
}


#' Create a ballot object that contains the election info of many races
#'
#' @param .cand Our candidate's ballot model name, like Ballot_Trump
#' @param .opp The opposing candidate's ballot model name, like Ballot_Biden
#' @param .race Election type, e.g. 'General', 'Primary'. Defaults to `Primary`
#'
#' @return List
#' @export
#'
ana_create_ballot_objects <- function(.cand, .opp, .race='Primary') {
  ballots <- list(cand = .cand,
                  opp = .opp,
                  race = .race)
  ballots
}


#' Get both candidates' ballot model names, as well as their unique prefixes for identification 
#'
#' @param .obj The ballot object you created through ana_create_ballot_objects
#' @param .race Election type, e.g. 'General', 'Primary'. Defaults to `Primary`
#'
#' @return List
#' @export

ana_get_ballot_cols <- function(.obj, .race='Primary') {
  idx <- which(.obj$race == .race)
  pre_us <- stringr::str_remove_all(.obj$cand[idx], "Ballot_")
  pre_them <- stringr::str_remove_all(.obj$opp[idx], "Ballot_")
  pre_us <-
    ifelse(pre_us == "R" & tolower(.race) == "Primary", "PrimaryR", pre_us)
  pre_them <-
    ifelse(pre_them == "D" & tolower(.race) == "Primary", "PrimaryD", pre_them)
  list(us = .obj$cand[idx],
       them = .obj$opp[idx],
       us_prefix = pre_us,
       them_prefix = pre_them,
       neutral_prefix = .race
  )
}

#' Calculate the net ballot score of a race
#'
#' @param .df Data frame containing the ballot scores of all parties in the election
#' @param .obj A ballot object created from the previous step ana_get_ballot_cols
#'
#' @return Data frame
#' @export
#'
ana_make_net_ballots <- function(.df, .obj) {
  net_ballots <- purrr::pmap_dfc(list(.obj$cand,
                                      .obj$opp,
                                      .obj$race),
                                 ~ {
                                   us <- collapse::get_elem(.df, ..1)
                                   them <-
                                     collapse::get_elem(.df, ..2)
                                   race <-
                                     rlang::sym(paste0("Net", ..3))
                                   dplyr::transmute(.df, !!race := us - them)
                                 })
  dplyr::bind_cols(.df, net_ballots)
}



#' Count expected turnout by universe
#'
#' @param .df Data frame that contains all the universe columns and the turnout score column
#' @param .universes Universe cols
#' @param .turnout Turnout col
#'
#' @return Data frame
#' @export

ana_cnt_turnout_by_universe <- function(.df, .universes, .turnout) {
  turnout <- .df %>%
    collapse::get_elem(.turnout) %>%
    collapse::replace_NA(0)
  (turnout * collapse::get_vars(.df, .universes)) %>%
    collapse::fsum() %>%
    tibble::enframe(name = "Universes", value = "Turnout")
}


#' Title
#'
#' @param .us Universe names of our candidate
#' @param .them Universe names of the opposing candidate
#' @param .group 
#'
#' @return List
#' @export

ana_create_universe_objects <- function(.us, .them, .group) {
  universes <- list(us = .us,
                    them = .them,
                    group = .group)
  universes
}

ana_make_net_universes <- function(.df, .obj) {
  net_ballots <- purrr::pmap_dfc(list(.obj$us,
                                      .obj$them,
                                      .obj$group),
                                 ~ {
                                   us <- collapse::get_elem(.df, ..1)
                                   them <-
                                     collapse::get_elem(.df, ..2)
                                   race <-
                                     rlang::sym(paste0("Net", ..3))
                                   dplyr::transmute(.df, !!race := us - them)
                                 })
  dplyr::bind_cols(.df, net_ballots)
}


#' Append the ballot object to the current project settings as a nested column,export the new project settings
#'
#' @param .settings The current project settings, from the ana_project_settings function
#' @param .ballot_obj A ballot_obj 
#'
#' @return Data frame of the new project settings
#' @export

ana_update_settings <- function(.settings, .ballot_obj) {
  nested <-
    dplyr::bind_cols(.settings, dplyr::as_tibble(.ballot_obj)) %>%
    tidyr::nest(ballots = c(cand, opp, race))
  qs::qsave(nested,
            fs::path(.settings$path, "Data", "project_settings.qs"))
  nested
}
