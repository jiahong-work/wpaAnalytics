ana_create_ballot_objects <- function(.cand, .opp, .race) {
  ballots <- list(cand = .cand,
                  opp = .opp,
                  race = .race)
  ballots
}
ana_get_ballot_cols <- function(.obj, .race) {
  idx <- which(.obj$race == .race)
  pre_us <- stringr::str_remove_all(.obj$cand[idx], "Ballot_")
  pre_them <- stringr::str_remove_all(.obj$opp[idx], "Ballot_")
  pre_us <-
    ifelse(pre_us == "R" & tolower(.race) == "gen", "Gen_R", pre_us)
  pre_them <-
    ifelse(pre_them == "D" & tolower(.race) == "gen", "Gen_D", pre_them)
  list(us = .obj$cand[idx],
       them = .obj$opp[idx],
       us_prefix = pre_us,
       them_prefix = pre_them,
       neutral_prefix = .race
       )
}

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



cnt_turnout_by_universe <- function(.data, .universes, .turnout) {
  turnout <- .data %>%
    collapse::get_elem(.turnout) %>%
    collapse::replace_NA(0)
  (turnout * collapse::get_vars(.data, .universes)) %>%
    collapse::fsum() %>%
    tibble::enframe(name = "Universes", value = "Turnout")
}

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
ana_update_settings <- function(.settings, .ballot_obj) {
  nested <-
    dplyr::bind_cols(.settings, dplyr::as_tibble(.ballot_obj)) %>%
    tidyr::nest(ballots = c(cand, opp, race))
  qs::qsave(nested,
            fs::path(.settings$path, "Data", "project_settings.qs"))
  nested
}
