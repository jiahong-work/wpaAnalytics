ana_init_defs <- function() {
  tibble::tibble(
    tibble::tibble(
      name = character(),
      define = character(),
      describe = character(),
      group = character()
    )
  )
}

ana_define_universe <-
  function(.name,
           .define,
           .describe,
           .group,
           .init = FALSE) {
    if (.init) {
      u_tbl <- ana_init_defs()
    }
    universe <- tibble::tibble(
      name = .name,
      define = .define,
      describe = .describe,
      group = .group
    )
    u_tbl <<- dplyr::add_row(u_tbl, universe)
  }

#' Build universes from model scores
#'
#' @param .data data frame of model scores
#' @param .names vector of column names
#' @param .conditions vector of universe definitions
#'
#' @return
#' @export
#'
ana_build_universes <- function(.df, .names, .definitions) {
  purrr::map2(.names, .definitions, ~ {
    .df |> dplyr::transmute(
      rnc_regid,
      !!rlang::sym(.x) := dplyr::if_else(!!rlang::parse_expr(.y), 1, 0)
    )
  }) |>
    purrr::reduce(dplyr::left_join, by = "rnc_regid")
}





ana_define_favorable <- function(.candidate,
                                 .fav_col,
                                 .unfav_col,
                                 .dnk_col,
                                 .cutoff = 0,
                                 .dnk_cutoff = .3,
                                 .group = "image",
                                 .init = FALSE) {
  ana_define_universe(
    .name = glue::glue("{.candidate}_Favorable_Flag"),
    .define = glue::glue(
      "({.fav_col} - {.unfav_col} > {.cutoff}) & {.dnk_col} < {.dnk_cutoff}"
    ),
    .describe = glue::glue("Voters with a favorable view of {.candidate}"),
    .group = .group,
    .init = .init
  )
  ana_define_universe(
    .name = glue::glue("{.candidate}_Unfavorable_Flag"),
    .define = glue::glue(
      "({.unfav_col} - {.fav_col} > {.cutoff}) & {.dnk_col} < {.dnk_cutoff}"
    ),
    .describe = glue::glue("Voters with a unfavorable view of {.candidate}"),
    .group = .group,
    .init = FALSE
  )
  ana_define_universe(
    .name = glue::glue("{.candidate}_NHDK_Flag"),
    .define = glue::glue("{.dnk_col} >= {.dnk_cutoff}"),
    .describe = glue::glue("Voters that have no opinion or do not know {.candidate}"),
    .group = .group,
    .init = FALSE
  )
}

ana_define_approval <- function(.candidate,
                                .approve_col = NULL,
                                .disapprove_col = NULL,
                                .cutoff = 0,
                                .group = "image",
                                .init = FALSE) {

  approve_col <- .approve_col %||% paste0(.candidate, "_", "Approve")
  disapprove_col <- .disapprove_col %||% paste0(.candidate, "_", "Disapprove")

  ana_define_universe(
    .name = glue::glue("{.candidate}_Approve_Flag"),
    .define = glue::glue("{approve_col} - {disapprove_col} > {.cutoff}"),
    .describe = glue::glue("Voters that approve of {.candidate}'s performance."),
    .group = .group,
    .init = .init
  )
  ana_define_universe(
    .name = glue::glue("{.candidate}_Disapprove_Flag"),
    .define = glue::glue("{disapprove_col} - {approve_col} > {.cutoff}"),
    .describe = glue::glue("Voters that disapprove of {.candidate}'s performance."),
    .group = .group,
    .init = FALSE
  )
}


ana_define_party <-
  function(.r_col = "Party_R",
           .d_col = "Party_D",
           .i_col = "Party_IO") {
    # Define Reps
    ana_define_universe(
      .name = "RParty_Flag",
      .define = glue::glue("{.r_col} > {.d_col} & {.r_col} > {.i_col}"),
      .describe = "Voters who identify as Republicans",
      .group = "Party Affiliation",
      .init = FALSE
    )

    # Define Dems
    ana_define_universe(
      .name = "DParty_Flag",
      .define = glue::glue("{.d_col} > {.r_col} & {.d_col} > {.i_col}"),
      .describe = "Voters who identify as Democrats",
      .group = "Party Affiliation",
      .init = FALSE
    )
    # Define IO
    ana_define_universe(
      .name = "IParty_Flag",
      .define = glue::glue("{.i_col} > {.r_col} & {.i_col} > {.d_col}"),
      .describe = "Voters who identify as Independents",
      .group = "Party Affiliation",
      .init = FALSE
    )

  }

