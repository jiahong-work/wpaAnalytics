#' Utility function to get universe counts by demos
#'
#' @param .df Data frame containing `.demos` and `.universes`
#' @param .demos Demo columns to group universe counts by
#' @param .universes Universe columns to sum 
#'
#' @return Data frame
#' @export
#'
ana_get_demo_counts <- function(.df, .demos, .universes) {
  msg_missing_cols(.df, c(.demos, .universes))
  purrr::map_dfr(
    .demos,
    ~ ana_cnt_universes_by_filter(.df, .x, .universes) |>
      
      dplyr::mutate(Demo = .x, .before = 1) |>
      purrr::set_names(c("Demo", "Levels", .universes))
  )
}


#' Calculate universe counts by demo
#'
#' @param .df Data frame containing demos and universes
#' @param .by Atomic vector of demo to group by
#' @param .universes Universes to sum by group
#'
#' @return Data frame
#' @export
#'
#' @examples
#' ana_cnt_universes_by_filter(demo_data, .by = "race", .universes = c("Msg_CRT_Tight"))
#'
ana_cnt_universes_by_filter <-
  function(.df, .by, .universes) {
    collapse::get_vars(.df, .universes) |>
      collapse::fsum(g = collapse::GRP(collapse::get_elem(.df, .by)),
                     use.g.names = TRUE) |>
      tibble::rownames_to_column("Universe")
  }

#' Calculate expected turnout by demo
#'
#' @param .df Data frame containing `.demos` and `.turnout` column
#' @param .demos Vector of column names containing demos
#' @param .turnout Name of turnout column in `.df`
#'
#' @return Data frame
#' @export
#'
ana_calc_turnout_by_demo <-
  function(.df, .demos, .turnout = "TurnoutGeneral") {
    purrr::map_dfr(.demos,
                   ~ {
                     cli::cli_alert_info("Calculating turnout for {(.x)}")
                     cnt_aggregate_exp_tunrnout(
                       .df,
                       .turnout_col = .turnout,
                       .grp_cols = .x,
                       .type = "sum"
                     ) |>
                       purrr::set_names("levels", "turnout") |>
                       dplyr::transmute(Demo = .x,
                                        Levels = as.character(levels),
                                        Turnout = turnout)
                   })
  }

#' Utility function to get count of number of voters in demo
#'
#' @param .df Data frame containing `.demos`
#' @param .demos Vector of demo column names
#'
#' @return Data frame
#' @export
#'
ana_get_demo_totals <- function(.df, .demos) {
  # TODO: Change from `count` to something faster
  purrr::map(
    .demos,
    ~ dplyr::count(.df, !!rlang::sym(.x), name = "Total") |>
      dplyr::rename(Universe = 1) |>
      dplyr::mutate(Demo = .x, .before = 1) |>
      purrr::set_names(c("Demo", "Levels", "Total")) |>
      dplyr::mutate(Levels = as.character(Levels))
  ) |>
    dplyr::bind_rows()
}


#' Convert the sums into percentage of the total
#'
#' @param .df Data frame inherited from ana_get_demo_totals
#' @param .universes Universe columns
#' @param .total_col The total column
#'
#' @return Data frane
#' @export

ana_convert_pct_of <- function(.df,
                               .universes,
                               .total_col = "Total") {
  msg_missing_cols(.df = .df, .cols = c(.universes, .total_col))
  df <- collapse::ftransform(.df , demo_col = paste(Demo, Levels))
  pcts <- collapse::get_vars(df, "demo_col") |>
    dplyr::bind_cols(
      purrr::map(
        .universes,
        ~ collapse::get_elem(df, .x) / collapse::get_elem(df, .total_col)
      ) |>
        dplyr::bind_cols() |>
        purrr::set_names(.universes)
    )
  df |>
    dplyr::select(Demo, Levels) |>
    dplyr::mutate(demo_col = paste(Demo, Levels)) |>
    dplyr::left_join(pcts, by = "demo_col") |>
    dplyr::select(-demo_col)
}


#' Pipiline that exports the demo counts, the demo percentages in all voters, 
#' and the demos percentages in the expected turnout
#'
#' @param .df Data frame containing `.demos` and `.universes`
#' @param .demo_cols Demo columns to group universe counts by
#' @param .universe_cols Universe columns to sum 
#' @param .turnout_col Name of turnout column in `.df`
#'
#' @return A list of three data frames
#' @export
#'

ana_demo_data_pipeline <-
  function(.df,
           .demo_cols,
           .universe_cols,
           .turnout_col) {
    # msg_missing_cols(.df = .df,
    #                  .cols = c(.demo_cols, .universe_cols, .turnout_col))
    # Calculate total voters in group
    demo_sums <- ana_get_demo_totals(.df = .df,
                                     .demos = .demo_cols)
    # Calculate expected turnout by group
    demo_turnout <-
      ana_calc_turnout_by_demo(.df = .df,
                               .demos = .demo_cols,
                               .turnout = .turnout_col)
    # Calculate universe counts by group
    demo_counts <- ana_get_demo_counts(.df = .df,
                                       .demos = .demo_cols,
                                       .universes = .universe_cols)
    # Join all count data frames
    df_demos <- list(demo_sums,
                     demo_turnout,
                     demo_counts) |> purrr::reduce(dplyr::left_join)
    
    demo_pct_of_voters <- ana_convert_pct_of(.df = df_demos,
                                             .universes = .universe_cols,
                                             .total_col = "Total")
    
    demo_pct_of_turnout <- ana_convert_pct_of(.df = df_demos,
                                              .universes = .universe_cols,
                                              .total_col = "Turnout")
    list(
      demo_counts = df_demos,
      pct_voters = demo_pct_of_voters,
      pct_turnout = demo_pct_of_turnout
    )
  }



#' Sub function for ana_clean_demos_pipeline, recode age range 
#'
#' @param .agerange_col Age range col
#'
#' @return A data frame col
#' @export
#'
ana_clean_agerange_levels <- function(.agerange_col) {
  dplyr::case_when(
    .col == 1 ~ "18 to 24",
    .col == 2 ~ "25 to 29",
    .col == 3 ~ "30 to 39",
    .col == 4 ~ "40 to 49",
    .col == 5 ~ "50 to 59",
    .col == 6 ~ "60 to 74",
    .col == 7 ~ "75+",
    .col == 0 ~ "unknown",
    TRUE ~ "unknown"
  )
}

#' Subfunction for ana_clean_demos_pipeline, onvert Household ID to sequence number 
#'
#' @param .df Data frame that contains the household ID col
#' @param .hh_col Household ID col
#'
#' @return Data frame with the converted household sequence column
#' @export
#'
ana_convert_hh_to_numeric <-
  function(.df, .hh_col = "HouseHoldID_Custom") {
    hh_id <-
      collapse::qTBL(collapse::funique(collapse::get_elem(.df, .hh_col))) %>%
      purrr::set_names(.hh_col) %>%
      collapse::ftransform(HouseHoldID = seq_row(.))
    .df %>% dplyr::left_join(hh_id, by = .hh_col) %>%
      dplyr::select(-!!rlang::sym(.hh_col))
  }


#' Sub function for ana_data_export_pipeline, 
#' a pipeline that transforms the demographic cols into more readable cols 
#'
#' @param .df Data frame that contains all the demographic cols
#' @param .new_reg Date cutoff for new registrants
#'
#' @return Data frame
#' @export
#'
ana_clean_demos_pipeline <- function(.df, .new_reg = "2020-11-01") {
  df <-
    collapse::ftransform(.df, AgeRange = ana_clean_agerange_levels(AgeRange)) %>%
    collapse::ftransform(Sex = case_when(Sex == "F" ~ "Female",
                                         Sex == "M" ~ "Male")) %>%
    collapse::ftransform(NewReg = as.numeric(RegistrationDate >= as.Date(.new_reg))) %>%
    collapse::ftransform(Cord_Cutters = collapse::replace_NA(Cord_Cutters, 0))
  ana_convert_hh_to_numeric(df)
}



#' Subfunction,create a data frame with just the universe cols and the demo cols
#'
#' @param .df Data frame that contains the universe cols
#' @param .universes Universe cols
#' @param .demo_df Data frame that contains the demographic cols, inherited in ana_data_export_pipeline 
#' @param .turnout_col Turnout score col

#' @return Data frame
#' @export
#'
ana_build_universe_demo_df <-
  function(.df, .universes, .demo_df, .turnout_col) {
    dplyr::left_join(collapse::get_vars(.df, c("rnc_regid", .universes,.turnout_col)),
                     .demo_df,
                     by = "rnc_regid")
  }


#' Pipeline, calculate the crosstab counts between universes, demos and turnout, and export in spreadsheets
#'
#' @param .conx SQL Server Connection 
#' @param .f_cols demographic cols in the dsDemos and flagsgeos tables where you want to apply filters
#' @param .f_conds demographic filters you want to apply when downloading the dsDemos and flagsgeos tables 
#' @param .df Data frame that contains the universe cols and turnout col
#' @param .universes universe cols
#' @param .turnout_col Turnout score col
#' @param .new_reg_cut cutoff date for the new regs
#' @param .geo_cols Geo cols that you want to get contact method counts by
#' @param .file_path File path where you want to export the spreadsheets

#'
#' @return Spreadsheets, all exported to the file path
#' @export

ana_data_export_pipeline <-
  function(.conx,
           .f_cols,
           .f_conds,
           .df,
           .universes,
           .turnout_col,
           .new_reg_cut = "2020-11-01",
           .geo_cols =
             c(
               "CountyName",
               "DMA",
               "CongressionalDistrict_NextElection",
               "SenateDistrict_NextElection",
               "LegislativeDistrict_NextElection"),
           .file_path) {
    cli::cli_h1("Building Universe Counts Workbook")
    cli::cli_alert_info("Downloading demo data using {(.f_cols)} columns and with the conditions {(.f_conds)}")
    demos <- pull_data(
      conn = .conx,
      "bob",
      "dsDemos",
      .cols = .f_cols,
      .conds = .f_conds,
      .collect = TRUE
    )
    cli::cli_alert_info("Downloading cord cutter data from the {(.path flags)} table")
    cord_cutters <- pull_data(
      conn = .conx,
      "bob",
      "flagsgeos",
      .cols = .f_cols,
      .conds = .f_conds,
      .subset_cols = c("rnc_regid", "Cord_Cutters"),
      .collect = TRUE
    ) %>%
      collapse::replace_NA(0)
    cli::cli_alert_info("Joining demo and cord cutter data")
    demos_df <-
      dplyr::left_join(demos, cord_cutters, by = "rnc_regid")
    
    
    all_demos <- ana_list_crosstab_demos()
    cli::cli_alert_info("Using {all_demos} for count tabulation")
    demos_df <- ana_clean_demos_pipeline(demos_df, .new_reg_cut)
    
    cli::cli_alert_info("Building dataset")
    df <-
      ana_build_universe_demo_df(
        .df = .df,
        .universes = .universes,
        .demo_df = demos_df,
        .turnout_col = .turnout_col
      )
    cli::cli_alert_info("Getting cutoff for turnout universe")
    to_cut <-
      wpa_get_turnout_cutoff(.data = df, .to_col = .turnout_col)
    cli::cli_alert_info("Turnout universe cutoff {to_cut}")
    to_universe_mask <-
      collapse::get_elem(df, .turnout_col) > to_cut
    
    cli::cli_alert_info("Building universe counts worksheets")
    demo_cnts <- ana_demo_data_pipeline(
      df,
      .demo_cols = all_demos,
      .universe_cols = .universes,
      .turnout_col = .turnout_col
    )
    
    cli::cli_alert_info("Building turnout universe counts worksheet")
    # Create a turnout universe
    to_demo_cnts <- ana_demo_data_pipeline(
      collapse::fsubset(df, to_universe_mask),
      .demo_cols = all_demos,
      .universe_cols = .universes,
      .turnout_col = .turnout_col
    )
    
    sheets <- append(demo_cnts, list(to_demo_cnts$demo_counts))
    sheet_names <- c(names(demo_cnts), "turnout_demo_counts")
    
    build_excel_workbook(
      sheets,
      sheet_names,
      glue::glue("{.file_path}/UniverseCounts_{get_today()}.xlsx")
    )
    
    cli::cli_alert_info("Getting universe counts by contact method")
    contact_counts <- purrr::map(.geo_cols,
                                 ~ cnt_contact_universe_by_demo(df,
                                                                .split = .x,
                                                                .universes = .universes))
    build_excel_workbook(
      contact_counts,
      c(.geo_cols),
      glue::glue("{.file_path}/UniverseContactCounts_{get_today()}.xlsx")
    )
    
    
    
    cli::cli_alert_info("Calculating universe crosstabs")
    cross_tabs <- cnt_gen_filter_crosstab(df,
                                          .universes,
                                          .universes) %>% dplyr::bind_rows()
    
    wpa_export_csv(cross_tabs,
                   "Crosstabs",
                   .proj = "WPAi",
                   .path = .file_path)
  }






#' Pipeline that merges the universe cols, demographic cols, and turnout score by rnc_regid
#'
#' @param .conx SQL server connection
#' @param .df Data frame that contains the universe cols
#' @param .universes Universe cols
#' @param .f_cols Demographic cols that you want to select, a vector
#' @param .f_conds Demographic cols that you want to select, a vector
#' @param .new_reg_cut The date cutoff that determines old and new registraints 
#' @param .turnout_path The file path of the turnout scores
#'
#' @return Data frame
#' @export

ana_add_demos_pipeline <-
  function(.conx,
           .df,
           .universes,
           .f_cols,
           .f_conds,
           .new_reg_cut = "2020-11-01",
           .turnout_path = NULL) {
    cli::cli_h3("Downloading Demo Data")
    cli::cli_alert_info("Columns: {.code {(.f_cols)}}")
    cli::cli_alert_info("Conditions: {.code {(.f_conds)}}")
    demos <- pull_data(
      conn = .conx,
      "bob",
      "dsDemos",
      .cols = .f_cols,
      .conds = .f_conds,
      .collect = TRUE
    )
    cli::cli_alert_info("Downloading {.code Cord_Cutters}")
    cord_cutters <- pull_data(
      conn = .conx,
      "bob",
      "flagsgeos",
      .cols = .f_cols,
      .conds = .f_conds,
      .subset_cols = c("rnc_regid", "Cord_Cutters"),
      .collect = TRUE
    ) %>%
      collapse::replace_NA(0)
    cli::cli_alert_info("Joining Demo and {.code Cord_Cutter} data")
    demos_df <-
      dplyr::left_join(demos, cord_cutters, by = "rnc_regid")
    
    demos_df <- ana_clean_demos_pipeline(demos_df, .new_reg_cut)
    
    cli::cli_alert_info("Building dataset")
    df <-
      ana_build_universe_demo_df(.df = .df,
                                 .universes = .universes,
                                 .demo_df = demos_df)
    
    if (!is.null(.turnout_path)) {
      cli::cli_alert_info("Adding turnout model {.path {(.turnout_path)}}")
      to_model <- rio::import(.turnout_path)
      
      if (!("Turnout_Score" %in% colnames(to_model))) {
        rlang::abort("Turnout_Score not in turnout model data frame")
      }
      return(dplyr::left_join(df,
                              collapse::get_vars(
                                to_model, c("rnc_regid", "Turnout_Score")
                              ),
                              by = "rnc_regid"))
    }
    df
  }





#' Title
#'
#' @param .df Data frame that contains all the universe cols
#' @param .def_tbl table with the universe definitions. Defaults to `u_tbl`
#' @param .settings project settings
#' @param .return_all Logical, whether return the ds_msgs (with all the scores and universes) as a object 
#'                    in the local environment
#'
#' @return Universes data frame with definitions, can also return the whole target population's 
#'         scores and universes if needed
#' @export
#'
#' @examples 
#' ana_universe_pipeline(
#' .df=df,
#' .def_tbl = u_tbl,
#' .settings = settings,
#' .return_all = TRUE
#' )
ana_universe_pipeline <- function(.df,
                                  .def_tbl = u_tbl,
                                  .settings = settings,
                                  .return_all = TRUE) {
  cli::cli_alert_info("Building universes")
  universes <-
    ana_build_universes(.df, .def_tbl$name, .def_tbl$define)
  fs::dir_create(.settings$path, "Reports")
  cli::cli_alert_info("Exporting universe definitions")
  e_tbl <-
    ana_append_universe_counts(universes, .def_tbl = .def_tbl)
  data.table::fwrite(e_tbl, fs::path(.settings$path, "Reports", "universe_counts.csv"))
  cli::cli_alert_info("Joining universes and scores")
  df_msgs <- dplyr::left_join(.df, universes) %>% wpa_trim_scores(.digits = 6)
  fs::dir_create(.settings$path, "Scores")
  cli::cli_alert_info("Exporting universes and scores")
  fst::write_fst(df_msgs,
                 fs::path(
                   .settings$path,
                   "Scores",
                   paste(
                     .settings$project,
                     "ScoresAndFlags",
                     get_today(),
                     ".fst",
                     sep = "_"
                   )
                 ))
  if(isTRUE(.return_all)){
    return(df_msgs)
  }
  universes
}



#' Sub function, 
#'
#' @param .df Universe data frame
#' @param .def_tbl Universe definition data frame
#'
#' @return A data frame for universe totals and percentages
#' @export
#'
#' @examples
#' ana_append_universe_counts(universes, .def_tbl = .def_tbl)

ana_append_universe_counts <- function(.df, .def_tbl) {
  .def_tbl %>%
    dplyr::left_join(
      .df %>%
        dplyr::select(-rnc_regid) %>%
        collapse::fsum() %>%
        tibble::enframe("name",
                        "count")
    ) %>%
    dplyr::mutate(Pct = count / nrow(.df))
}



#' Grab the universe names in a data frame that contain '_Tight', '_Flag', '_Broad', and other patterns if needed
#'
#' @param .df Data frame with the universes
#' @param .pat String patterns that the target universe colnames contain
#'
#' @return A vector of universe names
#' @export
#'
#' @examples
#' ana_grab_universe_col_names(df)
ana_grab_universe_col_names <- function(.df, .pat = NULL) {
  u_pat <- c("_Tight", "_Flag", "_Broad", .pat)
  dplyr::select(.df, dplyr::contains(u_pat)) %>% colnames()
}



#' Calculate the crosstabs by a single demo col, also a subfunction of ana_calc_crosstab_by_demo_s
#'
#' @param .df Data frame that contains the demos and universes
#' @param .demo A demo col
#' @param .cross_x Universe cols as rows of the count table
#' @param .cross_y Universe cols as cols of the count table
#'
#' @return Data frame of the crosstabs by a demo
#' @export

ana_calc_crosstab_by_demo <-
  function(.df, .demo, .cross_x, .cross_y) {
    g_splits <-
      dplyr::group_split(.df, !!rlang::sym(.demo), .keep = TRUE)
    
    purrr::map(g_splits,
               ~ {
                 the_level <- dplyr::pull(.x, !!rlang::sym(.demo)) %>% unique()
                 
                 cnt_gen_filter_crosstab(.x, .x_cols = .cross_x, .y_cols = .cross_y) %>%
                   purrr::pluck(1) %>%
                   mutate(Demo = .demo, .before = 1) %>%
                   mutate(Level = the_level, .before = 2)
                 
                 
               }) %>%
      bind_rows()
  }

#' Calculate the crosstabs by demo cols
#'
#' @param .df Data frame that contains the demos and universes
#' @param .demo Demo cols
#' @param .cross_x Universe cols as rows of the count table
#' @param .cross_y Universe cols as cols of the count table
#'
#' @return A list of crosstabs by each individual demo 
#' @export
#'
#' @examples
#' crosses <- names(msg_models %>% select(contains('broad'), contains('tight'), contains('flag'), vh22g_voted, contains('good')))
#' ana_calc_crosstab_by_demo_s(msg_models, .demos = crosses, .cross_x = crosses, .cross_y = crosses)

ana_calc_crosstab_by_demo_s <-
  function(.df, .demos, .cross_x, .cross_y) {
    purrr::map(
      .demos,
      ~ ana_calc_crosstab_by_demo(
        .df = .df,
        .demo = .x,
        .cross_x = .cross_x,
        .cross_y = .cross_y
      )
    )
  }


#' Calculate expected turnout by demos
#'
#' @param .df Data frame that contains demo cols and the turnout col
#' @param .demos Demo cols
#' @param .turnout Turnout cols
#'
#' @return A data framne for counts with Demo group, demo level, and expected turnout
#' @export
#'
#' @examples
#' ana_calc_to_by_demos(df, demo_names[2], "TurnoutPrimary")

ana_calc_to_by_demos <- function(.df, .demos, .turnout) {
  purrr::map_dfr(
    .demos,
    ~ collapse::collap(
      X = .df,
      by = stats::as.formula(paste("~", .x)),
      FUN = collapse::fsum,
      cols = c(.turnout)
    ) %>%
      dplyr::mutate(Demo = .x, .before = 1) %>%
      purrr::set_names(c("Demo", "Turnout", "Level")) %>%
      dplyr::mutate(Level = as.character(Level)) %>%
      dplyr::select(Demo, Level, Turnout)
  )
}




#' Calculate the expected turnout by demos for certain target universes
#'
#' @param .df Data frame that contains the universe cols, demo cols, and the turnout col
#' @param .universes Universe cols
#' @param .demos Demo cols
#' @param .turnout Turnout col
#' @param .wide Logical, want the final output to be a wide table or not(long table). Defaults to `FALSE`
#'
#' @return A table df 
#' @export 
#'
#' @examples
#' ana_sum_turnout_pipeline(combo, the_universes, the_demos, "Turnout", TRUE)

ana_sum_turnout_pipeline <-
  function(.df, .universes, .demos, .turnout, .wide = FALSE) {
    df <- purrr::map(.universes,
                     ~ {
                       ana_filter_universe(.data = .df, .universe = .x) %>%
                         ana_calc_to_by_demos(.demos = .demos,
                                              .turnout = .turnout) %>%
                         dplyr::mutate(Universe = .x, .before = 1)
                     }) %>%
      dplyr::bind_rows()
    
    if (isTRUE(.wide)) {
      return(tidyr::spread(df, Universe, Turnout))
    }
    df
  }




#' Subset only certain demo groups and certain cols of a table df with demo rows 
#'
#' @param .df A table data frame that contains all the demo rows and the universe cols
#' @param .rows Demo rows that we want to subset
#' @param .cols Universe cols that we want to subset
#' @param .row_col The demo col where all the rows are different demographic categories we want to subset, Defaults to `Demo`
#'
#' @return A table data frame
#' @export
#'
#' @examples
# to_tbl <- rio::import("Reports/BfC_NY1_Cong_TurnoutByUniverseDemo_v1_220628.csv")
# subset<-ana_select_universe_tbl(
# to_tbl,
# c("Sex", "AgeRange", "Race"),
# c( "ConsiderStrong_Bond_Flag","Consider_Bond_Flag","ConsiderStrong_Maga_Flag",
# "Consider_Maga_Flag","ConsiderStrong_Native_Flag","Consider_Native_Flag"),
# .row_col = "Demo")

ana_select_universe_tbl <-
  function(.df, .rows, .cols, .row_col = "Demo") {
    dplyr::select(.df, dplyr::all_of(c(.row_col, "Level", .cols))) %>%
      dplyr::filter(!!rlang::sym(.row_col) %in% .rows) %>%
      dplyr::mutate(!!rlang::sym(.row_col) <- factor(
        !!rlang::sym(.row_col),
        levels = .rows,
        ordered = TRUE
      )) %>%
      arrange(!!rlang::sym(.row_col))
  }



#' Subset a crosstab data frame
#'
#' @param .df A data frame that include all the crosstab counts
#' @param .rows Universe names that you want to keep as rows  in the new subset
#' @param .cols Universe names that you want to keep as cols in the new subset
#'
#' @return A subset of the original crosstab data frame
#' @export
#'
#' @examples
# overlaps <- rio::import(overlaps_path)
# overlap_s <- purrr::map2(
#   list(
#     core_cols,
#     core_cols,
#     core_cols,
#     image_cols,
#     image_cols,
#     issue_cols
#   ),
#   list(
#     image_cols,
#     issue_cols,
#     party_cols,
#     issue_cols,
#     party_cols,
#     party_cols
#   ),
#   ~ ana_select_crosstab_tbl(.df = overlaps,
#                             .rows = .x,
#                             .cols = .y) %>%
#     pp_strip_underscores("Universe")
# )

ana_select_crosstab_tbl <- function(.df, .rows, .cols) {
  dplyr::select(.df, dplyr::all_of(c("Universe", .cols))) %>%
    dplyr::filter(Universe %in% .rows)
}




#' Pipeline, export all the crosstab counts between universes, demos and expected turnout
#'
#' @param .df Data frame that includes universe cols, demo cols, and the turnout col
#' @param .universes Universes cols
#' @param .demos Demo cols
#' @param .turnout_col Turnout col
#' @param .settings Project settings
#'
#' @return All the counts are exported to the 'Reports' folder of the project as spreadsheets. Nothing to the local environment
#' @export
#'
#' @examples
#  ana_export_analytics(.df = df_msg,
# .universes = the_universes,
# .demos = the_demos,
# .turnout_col = "TurnoutGeneral",
# .settings = p_settings)

ana_export_analytics <-
  function(.df,
           .universes,
           .demos,
           .turnout_col,
           .settings) {
    fs::dir_create(fs::path(.settings$path, "Reports"))
    cli::cli_h1("Exporting Analytics")
    cli::cli_alert_info("Exporting counts by expected turnout")
    ana_sum_turnout_pipeline(.df, .universes, .demos, .turnout_col, TRUE) %>%
      data.table::fwrite(fs::path(.settings$path, "Reports", "turnout_universe_demo.csv"))
    cli::cli_alert_info("Exporting counts by universe")
    cnt_turnout_by_universe(.df, .universes, .turnout = .turnout_col) %>%
      data.table::fwrite(fs::path(.settings$path, "Reports", "turnout_universe.csv"))
    cli::cli_alert_info("Calculating universe by demo counts and percentages")
    d_data <- ana_demo_data_pipeline(
      .df = .df,
      .demo_cols = .demos,
      .universe_cols = .universes,
      .turnout_col = .turnout_col
    )
    cli::cli_alert_info("Exporting universe by demo counts and percentages")
    build_excel_workbook(
      .dfs = d_data,
      .sheets = c("demo_data", "pct_voters", "pct_turnout"),
      .fname = fs::path(
        .settings$path,
        "Reports",
        paste0(.settings$project,
               "_",
               "Counts.xlsx")
      )
    )
    cli::cli_alert_info("Calculating universe crosstabs")
    ana_export_overlaps(.df = .df, .universes = .universes, .settings = .settings)
    # Crosstabs
    cli::cli_alert_info("Calculating counts by contact method")
    # Hack to get counts for just universes ----
    contact_methods <- cnt_contact_universe_by_demo(.df = .df,
                                                    .split = "State",
                                                    .universes = .universes) %>%
      dplyr::select(-State, -unique_cells)
    cli::cli_alert_info("Exporting counts by contact method")
    contact_methods %>%
      data.table::fwrite(fs::path(.settings$path, "Reports", "contact_method_counts.csv"))
    cli::cli_alert_success("⚡ Complete")
  }



#' Utility function to export just the crosstabs between universes to Reports folder
#'
#' @param .df Data frame that contains all the universe cols
#' @param .universes Universe cols
#' @param .settings Project settings

#' @examples
# ana_export_analytics(
#   df_demos,
#   .universes = the_universes,
#   .demos = the_demos,
#   .turnout_col = "Turnout",
#   .settings = ps
# )

ana_export_overlaps <- function(.df, .universes, .settings) {
  overlaps <- cnt_gen_filter_crosstab(.df = .df,
                                      .x_cols = .universes,
                                      .y_cols = .universes)
  
  # Export CSV
  overlaps %>%
    purrr::pluck(1) %>%
    data.table::fwrite(fs::path(.settings$path,"Reports","universe_overlaps.csv"))
}




#' Download the demographic variables from bonfire and append to a data frame
#'
#' @param .conx SQL server connection, must be for the bonfire database
#' @param .df Data frame to which  you want to append the demographic variables 
#' @param .demos Demographic variable names you want to download and append
#' @param .tbl Bonfire table from which you want to extract the demographic variables 
#'
#' @return Data frame with demo cols appended
#' @export
#'
#' @examples
# demos <-
#   ana_add_demos_basic(
#     wpai,
#     df,
#     c(
#       "Sex",
#       "AgeRangeNew",
#       "Race",
#       "Education",
#       "CountyName",
#       "DMA",
#       "CongressionalDistrict_NextElection",
#       "SenateDistrict_NextElection",
#       "LegislativeDistrict_NextElection"
#     )
#   )

ana_add_demos_basic <- function(.conx, .df, .demos, .tbl = "commonvariables") {
  db <-
    dplyr::copy_to(.conx, .df %>% dplyr::select(rnc_regid), "#adddemos")
  df <- dplyr::left_join(db,
                         get_tbl(.conx, "bob", .tbl) %>%
                           dplyr::select(rnc_regid, dplyr::all_of(.demos))
  ) %>%
    dplyr::collect()
  DBI::dbRemoveTable(.conx, "#adddemos")
  df
}



#' Sub function of ana_turnout_issue_pipeline. 
#' Get the issue flag counts among the like voters(whose turnout score is above the cutoff)
#'
#' @param .df Data frame that contains the turnout scores(including high and low turnout scenarios) and issue flags
#' @param .turnout Turnout score for cutting off the likely voters 
#' @param .type A string, whether this is for high, low, or base turnout scenario
#' @param .issue the top issue model we want to count by turnout scenarios. Defaults to `topissue`
#'
#' @return Data frame
#' @export

ana_get_turnout_isssues <-
  function(.df, .turnout, .type,.issue = "topissue") {
    flag <- .df %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(.turnout))) %>%
      head(collapse::fsum(collapse::get_elem(.df, .turnout)))
    flag %>%
      dplyr::group_by(!!rlang::sym(.issue)) %>%
      dplyr::summarise(!!rlang::sym(.type) := dplyr::n()) %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(.type)))
  }


#' Pipeline to get the top issues by turnout scenarios
#'
#' @param .df Data frame that contains the turnout scores(including high and low turnout scenarios) and issue flags
#' @param .base TUrnout score for the base turnout scenario
#' @param .hi Turnout score for the high turnout scenario
#' @param .lo Turnout score for the low turnout scenario
#'
#' @return Data frame, combined with top issue counts by high, low, and base turnout scenarios
#' @export

ana_turnout_issue_pipeline <- function(.df, .base="TurnoutGeneral",
                                       .hi="Turnout_High",
                                       .lo="Turnout_Low"){
  base_to <- ana_get_turnout_isssues(.df, .base, "Base")
  hi_to <- ana_get_turnout_isssues(.df, .hi, "High")
  lo_to <- ana_get_turnout_isssues(.df, .lo, "Low")
  list(base_to, hi_to, lo_to) %>%
    purrr::reduce(dplyr::full_join,by="topissue")
}



#' Title
#'
#' @param .df 
#' @param .universes 
#'
#' @return
#' @export
#'
#' @examples
# FDATAN <- FDATA %>%
#   dplyr::mutate(core = case_when(Paxton_ITB == 1 ~ 'r_itb',
#                                  Paxton_Shore_Up == 1 ~ 'r_shore_up',
#                                  Paxton_GOTV == 1 ~ 'r_gotv',
#                                  Paxton_GOTV_T2 == 1 ~ 'r_gotv_t2',
#                                  AG_Persuasion == 1 ~ 'persuasion',
#                                  AG_Persuasion_T2 == 1 ~ 'persuasion_t2',
#                                  Garza_Marginals == 1 ~ 'd_marginals',
#                                  Garza_GOTV == 1 ~ 'd_gotv',
#                                  Garza_GOTV_T2 == 1 ~ 'd_gotv_t2',
#                                  Garza_ITB == 1 ~ 'd_itb',
#                                  AG_Disengaged == 1 ~ 'disengaged',
#                                  AG_Dead_Weight == 1 ~ 'dead_weight'))

# ST <- ana_core_action(ST, .universes = wpa_replace_universe_prefix("Paxton", "Garza", "AG"))

ana_core_action <- function(.df, .universes) {
  df <- collapse::get_vars(.df, c("rnc_regid",.universes))
  cau <- tidyr::gather(df, core, flag, -rnc_regid) %>%
    collapse::fsubset(flag == 1) 
  # %>%
  #   collapse::fselect(-flag)
  dplyr::left_join(.df, cau, by = "rnc_regid")
}


ana_get_top_universe_issues <-
  function(.df,
           .top_ten,
           .issue_col = "topissue",
           .core_col = "core",
           .us = "R",
           .them = "D",
           .neutral = NULL) {
    top_universe <- .df %>%
      dplyr::filter(!!rlang::sym(.issue_col) %in% .top_ten) %>%
      dplyr::count(!!rlang::sym(.core_col), !!rlang::sym(.issue_col),
                   name = "count")
    top_universe %>%
      tidyr::spread(!!rlang::sym(.core_col), count) %>%
      dplyr::select(!!rlang::sym(.issue_col),
                    wpa_replace_universe_prefix(.us, .them, .neutral))
  }




ana_plot_top_issues <- function(.df, .values, .models, .txt_size=18, .nudge=9000){
  .df %>%
    dplyr::mutate(
      key = stringr::str_replace_all(!!rlang::sym(.models), "_", " "),
      lbls = scales::comma(!!rlang::sym(.values),accuracy = 1)
    ) %>%
    ggplot(aes(!!rlang::sym(.values),
               reorder(key,
                       !!rlang::sym(.values)))) +
    geom_col(fill = "red3") +
    geom_text(aes(label = lbls),nudge_x = .nudge,size=6) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(labels = scales::comma_format()) +
    hrbrthemes::theme_tinyhand(base_family = "Franklin Gothic Book",
                               base_size = .txt_size) +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()) +
    theme(
      plot.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
}

ana_view_nets <- function(.df, .us, .them, .bins = 50) {
  df <-
    dplyr::mutate(.df, Diff = !!rlang::sym(.us)-!!rlang::sym(.them))
  
  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(Diff)) +
    ggplot2::geom_histogram(bins = .bins)
  
  t <- tibble(
    cutoff = seq(0, .75, .05),
    count = purrr::map_dbl(seq(0, .75, .05), ~ collapse::fsum(df$Diff > .x))
  ) %>%
    dplyr::mutate(pct = count / collapse::fnrow(df))
  print(txt_tbl(t))
  p
}


ana_list_all_core_action_cols <- function(.ballot_obj) {
  races <- .ballot_obj$race
  races %>%
    purrr::map(~ {
      obj <- ana_get_ballot_cols(ballot_obj, .x)
      wpa_replace_universe_prefix(obj$us_prefix, obj$them_prefix, obj$neutral_prefix)
    }) %>%
    unlist()
}

ana_export_all_files <- function(.project,
                                 .path = fs::path_wd()) {
  f_names <- fs::dir_ls(fs::path(.path, "Reports"), glob = "*.csv")
  
  fs::file_copy(f_names, new_path = fs::path(
    .path,
    "Exports",
    paste0(.project, "_", f_names %>% fs::path_file())
  ))
}
