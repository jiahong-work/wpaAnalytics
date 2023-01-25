ana_list_all_demos <- function(.extra){
  c("State",
    "Party",
    "RegisteredParty",
    "DTCalcParty",
    "Sex",
    "Race",
    "Age",
    "AgeRange",
    "Education",
    "HouseholdIncome",
    "OTT_Address",
    "CongressionalDistrict",
    "CongressionalDistrict_NextElection",
    "SenateDistrict",
    "SenateDistrict_NextElection",
    "LegislativeDistrict",
    "LegislativeDistrict_NextElection",
    "Precinct",
    "Lat",
    "Lng",
    "DMA",
    "CountyName",
    "CensusBlock",
    "RegistrationAddressZip5",
    "RegistrationDate",
    "ActiveVoter",
    "LandLine_ReliabilityCode",
    "CellPhone_ReliabilityCode",
    "Cord_Cutters",
    "NewReg",
    "HouseHoldID",
    .extra)
}





ana_list_contact_cols <- function(.extra=NULL){
  c("Cable_Subscribers",
    "Christian_Radio",
    "Conservative_Talk",
    "Cord_Cutters",
    "Country_Radio",
    "Delayed_TV_Viewers",
    "Local_Sports_Talk",
    "Online_Activists",
    "Offline_Activists",
    "Premium_Streaming_Services_User",
    "R_Email_Donors",
    "R_Txt_Donors",
    "Satellite_Subscribers",
    "Streamed_Music_Viewers",
    "Streamed_Video_Viewers",
    .extra)
}

ana_list_all_universes <-
  function(.df,
           .prefix_us = "R",
           .prefix_them = "D",
           .prefix_neutral = NULL,
           .extra = NULL) {
    c(wpa_replace_universe_prefix(.us = .prefix_us,
                                  .them = .prefix_them,
                                  .neutral = .prefix_neutral),
      ana_grab_universe_col_names(.df),
      .extra
    )
  }


ana_list_basic_demos <- function(.extra=NULL){
  c("Sex",
    "AgeRange",
    "Race",
    "Education",
    "HouseholdIncome",
    "NewReg",
    "CountyName",
    "DMA",
    "CongressionalDistrict_NextElection",
    "SenateDistrict_NextElection",
    "LegislativeDistrict_NextElection",
    "RegistrationAddressZip5",
    .extra)
}


ana_list_crosstab_demos <- function() {
  c(
    "Sex",
    "AgeRange",
    "Race",
    "Education",
    "HouseholdIncome",
    "NewReg",
    "ActiveVoter",
    "CountyName",
    "DMA",
    "CongressionalDistrict_NextElection",
    "SenateDistrict_NextElection",
    "LegislativeDistrict_NextElection",
    "CongressionalDistrict",
    "SenateDistrict",
    "LegislativeDistrict",
    "RegistrationAddressZip5",
    "Precinct"
  )
}

ana_group_universe_cols <- function(.path) {
  fname <- fs::path(.path, "Reports", "universe_counts.csv")
  df <- rio::import(fname)
  c_names <- df %>%
    dplyr::group_split(group) %>%
    purrr::map( ~ dplyr::pull(.x, name))
  u_names <- dplyr::group_keys(df, group) %>% dplyr::pull(group)
  names(c_names) <- u_names
  c_names
}
