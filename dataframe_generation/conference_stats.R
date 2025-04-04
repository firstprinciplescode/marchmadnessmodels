#### CONFERENCE SHIT

# WE'VE GOT TO BE ABLE TO GROUP THIS SHIT
# THEN WE'VE GOT TO BE ABLE TO CLUSTER THEM

# WE'RE GOING TO HAVE TO Z-SCORE AT SOME POINT. Z-SCORE THE GAMELOG_DF LATER

offense_metrics <- c("PPP", "2Pperc", "3P%", "FT%", "FTR", 
                     "TOV_perc", "STL_perc", "3PRate", "a_fgm", 
                     "BLK_perc", "ORB_perc", "FL_perc", "poss")

'%ni%' <- Negate('%in%')


## AGG STATS FOR OFFENSE, GETTING THE DIFFERENCES

conference_stats_2025 <- gamelog_2025 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  dplyr::summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2025 <- conference_stats_2025 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2025 <- conference_stats_2025 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2025 <- conference_stats_2025 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2024 <- gamelog_2024 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2024 <- conference_stats_2024 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2024 <- conference_stats_2024 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2024 <- conference_stats_2024 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2023 <- gamelog_2023 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2023 <- conference_stats_2023 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2023 <- conference_stats_2023 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2023 <- conference_stats_2023 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2022 <- gamelog_2022 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2022 <- conference_stats_2022 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2022 <- conference_stats_2022 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2022 <- conference_stats_2022 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2021 <- gamelog_2021 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2021 <- conference_stats_2021 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2021 <- conference_stats_2021 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2021 <- conference_stats_2021 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2020 <- gamelog_2020 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2020 <- conference_stats_2020 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2020 <- conference_stats_2020 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2020 <- conference_stats_2020 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2019 <- gamelog_2019 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2019 <- conference_stats_2019 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2019 <- conference_stats_2019 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2019 <- conference_stats_2019 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2018 <- gamelog_2018 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2018 <- conference_stats_2018 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2018 <- conference_stats_2018 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2018 <- conference_stats_2018 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2017 <- gamelog_2017 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2017 <- conference_stats_2017 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2017 <- conference_stats_2017 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2017 <- conference_stats_2017 %>%
  select(Conference, !ends_with("_diff"))


conference_stats_2016 <- gamelog_2016 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

conference_stats_2016 <- conference_stats_2016 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

conference_diff_2016 <- conference_stats_2016 %>%
  select(Conference, ends_with("_diff"))

conference_stats_2016 <- conference_stats_2016 %>%
  select(Conference, !ends_with("_diff"))



#
#


### CALCULATING THE PERCENTILES

metric_cols_conf_perc <- setdiff(colnames(conference_stats_2024), "Conference")

invert_flags_conf_perc <- c(FALSE, FALSE,  # PPP
                  FALSE, FALSE,  # 2Pperc
                  FALSE, FALSE,  # 3P%
                  FALSE, FALSE,  # FT%
                  FALSE, FALSE,  # FTR
                  TRUE, TRUE,    # TOV_perc (invert)
                  TRUE, TRUE,    # STL_perc (invert)
                  FALSE, FALSE,  # 3PRate
                  FALSE, FALSE,  # a_fgm
                  TRUE, TRUE,    # BLK_perc (invert)
                  FALSE, FALSE,
                  FALSE, FALSE,
                  F, F)  # ORB_perc, FL_perc, poss

percentile_rescale <- function(x, invert = FALSE) {
  if (invert) {
    return(rescale(x, to = c(100, 0), from = range(x, na.rm = TRUE))) # Inverted scaling
  } else {
    return(rescale(x, to = c(0, 100), from = range(x, na.rm = TRUE))) # Normal scaling
  }
}


conference_stats_2025_perc <- conference_stats_2025 %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(metric_cols_conf_perc)) {
      col_name <- metric_cols_conf_perc[i]
      inv_flag <- invert_flags_conf_perc[i]
      new_col_name <- paste0(col_name, "_perc")
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  select(Conference, ends_with("_perc"))


conference_stats_2024_perc <- conference_stats_2024 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2024_perc <- conference_stats_2024_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2023_perc <- conference_stats_2023 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2023_perc <- conference_stats_2023_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2022_perc <- conference_stats_2022 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2022_perc <- conference_stats_2022_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2021_perc <- conference_stats_2021 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2021_perc <- conference_stats_2021_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2020_perc <- conference_stats_2020 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2020_perc <- conference_stats_2020_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2019_perc <- conference_stats_2019 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2019_perc <- conference_stats_2019_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2018_perc <- conference_stats_2018 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2018_perc <- conference_stats_2018_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2017_perc <- conference_stats_2017 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2017_perc <- conference_stats_2017_perc %>% select(Conference, ends_with("_perc"))

conference_stats_2016_perc <- conference_stats_2016 %>%
  mutate(across(all_of(metric_cols_conf_perc), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]), 
                .names = "{.col}_perc"))
conference_stats_2016_perc <- conference_stats_2016_perc %>% select(Conference, ends_with("_perc"))


##
##


metric_cols_conf_perc_diff <- setdiff(colnames(conference_diff_2024), "Conference")

invert_flags_conf_perc_diff <- c(FALSE,  # PPP
                            FALSE,   # 2Pperc
                            FALSE,   # 3P%
                            FALSE,  # FT%
                            FALSE,   # FTR
                            TRUE,    # TOV_perc (invert)
                            TRUE,   # STL_perc (invert)
                            FALSE, # 3PRate
                            FALSE, # a_fgm
                            TRUE,   # BLK_perc (invert)
                            FALSE,
                            FALSE, 
                            F)  # ORB_perc, FL_perc, poss


conference_diff_2025_perc <- conference_diff_2025 %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(metric_cols_conf_perc_diff)) {
      col_name <- metric_cols_conf_perc_diff[i]
      inv_flag <- invert_flags_conf_perc_diff[i]
      new_col_name <- paste0(col_name, "_perc")
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  select(Conference, ends_with("_perc"))


conference_diff_2024_perc <- conference_diff_2024 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2024_perc <- conference_diff_2024_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2023_perc <- conference_diff_2023 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2023_perc <- conference_diff_2023_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2022_perc <- conference_diff_2022 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2022_perc <- conference_diff_2022_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2021_perc <- conference_diff_2021 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2021_perc <- conference_diff_2021_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2020_perc <- conference_diff_2020 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2020_perc <- conference_diff_2020_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2019_perc <- conference_diff_2019 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2019_perc <- conference_diff_2019_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2018_perc <- conference_diff_2018 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2018_perc <- conference_diff_2018_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2017_perc <- conference_diff_2017 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2017_perc <- conference_diff_2017_perc %>% select(Conference, ends_with("_perc"))


conference_diff_2016_perc <- conference_diff_2016 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff), 
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]), 
                .names = "{.col}_perc"))

conference_diff_2016_perc <- conference_diff_2016_perc %>% select(Conference, ends_with("_perc"))


##
## FOR NON-CONF PLAY, DUE TO AVAILABILITY OF THE NON_CONF PERCENTILES, LESS HETEROSKEDACITY SHIT OR WHATEVER, CONSIDER USING THOSE NEXT TIME - I.E. DON'T USE LOCATIONIND, JUST MEAN THOSE DEF_NON_CONF_XXX_PERC COLUMNS
## ACTUALLY, WE DO IT IN THE NON_CONF_PERC ROWS
##


non_conference_stats_2025 <- gamelog_2025 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  dplyr::summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2025 <- non_conference_stats_2025 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2025 <- non_conference_stats_2025 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2025 <- non_conference_stats_2025 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2024 <- gamelog_2024 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2024 <- non_conference_stats_2024 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2024 <- non_conference_stats_2024 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2024 <- non_conference_stats_2024 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2023 <- gamelog_2023 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2023 <- non_conference_stats_2023 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2023 <- non_conference_stats_2023 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2023 <- non_conference_stats_2023 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2022 <- gamelog_2022 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2022 <- non_conference_stats_2022 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2022 <- non_conference_stats_2022 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2022 <- non_conference_stats_2022 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2021 <- gamelog_2021 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2021 <- non_conference_stats_2021 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2021 <- non_conference_stats_2021 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2021 <- non_conference_stats_2021 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2020 <- gamelog_2020 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2020 <- non_conference_stats_2020 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2020 <- non_conference_stats_2020 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2020 <- non_conference_stats_2020 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2019 <- gamelog_2019 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2019 <- non_conference_stats_2019 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2019 <- non_conference_stats_2019 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2019 <- non_conference_stats_2019 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2018 <- gamelog_2018 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2018 <- non_conference_stats_2018 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2018 <- non_conference_stats_2018 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2018 <- non_conference_stats_2018 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2017 <- gamelog_2017 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2017 <- non_conference_stats_2017 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2017 <- non_conference_stats_2017 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2017 <- non_conference_stats_2017 %>%
  select(Conference, !ends_with("_diff"))


non_conference_stats_2016 <- gamelog_2016 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  mutate(LocationInd = ifelse(Location == "H", "H", "A")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(offense_metrics), 
                   list(home_mean = ~ mean(.x[LocationInd == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[LocationInd != "H"], na.rm = TRUE))))

non_conference_stats_2016 <- non_conference_stats_2016 %>%
  mutate(
    PPP_mean_diff = PPP_away_mean - PPP_home_mean,
    `2Pperc_mean_diff` = `2Pperc_away_mean` - `2Pperc_home_mean`,
    `3P%_mean_diff` = `3P%_away_mean` - `3P%_home_mean`,
    `FT%_mean_diff` = `FT%_away_mean` - `FT%_home_mean`,
    FTR_mean_diff = FTR_away_mean - FTR_home_mean,
    TOV_perc_mean_diff = TOV_perc_away_mean - TOV_perc_home_mean,
    STL_perc_mean_diff = STL_perc_away_mean - STL_perc_home_mean,
    `3PRate_mean_diff` = `3PRate_away_mean` - `3PRate_home_mean`,
    a_fgm_mean_diff = a_fgm_away_mean - a_fgm_home_mean,
    BLK_perc_mean_diff = BLK_perc_away_mean - BLK_perc_home_mean,
    ORB_perc_mean_diff = ORB_perc_away_mean - ORB_perc_home_mean,
    FL_perc_mean_diff = FL_perc_away_mean - FL_perc_home_mean,
    poss_mean_diff = poss_away_mean - poss_home_mean
  )

non_conference_diff_2016 <- non_conference_stats_2016 %>%
  select(Conference, ends_with("_diff"))

non_conference_stats_2016 <- non_conference_stats_2016 %>%
  select(Conference, !ends_with("_diff"))


##
##


non_conf_perc_metrics <- colnames(gamelog_2024[c(203:228)])

non_conference_perc_2025 <- gamelog_2025 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  dplyr::summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2024 <- gamelog_2024 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2023 <- gamelog_2023 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2022 <- gamelog_2022 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2021 <- gamelog_2021 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2020 <- gamelog_2020 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2019 <- gamelog_2019 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2018 <- gamelog_2018 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2017 <- gamelog_2017 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_2016 <- gamelog_2016 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Conference) %>%
  summarise(across(all_of(non_conf_perc_metrics), ~ mean(.x, na.rm = TRUE)))


##
##


non_conference_perc_2025 <- non_conference_perc_2025 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2025_diff <- non_conference_perc_2025 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2025 <- non_conference_perc_2025 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2025_diff), "Conference")

non_conference_perc_2025_diff <- non_conference_perc_2025_diff %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(diff_cols_only)) {
      col_name <- diff_cols_only[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0(col_name, "_perc")
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  select(Conference, ends_with("_diff_perc"))



non_conference_perc_2024 <- non_conference_perc_2024 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2024_diff <- non_conference_perc_2024 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2024 <- non_conference_perc_2024 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2024_diff), "Conference")

non_conference_perc_2024_diff <- non_conference_perc_2024_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2023 <- non_conference_perc_2023 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2023_diff <- non_conference_perc_2023 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2023 <- non_conference_perc_2023 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2023_diff), "Conference")

non_conference_perc_2023_diff <- non_conference_perc_2023_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2022 <- non_conference_perc_2022 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2022_diff <- non_conference_perc_2022 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2022 <- non_conference_perc_2022 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2022_diff), "Conference")

non_conference_perc_2022_diff <- non_conference_perc_2022_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2021 <- non_conference_perc_2021 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2021_diff <- non_conference_perc_2021 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2021 <- non_conference_perc_2021 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2021_diff), "Conference")

non_conference_perc_2021_diff <- non_conference_perc_2021_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2020 <- non_conference_perc_2020 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2020_diff <- non_conference_perc_2020 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2020 <- non_conference_perc_2020 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2020_diff), "Conference")

non_conference_perc_2020_diff <- non_conference_perc_2020_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2019 <- non_conference_perc_2019 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2019_diff <- non_conference_perc_2019 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2019 <- non_conference_perc_2019 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2019_diff), "Conference")

non_conference_perc_2019_diff <- non_conference_perc_2019_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2018 <- non_conference_perc_2018 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2018_diff <- non_conference_perc_2018 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2018 <- non_conference_perc_2018 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2018_diff), "Conference")

non_conference_perc_2018_diff <- non_conference_perc_2018_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2017 <- non_conference_perc_2017 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2017_diff <- non_conference_perc_2017 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2017 <- non_conference_perc_2017 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2017_diff), "Conference")

non_conference_perc_2017_diff <- non_conference_perc_2017_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))


non_conference_perc_2016 <- non_conference_perc_2016 %>%
  mutate(
    def_non_conf_PPP_diff = def_non_conf_away_perc_PPP - def_non_conf_home_perc_PPP,
    def_non_conf_2Pperc_diff = def_non_conf_away_perc_2Pperc - def_non_conf_home_perc_2Pperc,
    `def_non_conf_3P%_diff` = `def_non_conf_away_perc_3P%` - `def_non_conf_home_perc_3P%`,
    `def_non_conf_FT%_diff` = `def_non_conf_away_perc_FT%` - `def_non_conf_home_perc_FT%`,
    def_non_conf_FTR_diff = def_non_conf_away_perc_FTR - def_non_conf_home_perc_FTR,
    def_non_conf_TOV_perc_diff = def_non_conf_away_perc_TOV_perc - def_non_conf_home_perc_TOV_perc,
    def_non_conf_STL_perc_diff = def_non_conf_away_perc_STL_perc - def_non_conf_home_perc_STL_perc,
    def_non_conf_3PRate_diff = def_non_conf_away_perc_3PRate - def_non_conf_home_perc_3PRate,
    def_non_conf_a_fgm_diff = def_non_conf_away_perc_a_fgm - def_non_conf_home_perc_a_fgm,
    def_non_conf_BLK_perc_diff = def_non_conf_away_perc_BLK_perc - def_non_conf_home_perc_BLK_perc,
    def_non_conf_ORB_perc_diff = def_non_conf_away_perc_ORB_perc - def_non_conf_home_perc_ORB_perc,
    def_non_conf_FL_perc_diff = def_non_conf_away_perc_FL_perc - def_non_conf_home_perc_FL_perc,
    def_non_conf_poss_diff = def_non_conf_away_perc_poss - def_non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_2016_diff <- non_conference_perc_2016 %>%
  select(Conference, ends_with("_diff"))

non_conference_perc_2016 <- non_conference_perc_2016 %>%
  select(Conference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_only <- setdiff(colnames(non_conference_perc_2016_diff), "Conference")

non_conference_perc_2016_diff <- non_conference_perc_2016_diff %>%
  mutate(across(all_of(diff_cols_only),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), diff_cols_only)]),
                .names = "{.col}_perc")) %>% select(Conference, ends_with("_diff_perc"))



# GOTTA ADD THE DIFFERENCES HERE


###
### PUT THEM ALL TOGETHER - THIS IS OFFENSE
###

# WITHIN THE CONFERENCE
conference_final_2016 <- cbind(conference_stats_2016_perc %>% arrange(Conference), conference_diff_2016_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2017 <- cbind(conference_stats_2017_perc %>% arrange(Conference), conference_diff_2017_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2018 <- cbind(conference_stats_2018_perc %>% arrange(Conference), conference_diff_2018_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2019 <- cbind(conference_stats_2019_perc %>% arrange(Conference), conference_diff_2019_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2020 <- cbind(conference_stats_2020_perc %>% arrange(Conference), conference_diff_2020_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2021 <- cbind(conference_stats_2021_perc %>% arrange(Conference), conference_diff_2021_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2022 <- cbind(conference_stats_2022_perc %>% arrange(Conference), conference_diff_2022_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2023 <- cbind(conference_stats_2023_perc %>% arrange(Conference), conference_diff_2023_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2024 <- cbind(conference_stats_2024_perc %>% arrange(Conference), conference_diff_2024_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
conference_final_2025 <- cbind(conference_stats_2025_perc %>% arrange(Conference), conference_diff_2025_perc %>% arrange(Conference) %>% ungroup() %>% select(-Conference))

# OUTSIDE CONFERENCE
non_conference_final_2025 <- cbind(non_conference_perc_2025 %>% arrange(Conference), non_conference_perc_2025_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2024 <- cbind(non_conference_perc_2024 %>% arrange(Conference), non_conference_perc_2024_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2023 <- cbind(non_conference_perc_2023 %>% arrange(Conference), non_conference_perc_2023_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2022 <- cbind(non_conference_perc_2022 %>% arrange(Conference), non_conference_perc_2022_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2021 <- cbind(non_conference_perc_2021 %>% arrange(Conference), non_conference_perc_2021_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2020 <- cbind(non_conference_perc_2020 %>% arrange(Conference), non_conference_perc_2020_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2019 <- cbind(non_conference_perc_2019 %>% arrange(Conference), non_conference_perc_2019_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2018 <- cbind(non_conference_perc_2018 %>% arrange(Conference), non_conference_perc_2018_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2017 <- cbind(non_conference_perc_2017 %>% arrange(Conference), non_conference_perc_2017_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))
non_conference_final_2016 <- cbind(non_conference_perc_2016 %>% arrange(Conference), non_conference_perc_2016_diff  %>% arrange(Conference) %>% ungroup() %>% select(-Conference))

