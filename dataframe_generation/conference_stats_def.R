
offense_metrics <- c("PPP", "2Pperc", "3P%", "FT%", "FTR",
                     "TOV_perc", "STL_perc", "3PRate", "a_fgm",
                     "BLK_perc", "ORB_perc", "FL_perc", "poss")

oppconference_stats_2025 <- gamelog_2025 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  dplyr::summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2025 <- oppconference_stats_2025 %>%
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

oppconference_diff_def_2025 <- oppconference_stats_2025 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2025 <- oppconference_stats_2025 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2024 <- gamelog_2024 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2024 <- oppconference_stats_2024 %>%
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

oppconference_diff_def_2024 <- oppconference_stats_2024 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2024 <- oppconference_stats_2024 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2023 <- gamelog_2023 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2023 <- oppconference_stats_2023 %>%
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

oppconference_diff_def_2023 <- oppconference_stats_2023 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2023 <- oppconference_stats_2023 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2022 <- gamelog_2022 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2022 <- oppconference_stats_2022 %>%
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

oppconference_diff_def_2022 <- oppconference_stats_2022 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2022 <- oppconference_stats_2022 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2021 <- gamelog_2021 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2021 <- oppconference_stats_2021 %>%
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

oppconference_diff_def_2021 <- oppconference_stats_2021 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2021 <- oppconference_stats_2021 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2020 <- gamelog_2020 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2020 <- oppconference_stats_2020 %>%
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

oppconference_diff_def_2020 <- oppconference_stats_2020 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2020 <- oppconference_stats_2020 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2019 <- gamelog_2019 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2019 <- oppconference_stats_2019 %>%
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

oppconference_diff_def_2019 <- oppconference_stats_2019 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2019 <- oppconference_stats_2019 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2018 <- gamelog_2018 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2018 <- oppconference_stats_2018 %>%
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

oppconference_diff_def_2018 <- oppconference_stats_2018 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2018 <- oppconference_stats_2018 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2017 <- gamelog_2017 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2017 <- oppconference_stats_2017 %>%
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

oppconference_diff_def_2017 <- oppconference_stats_2017 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2017 <- oppconference_stats_2017 %>%
  select(OppConference, !ends_with("_diff"))


oppconference_stats_2016 <- gamelog_2016 %>%
  filter(GameType %in% c("CTOURN", "REG (Conf)")) %>%
  mutate(OppLocationInd = ifelse(OppLocation == "H", "H", "A")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(offense_metrics),
                   list(home_mean = ~ mean(.x[Location == "H"], na.rm = TRUE),
                        away_mean = ~ mean(.x[Location != "H"], na.rm = TRUE))))

oppconference_stats_2016 <- oppconference_stats_2016 %>%
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

oppconference_diff_def_2016 <- oppconference_stats_2016 %>%
  select(OppConference, ends_with("_diff"))

oppconference_stats_2016 <- oppconference_stats_2016 %>%
  select(OppConference, !ends_with("_diff"))


####
####


oppconference_stats_2025_perc <- oppconference_stats_2025 %>%
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
    select(OppConference, ends_with("_perc"))


oppconference_stats_2024_perc <- oppconference_stats_2024 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2024_perc <- oppconference_stats_2024_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2023_perc <- oppconference_stats_2023 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2023_perc <- oppconference_stats_2023_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2022_perc <- oppconference_stats_2022 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2022_perc <- oppconference_stats_2022_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2021_perc <- oppconference_stats_2021 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2021_perc <- oppconference_stats_2021_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2020_perc <- oppconference_stats_2020 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2020_perc <- oppconference_stats_2020_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2019_perc <- oppconference_stats_2019 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2019_perc <- oppconference_stats_2019_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2018_perc <- oppconference_stats_2018 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2018_perc <- oppconference_stats_2018_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2017_perc <- oppconference_stats_2017 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2017_perc <- oppconference_stats_2017_perc %>% select(OppConference, ends_with("_perc"))

oppconference_stats_2016_perc <- oppconference_stats_2016 %>%
  mutate(across(all_of(metric_cols_conf_perc),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc[match(cur_column(), metric_cols_conf_perc)]),
                .names = "{.col}_perc"))
oppconference_stats_2016_perc <- oppconference_stats_2016_perc %>% select(OppConference, ends_with("_perc"))


##
##


oppconference_diff_def_2025_perc <- oppconference_diff_def_2025 %>%
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
    select(OppConference, ends_with("_perc"))


oppconference_diff_def_2024_perc <- oppconference_diff_def_2024 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2024_perc <- oppconference_diff_def_2024_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2023_perc <- oppconference_diff_def_2023 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2023_perc <- oppconference_diff_def_2023_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2022_perc <- oppconference_diff_def_2022 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2022_perc <- oppconference_diff_def_2022_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2021_perc <- oppconference_diff_def_2021 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2021_perc <- oppconference_diff_def_2021_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2020_perc <- oppconference_diff_def_2020 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2020_perc <- oppconference_diff_def_2020_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2019_perc <- oppconference_diff_def_2019 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2019_perc <- oppconference_diff_def_2019_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2018_perc <- oppconference_diff_def_2018 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2018_perc <- oppconference_diff_def_2018_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2017_perc <- oppconference_diff_def_2017 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2017_perc <- oppconference_diff_def_2017_perc %>% select(OppConference, ends_with("_perc"))


oppconference_diff_def_2016_perc <- oppconference_diff_def_2016 %>%
  mutate(across(all_of(metric_cols_conf_perc_diff),
                ~ percentile_rescale(.x, invert = invert_flags_conf_perc_diff[match(cur_column(), metric_cols_conf_perc_diff)]),
                .names = "{.col}_perc"))

oppconference_diff_def_2016_perc <- oppconference_diff_def_2016_perc %>% select(OppConference, ends_with("_perc"))

##
##


non_conf_perc_metrics_def <- colnames(gamelog_2024[c(125:150)])

non_conference_perc_def_2025 <- gamelog_2025 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  dplyr::summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2024 <- gamelog_2024 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  dplyr::summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2023 <- gamelog_2023 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2022 <- gamelog_2022 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2021 <- gamelog_2021 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2020 <- gamelog_2020 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2019 <- gamelog_2019 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2018 <- gamelog_2018 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2017 <- gamelog_2017 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2016 <- gamelog_2016 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_metrics_def), ~ mean(.x, na.rm = TRUE)))


##
##



non_conf_perc_def_metrics <- colnames(gamelog_2024[c(125:150)])

non_conference_perc_def_2025 <- gamelog_2025 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  dplyr::summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2024 <- gamelog_2024 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2023 <- gamelog_2023 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2022 <- gamelog_2022 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2021 <- gamelog_2021 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2020 <- gamelog_2020 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2019 <- gamelog_2019 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2018 <- gamelog_2018 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2017 <- gamelog_2017 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))

non_conference_perc_def_2016 <- gamelog_2016 %>%
  filter(GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(OppConference) %>%
  summarise(across(all_of(non_conf_perc_def_metrics), ~ mean(.x, na.rm = TRUE)))


##
##


non_conference_perc_def_2025 <- non_conference_perc_def_2025 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2025_diff <- non_conference_perc_def_2025 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2025 <- non_conference_perc_def_2025 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2025_diff), "OppConference")

non_conference_perc_def_2025_diff <- non_conference_perc_def_2025_diff %>%
    group_modify(~ {
        df <- .x
        for (i in seq_along(diff_cols_def_only)) {
            col_name <- diff_cols_def_only[i]
            inv_flag <- invert_flags_def[i]
            new_col_name <- paste0(col_name, "_perc")
            df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
        }
        df
    }) %>%
    select(OppConference, ends_with("_perc")) %>%
    arrange(OppConference)


non_conference_perc_def_2024 <- non_conference_perc_def_2024 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2024_diff <- non_conference_perc_def_2024 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2024 <- non_conference_perc_def_2024 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2024_diff), "OppConference")

non_conference_perc_def_2024_diff <- non_conference_perc_def_2024_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2023 <- non_conference_perc_def_2023 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2023_diff <- non_conference_perc_def_2023 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2023 <- non_conference_perc_def_2023 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2023_diff), "OppConference")

non_conference_perc_def_2023_diff <- non_conference_perc_def_2023_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2022 <- non_conference_perc_def_2022 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2022_diff <- non_conference_perc_def_2022 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2022 <- non_conference_perc_def_2022 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2022_diff), "OppConference")

non_conference_perc_def_2022_diff <- non_conference_perc_def_2022_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2021 <- non_conference_perc_def_2021 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2021_diff <- non_conference_perc_def_2021 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2021 <- non_conference_perc_def_2021 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2021_diff), "OppConference")

non_conference_perc_def_2021_diff <- non_conference_perc_def_2021_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2020 <- non_conference_perc_def_2020 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2020_diff <- non_conference_perc_def_2020 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2020 <- non_conference_perc_def_2020 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2020_diff), "OppConference")

non_conference_perc_def_2020_diff <- non_conference_perc_def_2020_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2019 <- non_conference_perc_def_2019 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2019_diff <- non_conference_perc_def_2019 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2019 <- non_conference_perc_def_2019 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2019_diff), "OppConference")

non_conference_perc_def_2019_diff <- non_conference_perc_def_2019_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2018 <- non_conference_perc_def_2018 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2018_diff <- non_conference_perc_def_2018 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2018 <- non_conference_perc_def_2018 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2018_diff), "OppConference")

non_conference_perc_def_2018_diff <- non_conference_perc_def_2018_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2017 <- non_conference_perc_def_2017 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2017_diff <- non_conference_perc_def_2017 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2017 <- non_conference_perc_def_2017 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2017_diff), "OppConference")

non_conference_perc_def_2017_diff <- non_conference_perc_def_2017_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


non_conference_perc_def_2016 <- non_conference_perc_def_2016 %>%
  mutate(
    non_conf_PPP_diff = non_conf_away_perc_PPP - non_conf_home_perc_PPP,
    non_conf_2Pperc_diff = non_conf_away_perc_2Pperc - non_conf_home_perc_2Pperc,
    `non_conf_3P%_diff` = `non_conf_away_perc_3P%` - `non_conf_home_perc_3P%`,
    `non_conf_FT%_diff` = `non_conf_away_perc_FT%` - `non_conf_home_perc_FT%`,
    non_conf_FTR_diff = non_conf_away_perc_FTR - non_conf_home_perc_FTR,
    non_conf_TOV_perc_diff = non_conf_away_perc_TOV_perc - non_conf_home_perc_TOV_perc,
    non_conf_STL_perc_diff = non_conf_away_perc_STL_perc - non_conf_home_perc_STL_perc,
    non_conf_3PRate_diff = non_conf_away_perc_3PRate - non_conf_home_perc_3PRate,
    non_conf_a_fgm_diff = non_conf_away_perc_a_fgm - non_conf_home_perc_a_fgm,
    non_conf_BLK_perc_diff = non_conf_away_perc_BLK_perc - non_conf_home_perc_BLK_perc,
    non_conf_ORB_perc_diff = non_conf_away_perc_ORB_perc - non_conf_home_perc_ORB_perc,
    non_conf_FL_perc_diff = non_conf_away_perc_FL_perc - non_conf_home_perc_FL_perc,
    non_conf_poss_diff = non_conf_away_perc_poss - non_conf_home_perc_poss
  )

# Step 2: Split off diff columns
non_conference_perc_def_2016_diff <- non_conference_perc_def_2016 %>%
  select(OppConference, ends_with("_diff"))

# Step 3: Remove diff columns from original
non_conference_perc_def_2016 <- non_conference_perc_def_2016 %>%
  select(OppConference, !ends_with("_diff"))

# Step 4: Percentile rescale with invert flags
invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
diff_cols_def_only <- setdiff(colnames(non_conference_perc_def_2016_diff), "OppConference")

non_conference_perc_def_2016_diff <- non_conference_perc_def_2016_diff %>%
  mutate(across(all_of(diff_cols_def_only),
                ~ percentile_rescale(.x, invert = invert_flags_def[match(cur_column(), diff_cols_def_only)]),
                .names = "{.col}_perc")) %>% select(OppConference, ends_with("_perc")) %>% arrange(OppConference)


####
####

# THESE ARE WITHIN THE CONFERENCE.
conference_def_final_2025 <- cbind(oppconference_stats_2025_perc %>% arrange(OppConference), oppconference_diff_def_2025_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2024 <- cbind(oppconference_stats_2024_perc %>% arrange(OppConference), oppconference_diff_def_2024_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2023 <- cbind(oppconference_stats_2023_perc %>% arrange(OppConference), oppconference_diff_def_2023_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2022 <- cbind(oppconference_stats_2022_perc %>% arrange(OppConference), oppconference_diff_def_2022_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2021 <- cbind(oppconference_stats_2021_perc %>% arrange(OppConference), oppconference_diff_def_2021_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2020 <- cbind(oppconference_stats_2020_perc %>% arrange(OppConference), oppconference_diff_def_2020_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2019 <- cbind(oppconference_stats_2019_perc %>% arrange(OppConference), oppconference_diff_def_2019_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2018 <- cbind(oppconference_stats_2018_perc %>% arrange(OppConference), oppconference_diff_def_2018_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2017 <- cbind(oppconference_stats_2017_perc %>% arrange(OppConference), oppconference_diff_def_2017_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
conference_def_final_2016 <- cbind(oppconference_stats_2016_perc %>% arrange(OppConference), oppconference_diff_def_2016_perc %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))


## AND NON-CONFERENCE DEFENSE


non_conference_def_final_2025 <- cbind(non_conference_perc_def_2025 %>% arrange(OppConference), non_conference_perc_def_2025_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2024 <- cbind(non_conference_perc_def_2024 %>% arrange(OppConference), non_conference_perc_def_2024_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2023 <- cbind(non_conference_perc_def_2023 %>% arrange(OppConference), non_conference_perc_def_2023_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2022 <- cbind(non_conference_perc_def_2022 %>% arrange(OppConference), non_conference_perc_def_2022_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2021 <- cbind(non_conference_perc_def_2021 %>% arrange(OppConference), non_conference_perc_def_2021_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2020 <- cbind(non_conference_perc_def_2020 %>% arrange(OppConference), non_conference_perc_def_2020_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2019 <- cbind(non_conference_perc_def_2019 %>% arrange(OppConference), non_conference_perc_def_2019_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2018 <- cbind(non_conference_perc_def_2018 %>% arrange(OppConference), non_conference_perc_def_2018_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2017 <- cbind(non_conference_perc_def_2017 %>% arrange(OppConference), non_conference_perc_def_2017_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))
non_conference_def_final_2016 <- cbind(non_conference_perc_def_2016 %>% arrange(OppConference), non_conference_perc_def_2016_diff %>% arrange(OppConference) %>% ungroup() %>% select(-OppConference))

