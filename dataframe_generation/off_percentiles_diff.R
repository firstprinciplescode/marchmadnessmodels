# I WOULD NEED TO AGGREGATE THE BASE STATISTICS FIRST
# THEN WE DO THE PERCENTILES FOR THE DIFFERENCES ONLY!!! ONLY!!!

# THEN WE JOIN THAT BACK IN WITH THE ORIGINAL PERCENTILES AND SHIT
# THE ORIGINAL PERCENTILES ARE GOOD. THE DIFF --> NEEDS TO BE BETTER


offense_avg_metrics <- c('PPP', '2Pperc', '3P%', 'FT%', 'FTR', 'TOV_perc', 'STL_perc', '3PRate', 'a_fgm', 'BLK_perc', 'ORB_perc', 'FL_perc', 'poss')
defense_avg_def_metrics <- paste0('opp', offense_avg_metrics)

'%ni%' <- Negate('%in%')


gamelog_2016_home_avg <- gamelog_2016 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2016_away_avg <- gamelog_2016 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2016_conf_home_avg <- gamelog_2016 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2016_conf_away_avg <- gamelog_2016 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2016_non_conf_home_avg <- gamelog_2016 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2016_non_conf_home_avg) <- paste0(colnames(gamelog_2016_non_conf_home_avg), "_non_conf_home")

gamelog_2016_non_conf_away_avg <- gamelog_2016 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2016_non_conf_away_avg) <- paste0(colnames(gamelog_2016_non_conf_away_avg), "_non_conf_away")



gamelog_2016_non_conf_avg_intermediary <- full_join(gamelog_2016_non_conf_home_avg, gamelog_2016_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2016_non_conf_avg_intermediary <- gamelog_2016_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2016_avg_diff <- cbind(gamelog_2016_home_avg[,c(1:2)], gamelog_2016_away_avg[,c(3:15)] - gamelog_2016_home_avg[,c(3:15)])

gamelog_2016_conf_avg_diff <- cbind(gamelog_2016_conf_home_avg[,c(1:2)], gamelog_2016_conf_away_avg[,c(3:15)] - gamelog_2016_conf_home_avg[,c(3:15)])

gamelog_2016_non_conf_avg_diff <- cbind(gamelog_2016_non_conf_avg_intermediary[,c(1:2)], gamelog_2016_non_conf_avg_intermediary[,c(16:28)] - gamelog_2016_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2016_non_conf_avg_diff) <- colnames(gamelog_2016_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2016_avg_diff)[c(3:15)]

gamelog_2016_avg_diff_perc <- gamelog_2016_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2016_conf_avg_diff_perc <- gamelog_2016_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2016_conf_avg_diff_perc) <- paste0(colnames(gamelog_2016_conf_avg_diff_perc), "_conf")

gamelog_2016_non_conf_avg_diff_perc <- gamelog_2016_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2017_home_avg <- gamelog_2017 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2017_away_avg <- gamelog_2017 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2017_conf_home_avg <- gamelog_2017 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2017_conf_away_avg <- gamelog_2017 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2017_non_conf_home_avg <- gamelog_2017 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2017_non_conf_home_avg) <- paste0(colnames(gamelog_2017_non_conf_home_avg), "_non_conf_home")

gamelog_2017_non_conf_away_avg <- gamelog_2017 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2017_non_conf_away_avg) <- paste0(colnames(gamelog_2017_non_conf_away_avg), "_non_conf_away")



gamelog_2017_non_conf_avg_intermediary <- full_join(gamelog_2017_non_conf_home_avg, gamelog_2017_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2017_non_conf_avg_intermediary <- gamelog_2017_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2017_avg_diff <- cbind(gamelog_2017_home_avg[,c(1:2)], gamelog_2017_away_avg[,c(3:15)] - gamelog_2017_home_avg[,c(3:15)])

gamelog_2017_conf_avg_diff <- cbind(gamelog_2017_conf_home_avg[,c(1:2)], gamelog_2017_conf_away_avg[,c(3:15)] - gamelog_2017_conf_home_avg[,c(3:15)])

gamelog_2017_non_conf_avg_diff <- cbind(gamelog_2017_non_conf_avg_intermediary[,c(1:2)], gamelog_2017_non_conf_avg_intermediary[,c(16:28)] - gamelog_2017_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2017_non_conf_avg_diff) <- colnames(gamelog_2017_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2017_avg_diff)[c(3:15)]

gamelog_2017_avg_diff_perc <- gamelog_2017_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2017_conf_avg_diff_perc <- gamelog_2017_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2017_conf_avg_diff_perc) <- paste0(colnames(gamelog_2017_conf_avg_diff_perc), "_conf")

gamelog_2017_non_conf_avg_diff_perc <- gamelog_2017_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2018_home_avg <- gamelog_2018 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2018_away_avg <- gamelog_2018 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2018_conf_home_avg <- gamelog_2018 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2018_conf_away_avg <- gamelog_2018 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2018_non_conf_home_avg <- gamelog_2018 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2018_non_conf_home_avg) <- paste0(colnames(gamelog_2018_non_conf_home_avg), "_non_conf_home")

gamelog_2018_non_conf_away_avg <- gamelog_2018 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2018_non_conf_away_avg) <- paste0(colnames(gamelog_2018_non_conf_away_avg), "_non_conf_away")



gamelog_2018_non_conf_avg_intermediary <- full_join(gamelog_2018_non_conf_home_avg, gamelog_2018_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2018_non_conf_avg_intermediary <- gamelog_2018_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2018_avg_diff <- cbind(gamelog_2018_home_avg[,c(1:2)], gamelog_2018_away_avg[,c(3:15)] - gamelog_2018_home_avg[,c(3:15)])

gamelog_2018_conf_avg_diff <- cbind(gamelog_2018_conf_home_avg[,c(1:2)], gamelog_2018_conf_away_avg[,c(3:15)] - gamelog_2018_conf_home_avg[,c(3:15)])

gamelog_2018_non_conf_avg_diff <- cbind(gamelog_2018_non_conf_avg_intermediary[,c(1:2)], gamelog_2018_non_conf_avg_intermediary[,c(16:28)] - gamelog_2018_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2018_non_conf_avg_diff) <- colnames(gamelog_2018_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2018_avg_diff)[c(3:15)]

gamelog_2018_avg_diff_perc <- gamelog_2018_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2018_conf_avg_diff_perc <- gamelog_2018_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2018_conf_avg_diff_perc) <- paste0(colnames(gamelog_2018_conf_avg_diff_perc), "_conf")

gamelog_2018_non_conf_avg_diff_perc <- gamelog_2018_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2019_home_avg <- gamelog_2019 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2019_away_avg <- gamelog_2019 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2019_conf_home_avg <- gamelog_2019 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2019_conf_away_avg <- gamelog_2019 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2019_non_conf_home_avg <- gamelog_2019 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2019_non_conf_home_avg) <- paste0(colnames(gamelog_2019_non_conf_home_avg), "_non_conf_home")

gamelog_2019_non_conf_away_avg <- gamelog_2019 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2019_non_conf_away_avg) <- paste0(colnames(gamelog_2019_non_conf_away_avg), "_non_conf_away")



gamelog_2019_non_conf_avg_intermediary <- full_join(gamelog_2019_non_conf_home_avg, gamelog_2019_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2019_non_conf_avg_intermediary <- gamelog_2019_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2019_avg_diff <- cbind(gamelog_2019_home_avg[,c(1:2)], gamelog_2019_away_avg[,c(3:15)] - gamelog_2019_home_avg[,c(3:15)])

gamelog_2019_conf_avg_diff <- cbind(gamelog_2019_conf_home_avg[,c(1:2)], gamelog_2019_conf_away_avg[,c(3:15)] - gamelog_2019_conf_home_avg[,c(3:15)])

gamelog_2019_non_conf_avg_diff <- cbind(gamelog_2019_non_conf_avg_intermediary[,c(1:2)], gamelog_2019_non_conf_avg_intermediary[,c(16:28)] - gamelog_2019_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2019_non_conf_avg_diff) <- colnames(gamelog_2019_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2019_avg_diff)[c(3:15)]

gamelog_2019_avg_diff_perc <- gamelog_2019_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2019_conf_avg_diff_perc <- gamelog_2019_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2019_conf_avg_diff_perc) <- paste0(colnames(gamelog_2019_conf_avg_diff_perc), "_conf")

gamelog_2019_non_conf_avg_diff_perc <- gamelog_2019_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2020_home_avg <- gamelog_2020 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2020_away_avg <- gamelog_2020 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2020_conf_home_avg <- gamelog_2020 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2020_conf_away_avg <- gamelog_2020 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2020_non_conf_home_avg <- gamelog_2020 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2020_non_conf_home_avg) <- paste0(colnames(gamelog_2020_non_conf_home_avg), "_non_conf_home")

gamelog_2020_non_conf_away_avg <- gamelog_2020 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2020_non_conf_away_avg) <- paste0(colnames(gamelog_2020_non_conf_away_avg), "_non_conf_away")



gamelog_2020_non_conf_avg_intermediary <- full_join(gamelog_2020_non_conf_home_avg, gamelog_2020_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2020_non_conf_avg_intermediary <- gamelog_2020_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2020_avg_diff <- cbind(gamelog_2020_home_avg[,c(1:2)], gamelog_2020_away_avg[,c(3:15)] - gamelog_2020_home_avg[,c(3:15)])

gamelog_2020_conf_avg_diff <- cbind(gamelog_2020_conf_home_avg[,c(1:2)], gamelog_2020_conf_away_avg[,c(3:15)] - gamelog_2020_conf_home_avg[,c(3:15)])

gamelog_2020_non_conf_avg_diff <- cbind(gamelog_2020_non_conf_avg_intermediary[,c(1:2)], gamelog_2020_non_conf_avg_intermediary[,c(16:28)] - gamelog_2020_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2020_non_conf_avg_diff) <- colnames(gamelog_2020_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2020_avg_diff)[c(3:15)]

gamelog_2020_avg_diff_perc <- gamelog_2020_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2020_conf_avg_diff_perc <- gamelog_2020_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2020_conf_avg_diff_perc) <- paste0(colnames(gamelog_2020_conf_avg_diff_perc), "_conf")

gamelog_2020_non_conf_avg_diff_perc <- gamelog_2020_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2021_home_avg <- gamelog_2021 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2021_home_avg) <- paste0(colnames(gamelog_2021_home_avg), "_home")

gamelog_2021_away_avg <- gamelog_2021 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2021_away_avg) <- paste0(colnames(gamelog_2021_away_avg), "_away")

gamelog_2021_conf_home_avg <- gamelog_2021 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2021_conf_home_avg) <- paste0(colnames(gamelog_2021_conf_home_avg), "_conf_home")

gamelog_2021_conf_away_avg <- gamelog_2021 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2021_conf_away_avg) <- paste0(colnames(gamelog_2021_conf_away_avg), "_conf_away")

gamelog_2021_non_conf_home_avg <- gamelog_2021 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2021_non_conf_home_avg) <- paste0(colnames(gamelog_2021_non_conf_home_avg), "_non_conf_home")

gamelog_2021_non_conf_away_avg <- gamelog_2021 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2021_non_conf_away_avg) <- paste0(colnames(gamelog_2021_non_conf_away_avg), "_non_conf_away")


gamelog_2021_avg_intermediary <- full_join(gamelog_2021_home_avg, gamelog_2021_away_avg, by = c("Team_home" = "Team_away", "Conference_home" = "Conference_away")) %>% arrange(Team_home)

gamelog_2021_conf_avg_intermediary <- full_join(gamelog_2021_conf_home_avg, gamelog_2021_conf_away_avg, by = c("Team_conf_home" = "Team_conf_away", "Conference_conf_home" = "Conference_conf_away")) %>% arrange(Team_conf_home)

gamelog_2021_non_conf_avg_intermediary <- full_join(gamelog_2021_non_conf_home_avg, gamelog_2021_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)

gamelog_2021_avg_intermediary <- gamelog_2021_avg_intermediary %>%
  mutate(
    PPP_mean_home = coalesce(PPP_mean_home, PPP_mean_away),
    PPP_mean_away = coalesce(PPP_mean_away, PPP_mean_home),

    `2Pperc_mean_home` = coalesce(`2Pperc_mean_home`, `2Pperc_mean_away`),
    `2Pperc_mean_away` = coalesce(`2Pperc_mean_away`, `2Pperc_mean_home`),

    `3P%_mean_home` = coalesce(`3P%_mean_home`, `3P%_mean_away`),
    `3P%_mean_away` = coalesce(`3P%_mean_away`, `3P%_mean_home`),

    `FT%_mean_home` = coalesce(`FT%_mean_home`, `FT%_mean_away`),
    `FT%_mean_away` = coalesce(`FT%_mean_away`, `FT%_mean_home`),

    FTR_mean_home = coalesce(FTR_mean_home, FTR_mean_away),
    FTR_mean_away = coalesce(FTR_mean_away, FTR_mean_home),

    TOV_perc_mean_home = coalesce(TOV_perc_mean_home, TOV_perc_mean_away),
    TOV_perc_mean_away = coalesce(TOV_perc_mean_away, TOV_perc_mean_home),

    STL_perc_mean_home = coalesce(STL_perc_mean_home, STL_perc_mean_away),
    STL_perc_mean_away = coalesce(STL_perc_mean_away, STL_perc_mean_home),

    `3PRate_mean_home` = coalesce(`3PRate_mean_home`, `3PRate_mean_away`),
    `3PRate_mean_away` = coalesce(`3PRate_mean_away`, `3PRate_mean_home`),

    a_fgm_mean_home = coalesce(a_fgm_mean_home, a_fgm_mean_away),
    a_fgm_mean_away = coalesce(a_fgm_mean_away, a_fgm_mean_home),

    BLK_perc_mean_home = coalesce(BLK_perc_mean_home, BLK_perc_mean_away),
    BLK_perc_mean_away = coalesce(BLK_perc_mean_away, BLK_perc_mean_home),

    ORB_perc_mean_home = coalesce(ORB_perc_mean_home, ORB_perc_mean_away),
    ORB_perc_mean_away = coalesce(ORB_perc_mean_away, ORB_perc_mean_home),

    FL_perc_mean_home = coalesce(FL_perc_mean_home, FL_perc_mean_away),
    FL_perc_mean_away = coalesce(FL_perc_mean_away, FL_perc_mean_home),

    poss_mean_home = coalesce(poss_mean_home, poss_mean_away),
    poss_mean_away = coalesce(poss_mean_away, poss_mean_home)
  )

gamelog_2021_conf_avg_intermediary <- gamelog_2021_conf_avg_intermediary %>%
  mutate(
    PPP_mean_conf_home = coalesce(PPP_mean_conf_home, PPP_mean_conf_away),
    PPP_mean_conf_away = coalesce(PPP_mean_conf_away, PPP_mean_conf_home),

    `2Pperc_mean_conf_home` = coalesce(`2Pperc_mean_conf_home`, `2Pperc_mean_conf_away`),
    `2Pperc_mean_conf_away` = coalesce(`2Pperc_mean_conf_away`, `2Pperc_mean_conf_home`),

    `3P%_mean_conf_home` = coalesce(`3P%_mean_conf_home`, `3P%_mean_conf_away`),
    `3P%_mean_conf_away` = coalesce(`3P%_mean_conf_away`, `3P%_mean_conf_home`),

    `FT%_mean_conf_home` = coalesce(`FT%_mean_conf_home`, `FT%_mean_conf_away`),
    `FT%_mean_conf_away` = coalesce(`FT%_mean_conf_away`, `FT%_mean_conf_home`),

    FTR_mean_conf_home = coalesce(FTR_mean_conf_home, FTR_mean_conf_away),
    FTR_mean_conf_away = coalesce(FTR_mean_conf_away, FTR_mean_conf_home),

    TOV_perc_mean_conf_home = coalesce(TOV_perc_mean_conf_home, TOV_perc_mean_conf_away),
    TOV_perc_mean_conf_away = coalesce(TOV_perc_mean_conf_away, TOV_perc_mean_conf_home),

    STL_perc_mean_conf_home = coalesce(STL_perc_mean_conf_home, STL_perc_mean_conf_away),
    STL_perc_mean_conf_away = coalesce(STL_perc_mean_conf_away, STL_perc_mean_conf_home),

    `3PRate_mean_conf_home` = coalesce(`3PRate_mean_conf_home`, `3PRate_mean_conf_away`),
    `3PRate_mean_conf_away` = coalesce(`3PRate_mean_conf_away`, `3PRate_mean_conf_home`),

    a_fgm_mean_conf_home = coalesce(a_fgm_mean_conf_home, a_fgm_mean_conf_away),
    a_fgm_mean_conf_away = coalesce(a_fgm_mean_conf_away, a_fgm_mean_conf_home),

    BLK_perc_mean_conf_home = coalesce(BLK_perc_mean_conf_home, BLK_perc_mean_conf_away),
    BLK_perc_mean_conf_away = coalesce(BLK_perc_mean_conf_away, BLK_perc_mean_conf_home),

    ORB_perc_mean_conf_home = coalesce(ORB_perc_mean_conf_home, ORB_perc_mean_conf_away),
    ORB_perc_mean_conf_away = coalesce(ORB_perc_mean_conf_away, ORB_perc_mean_conf_home),

    FL_perc_mean_conf_home = coalesce(FL_perc_mean_conf_home, FL_perc_mean_conf_away),
    FL_perc_mean_conf_away = coalesce(FL_perc_mean_conf_away, FL_perc_mean_conf_home),

    poss_mean_conf_home = coalesce(poss_mean_conf_home, poss_mean_conf_away),
    poss_mean_conf_away = coalesce(poss_mean_conf_away, poss_mean_conf_home)
  )

gamelog_2021_non_conf_avg_intermediary <- gamelog_2021_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2021_avg_diff <- cbind(gamelog_2021_avg_intermediary[,c(1:2)], gamelog_2021_avg_intermediary[,c(16:28)] - gamelog_2021_avg_intermediary[,c(3:15)])
colnames(gamelog_2021_avg_diff) <- colnames(gamelog_2021_avg_diff)

gamelog_2021_conf_avg_diff <- cbind(gamelog_2021_conf_avg_intermediary[,c(1:2)], gamelog_2021_conf_avg_intermediary[,c(16:28)] - gamelog_2021_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2021_conf_avg_diff) <- colnames(gamelog_2021_conf_avg_diff)

gamelog_2021_non_conf_avg_diff <- cbind(gamelog_2021_non_conf_avg_intermediary[,c(1:2)], gamelog_2021_non_conf_avg_intermediary[,c(16:28)] - gamelog_2021_non_conf_avg_intermediary[,c(3:15)])


metric_avg_cols_2021 <- colnames(gamelog_2021_avg_diff)[c(3:15)]

gamelog_2021_avg_diff_perc <- gamelog_2021_avg_diff %>%
  mutate(across(all_of(metric_avg_cols_2021),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols_2021)]), .names = "{.col}_perc"))  %>%
  select(Team_home, Conference_home, ends_with("_perc"))


metric_conf_avg_cols_2021 <- colnames(gamelog_2021_conf_avg_diff)[c(3:15)]

gamelog_2021_conf_avg_diff_perc <- gamelog_2021_conf_avg_diff %>%
  mutate(across(all_of(metric_conf_avg_cols_2021),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_conf_avg_cols_2021)]), .names = "{.col}_perc"))  %>%
  select(Team_conf_home, Conference_conf_home, ends_with("_perc"))
colnames(gamelog_2021_conf_avg_diff_perc) <- paste0(colnames(gamelog_2021_conf_avg_diff_perc), "_conf")


metric_non_conf_avg_cols_2021 <- colnames(gamelog_2021_non_conf_avg_diff)[c(3:15)]

gamelog_2021_non_conf_avg_diff_perc <- gamelog_2021_non_conf_avg_diff %>%
  mutate(across(all_of(metric_non_conf_avg_cols_2021),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_non_conf_avg_cols_2021)]), .names = "{.col}_perc"))  %>%
  select(Team_non_conf_home, Conference_non_conf_home, ends_with("_perc"))


####
####
####



gamelog_2022_home_avg <- gamelog_2022 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2022_away_avg <- gamelog_2022 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2022_conf_home_avg <- gamelog_2022 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2022_conf_away_avg <- gamelog_2022 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2022_non_conf_home_avg <- gamelog_2022 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2022_non_conf_home_avg) <- paste0(colnames(gamelog_2022_non_conf_home_avg), "_non_conf_home")

gamelog_2022_non_conf_away_avg <- gamelog_2022 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2022_non_conf_away_avg) <- paste0(colnames(gamelog_2022_non_conf_away_avg), "_non_conf_away")



gamelog_2022_non_conf_avg_intermediary <- full_join(gamelog_2022_non_conf_home_avg, gamelog_2022_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2022_non_conf_avg_intermediary <- gamelog_2022_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2022_avg_diff <- cbind(gamelog_2022_home_avg[,c(1:2)], gamelog_2022_away_avg[,c(3:15)] - gamelog_2022_home_avg[,c(3:15)])

gamelog_2022_conf_avg_diff <- cbind(gamelog_2022_conf_home_avg[,c(1:2)], gamelog_2022_conf_away_avg[,c(3:15)] - gamelog_2022_conf_home_avg[,c(3:15)])

gamelog_2022_non_conf_avg_diff <- cbind(gamelog_2022_non_conf_avg_intermediary[,c(1:2)], gamelog_2022_non_conf_avg_intermediary[,c(16:28)] - gamelog_2022_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2022_non_conf_avg_diff) <- colnames(gamelog_2022_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2022_avg_diff)[c(3:15)]

gamelog_2022_avg_diff_perc <- gamelog_2022_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2022_conf_avg_diff_perc <- gamelog_2022_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2022_conf_avg_diff_perc) <- paste0(colnames(gamelog_2022_conf_avg_diff_perc), "_conf")

gamelog_2022_non_conf_avg_diff_perc <- gamelog_2022_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2023_home_avg <- gamelog_2023 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2023_away_avg <- gamelog_2023 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2023_conf_home_avg <- gamelog_2023 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2023_conf_away_avg <- gamelog_2023 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2023_non_conf_home_avg <- gamelog_2023 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2023_non_conf_home_avg) <- paste0(colnames(gamelog_2023_non_conf_home_avg), "_non_conf_home")

gamelog_2023_non_conf_away_avg <- gamelog_2023 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2023_non_conf_away_avg) <- paste0(colnames(gamelog_2023_non_conf_away_avg), "_non_conf_away")



gamelog_2023_non_conf_avg_intermediary <- full_join(gamelog_2023_non_conf_home_avg, gamelog_2023_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2023_non_conf_avg_intermediary <- gamelog_2023_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2023_avg_diff <- cbind(gamelog_2023_home_avg[,c(1:2)], gamelog_2023_away_avg[,c(3:15)] - gamelog_2023_home_avg[,c(3:15)])

gamelog_2023_conf_avg_diff <- cbind(gamelog_2023_conf_home_avg[,c(1:2)], gamelog_2023_conf_away_avg[,c(3:15)] - gamelog_2023_conf_home_avg[,c(3:15)])

gamelog_2023_non_conf_avg_diff <- cbind(gamelog_2023_non_conf_avg_intermediary[,c(1:2)], gamelog_2023_non_conf_avg_intermediary[,c(16:28)] - gamelog_2023_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2023_non_conf_avg_diff) <- colnames(gamelog_2023_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2023_avg_diff)[c(3:15)]

gamelog_2023_avg_diff_perc <- gamelog_2023_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2023_conf_avg_diff_perc <- gamelog_2023_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2023_conf_avg_diff_perc) <- paste0(colnames(gamelog_2023_conf_avg_diff_perc), "_conf")

gamelog_2023_non_conf_avg_diff_perc <- gamelog_2023_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2024_home_avg <- gamelog_2024 %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2024_away_avg <- gamelog_2024 %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2024_conf_home_avg <- gamelog_2024 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2024_conf_away_avg <- gamelog_2024 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2024_non_conf_home_avg <- gamelog_2024 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2024_non_conf_home_avg) <- paste0(colnames(gamelog_2024_non_conf_home_avg), "_non_conf_home")

gamelog_2024_non_conf_away_avg <- gamelog_2024 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2024_non_conf_away_avg) <- paste0(colnames(gamelog_2024_non_conf_away_avg), "_non_conf_away")



gamelog_2024_non_conf_avg_intermediary <- full_join(gamelog_2024_non_conf_home_avg, gamelog_2024_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2024_non_conf_avg_intermediary <- gamelog_2024_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2024_avg_diff <- cbind(gamelog_2024_home_avg[,c(1:2)], gamelog_2024_away_avg[,c(3:15)] - gamelog_2024_home_avg[,c(3:15)])

gamelog_2024_conf_avg_diff <- cbind(gamelog_2024_conf_home_avg[,c(1:2)], gamelog_2024_conf_away_avg[,c(3:15)] - gamelog_2024_conf_home_avg[,c(3:15)])

gamelog_2024_non_conf_avg_diff <- cbind(gamelog_2024_non_conf_avg_intermediary[,c(1:2)], gamelog_2024_non_conf_avg_intermediary[,c(16:28)] - gamelog_2024_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2024_non_conf_avg_diff) <- colnames(gamelog_2024_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2024_avg_diff)[c(3:15)]

gamelog_2024_avg_diff_perc <- gamelog_2024_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))

gamelog_2024_conf_avg_diff_perc <- gamelog_2024_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))
colnames(gamelog_2024_conf_avg_diff_perc) <- paste0(colnames(gamelog_2024_conf_avg_diff_perc), "_conf")

gamelog_2024_non_conf_avg_diff_perc <- gamelog_2024_non_conf_avg_diff %>%
  mutate(across(all_of(metric_avg_cols),
                ~ percentile_rescale(.x, invert = invert_flags[match(cur_column(), metric_avg_cols)]), .names = "{.col}_perc"))  %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####



gamelog_2025_home_avg <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H") %>%
  group_by(Team, Conference) %>%
  dplyr::summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2025_away_avg <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H") %>%
  group_by(Team, Conference) %>%
  dplyr::summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2025_conf_home_avg <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  dplyr::summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2025_conf_away_avg <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  dplyr::summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")

gamelog_2025_non_conf_home_avg <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  dplyr::summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2025_non_conf_home_avg) <- paste0(colnames(gamelog_2025_non_conf_home_avg), "_non_conf_home")

gamelog_2025_non_conf_away_avg <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team, Conference) %>%
  dplyr::summarise(across(all_of(offense_avg_metrics), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"), .groups = "drop")
colnames(gamelog_2025_non_conf_away_avg) <- paste0(colnames(gamelog_2025_non_conf_away_avg), "_non_conf_away")



gamelog_2025_non_conf_avg_intermediary <- full_join(gamelog_2025_non_conf_home_avg, gamelog_2025_non_conf_away_avg, by = c("Team_non_conf_home" = "Team_non_conf_away", "Conference_non_conf_home" = "Conference_non_conf_away")) %>% arrange(Team_non_conf_home)



gamelog_2025_non_conf_avg_intermediary <- gamelog_2025_non_conf_avg_intermediary %>%
  mutate(
    PPP_mean_non_conf_home = coalesce(PPP_mean_non_conf_home, PPP_mean_non_conf_away),
    PPP_mean_non_conf_away = coalesce(PPP_mean_non_conf_away, PPP_mean_non_conf_home),

    `2Pperc_mean_non_conf_home` = coalesce(`2Pperc_mean_non_conf_home`, `2Pperc_mean_non_conf_away`),
    `2Pperc_mean_non_conf_away` = coalesce(`2Pperc_mean_non_conf_away`, `2Pperc_mean_non_conf_home`),

    `3P%_mean_non_conf_home` = coalesce(`3P%_mean_non_conf_home`, `3P%_mean_non_conf_away`),
    `3P%_mean_non_conf_away` = coalesce(`3P%_mean_non_conf_away`, `3P%_mean_non_conf_home`),

    `FT%_mean_non_conf_home` = coalesce(`FT%_mean_non_conf_home`, `FT%_mean_non_conf_away`),
    `FT%_mean_non_conf_away` = coalesce(`FT%_mean_non_conf_away`, `FT%_mean_non_conf_home`),

    FTR_mean_non_conf_home = coalesce(FTR_mean_non_conf_home, FTR_mean_non_conf_away),
    FTR_mean_non_conf_away = coalesce(FTR_mean_non_conf_away, FTR_mean_non_conf_home),

    TOV_perc_mean_non_conf_home = coalesce(TOV_perc_mean_non_conf_home, TOV_perc_mean_non_conf_away),
    TOV_perc_mean_non_conf_away = coalesce(TOV_perc_mean_non_conf_away, TOV_perc_mean_non_conf_home),

    STL_perc_mean_non_conf_home = coalesce(STL_perc_mean_non_conf_home, STL_perc_mean_non_conf_away),
    STL_perc_mean_non_conf_away = coalesce(STL_perc_mean_non_conf_away, STL_perc_mean_non_conf_home),

    `3PRate_mean_non_conf_home` = coalesce(`3PRate_mean_non_conf_home`, `3PRate_mean_non_conf_away`),
    `3PRate_mean_non_conf_away` = coalesce(`3PRate_mean_non_conf_away`, `3PRate_mean_non_conf_home`),

    a_fgm_mean_non_conf_home = coalesce(a_fgm_mean_non_conf_home, a_fgm_mean_non_conf_away),
    a_fgm_mean_non_conf_away = coalesce(a_fgm_mean_non_conf_away, a_fgm_mean_non_conf_home),

    BLK_perc_mean_non_conf_home = coalesce(BLK_perc_mean_non_conf_home, BLK_perc_mean_non_conf_away),
    BLK_perc_mean_non_conf_away = coalesce(BLK_perc_mean_non_conf_away, BLK_perc_mean_non_conf_home),

    ORB_perc_mean_non_conf_home = coalesce(ORB_perc_mean_non_conf_home, ORB_perc_mean_non_conf_away),
    ORB_perc_mean_non_conf_away = coalesce(ORB_perc_mean_non_conf_away, ORB_perc_mean_non_conf_home),

    FL_perc_mean_non_conf_home = coalesce(FL_perc_mean_non_conf_home, FL_perc_mean_non_conf_away),
    FL_perc_mean_non_conf_away = coalesce(FL_perc_mean_non_conf_away, FL_perc_mean_non_conf_home),

    poss_mean_non_conf_home = coalesce(poss_mean_non_conf_home, poss_mean_non_conf_away),
    poss_mean_non_conf_away = coalesce(poss_mean_non_conf_away, poss_mean_non_conf_home)
  )


gamelog_2025_avg_diff <- cbind(gamelog_2025_home_avg[,c(1:2)], gamelog_2025_away_avg[,c(3:15)] - gamelog_2025_home_avg[,c(3:15)])

gamelog_2025_conf_avg_diff <- cbind(gamelog_2025_conf_home_avg[,c(1:2)], gamelog_2025_conf_away_avg[,c(3:15)] - gamelog_2025_conf_home_avg[,c(3:15)])

gamelog_2025_non_conf_avg_diff <- cbind(gamelog_2025_non_conf_avg_intermediary[,c(1:2)], gamelog_2025_non_conf_avg_intermediary[,c(16:28)] - gamelog_2025_non_conf_avg_intermediary[,c(3:15)])
colnames(gamelog_2025_non_conf_avg_diff) <- colnames(gamelog_2025_conf_avg_diff)


metric_avg_cols <- colnames(gamelog_2025_avg_diff)[c(3:15)]

gamelog_2025_avg_diff_perc <- gamelog_2025_avg_diff %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(metric_avg_cols)) {
      col_name <- metric_avg_cols[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0(col_name, "_perc")
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  select(Team, Conference, ends_with("_perc"))


gamelog_2025_conf_avg_diff_perc <- gamelog_2025_conf_avg_diff %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(metric_avg_cols)) {
      col_name <- metric_avg_cols[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0(col_name, "_perc")
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  select(Team, Conference, ends_with("_perc"))

colnames(gamelog_2025_conf_avg_diff_perc) <- paste0(colnames(gamelog_2025_conf_avg_diff_perc), "_conf")


gamelog_2025_non_conf_avg_diff_perc <- gamelog_2025_non_conf_avg_diff %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(metric_avg_cols)) {
      col_name <- metric_avg_cols[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0(col_name, "_perc")
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  select(Team, Conference, ends_with("_perc"))



####
####
####


colnames(gamelog_2025_avg_diff_perc) <- paste0(colnames(gamelog_2025_avg_diff_perc), "_diff")
colnames(gamelog_2025_conf_avg_diff_perc) <- paste0(colnames(gamelog_2025_avg_diff_perc), "_conf_diff")
colnames(gamelog_2025_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2025_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2024_avg_diff_perc) <- paste0(colnames(gamelog_2024_avg_diff_perc), "_diff")
colnames(gamelog_2024_conf_avg_diff_perc) <- paste0(colnames(gamelog_2024_avg_diff_perc), "_conf_diff")
colnames(gamelog_2024_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2024_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2023_avg_diff_perc) <- paste0(colnames(gamelog_2023_avg_diff_perc), "_diff")
colnames(gamelog_2023_conf_avg_diff_perc) <- paste0(colnames(gamelog_2023_avg_diff_perc), "_conf_diff")
colnames(gamelog_2023_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2023_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2022_avg_diff_perc) <- paste0(colnames(gamelog_2022_avg_diff_perc), "_diff")
colnames(gamelog_2022_conf_avg_diff_perc) <- paste0(colnames(gamelog_2022_avg_diff_perc), "_conf_diff")
         colnames(gamelog_2022_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2022_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2021_avg_diff_perc) <- paste0(colnames(gamelog_2021_avg_diff_perc), "_diff")
colnames(gamelog_2021_conf_avg_diff_perc) <- paste0(colnames(gamelog_2021_avg_diff_perc), "_conf_diff")
colnames(gamelog_2021_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2021_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2020_avg_diff_perc) <- paste0(colnames(gamelog_2020_avg_diff_perc), "_diff")
colnames(gamelog_2020_conf_avg_diff_perc) <- paste0(colnames(gamelog_2020_avg_diff_perc), "_conf_diff")
colnames(gamelog_2020_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2020_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2019_avg_diff_perc) <- paste0(colnames(gamelog_2019_avg_diff_perc), "_diff")
colnames(gamelog_2019_conf_avg_diff_perc) <- paste0(colnames(gamelog_2019_avg_diff_perc), "_conf_diff")
colnames(gamelog_2019_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2019_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2018_avg_diff_perc) <- paste0(colnames(gamelog_2018_avg_diff_perc), "_diff")
colnames(gamelog_2018_conf_avg_diff_perc) <- paste0(colnames(gamelog_2018_avg_diff_perc), "_conf_diff")
colnames(gamelog_2018_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2018_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2017_avg_diff_perc) <- paste0(colnames(gamelog_2017_avg_diff_perc), "_diff")
colnames(gamelog_2017_conf_avg_diff_perc) <- paste0(colnames(gamelog_2017_avg_diff_perc), "_conf_diff")
colnames(gamelog_2017_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2017_avg_diff_perc), "_non_conf_diff")


colnames(gamelog_2016_avg_diff_perc) <- paste0(colnames(gamelog_2016_avg_diff_perc), "_diff")
colnames(gamelog_2016_conf_avg_diff_perc) <- paste0(colnames(gamelog_2016_avg_diff_perc), "_conf_diff")
colnames(gamelog_2016_non_conf_avg_diff_perc) <- paste0(colnames(gamelog_2016_avg_diff_perc), "_non_conf_diff")


###
###
###


offense_final_2016 <- cbind(gamelog_2016_offense_stats, gamelog_2016_avg_diff_perc[,-c(1,2)], gamelog_2016_conf_avg_diff_perc[,-c(1,2)], gamelog_2016_non_conf_avg_diff_perc[,-c(1,2)])

offense_final_2017 <- cbind(gamelog_2017_offense_stats, gamelog_2017_avg_diff_perc[,-c(1,2)], gamelog_2017_conf_avg_diff_perc[,-c(1,2)], gamelog_2017_non_conf_avg_diff_perc[,-c(1,2)])

offense_final_2018 <- cbind(gamelog_2018_offense_stats, gamelog_2018_avg_diff_perc[,-c(1,2)], gamelog_2018_conf_avg_diff_perc[,-c(1,2)], gamelog_2018_non_conf_avg_diff_perc[,-c(1,2)])

offense_final_2019 <- cbind(gamelog_2019_offense_stats, gamelog_2019_avg_diff_perc[,-c(1,2)], gamelog_2019_conf_avg_diff_perc[,-c(1,2)], gamelog_2019_non_conf_avg_diff_perc[,-c(1,2)])

offense_final_2020 <- cbind(gamelog_2020_offense_stats, gamelog_2020_avg_diff_perc[,-c(1,2)], gamelog_2020_conf_avg_diff_perc[,-c(1,2)], gamelog_2020_non_conf_avg_diff_perc[,-c(1,2)])


offense_final_2021_intermediary <- cbind(gamelog_2021_offense_stats, gamelog_2021_avg_diff_perc[,-c(1,2)])
offense_final_2021_intermediary[is.na(offense_final_2021_intermediary)] <- 50

offense_final_2021_intermediary_two <- left_join(offense_final_2021_intermediary, gamelog_2021_conf_avg_diff_perc, by = c("Team" = "Team_home_diff_conf_diff", "Conference" = "Conference_home_diff_conf_diff"))

offense_final_2021 <- left_join(offense_final_2021_intermediary_two, gamelog_2021_non_conf_avg_diff_perc, by = c("Team" = "Team_home_diff_non_conf_diff", "Conference" = "Conference_home_diff_non_conf_diff"))

offense_final_2021[is.na(offense_final_2021)] <- 50



offense_final_2022 <- cbind(gamelog_2022_offense_stats, gamelog_2022_avg_diff_perc[,-c(1,2)], gamelog_2022_conf_avg_diff_perc[,-c(1,2)], gamelog_2022_non_conf_avg_diff_perc[,-c(1,2)])


offense_final_2023_intermediary <- cbind(gamelog_2023_offense_stats, gamelog_2023_avg_diff_perc[,-c(1,2)])
offense_final_2023_intermediary[is.na(offense_final_2023_intermediary)] <- 50

offense_final_2023_intermediary_two <- left_join(offense_final_2023_intermediary, gamelog_2023_conf_avg_diff_perc, by = c("Team" = "Team_diff_conf_diff", "Conference" = "Conference_diff_conf_diff"))

offense_final_2023 <- left_join(offense_final_2023_intermediary_two, gamelog_2023_non_conf_avg_diff_perc, by = c("Team" = "Team_diff_non_conf_diff", "Conference" = "Conference_diff_non_conf_diff"))

offense_final_2023[is.na(offense_final_2023)] <- 50


offense_final_2024_intermediary <- cbind(gamelog_2024_offense_stats, gamelog_2024_avg_diff_perc[,-c(1,2)])
offense_final_2024_intermediary[is.na(offense_final_2024_intermediary)] <- 50

offense_final_2024_intermediary_two <- left_join(offense_final_2024_intermediary, gamelog_2024_conf_avg_diff_perc, by = c("Team" = "Team_diff_conf_diff", "Conference" = "Conference_diff_conf_diff"))

offense_final_2024 <- left_join(offense_final_2024_intermediary_two, gamelog_2024_non_conf_avg_diff_perc, by = c("Team" = "Team_diff_non_conf_diff", "Conference" = "Conference_diff_non_conf_diff"))

offense_final_2024[is.na(offense_final_2024)] <- 50


offense_final_2025 <- cbind(gamelog_2025_offense_stats, gamelog_2025_avg_diff_perc[,-c(1,2)], gamelog_2025_conf_avg_diff_perc[,-c(1,2)], gamelog_2025_non_conf_avg_diff_perc[,-c(1,2)])


### THERE ARE ACTUALLY A LOT OF NAs. MOSTLY SHIT SCHOOLS THAT PLAY NON_CONF EXCLUSIVELY AWAY FOR THE MONEY
### GOTTA FIND A WAY TO DEAL WITH THAT FROM THE JUMP. THIS SUCKS MAN LOL.
