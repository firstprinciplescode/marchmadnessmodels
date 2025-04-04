# HOME PERC
# NOT HOME PERC
# CONF, HOME PERC
# CONF, AWAY PERC

# TABULATE BY CONFERENCE

### PERCENTILES WITH ...
# PPP (100 GOOD, 0 LOW)
# 2P% (100 GOOD, 0 LOW)
# 3P% (100 GOOD, 0 LOW)
# FT% (100 GOOD, 0 LOW)
# FTR (100 GOOD, 0 LOW)
# TOV% (100 LOW, 0 HIGH)
# STL% (100 LOW, 0 HIGH)
# 3PR (100 GOOD, 0 LOW)
# A_FGM (100 GOOD, 0 LOW)
# BLK% (100 LOW, 0 HIGH)
# ORB% (100 GOOD, 0 LOW)
# FL% (100 GOOD, 0 LOW)

## AND WITH THE OPPOSITION / DEFENSE TOO - REVERSE ORDER FROM ABOVE


library(readxl)
library(openxlsx)
library(tidyverse)
library(scales)
library(sqldf)

'%ni%' <- Negate('%in%')

# Function to rescale percentiles (invert for some metrics)
percentile_rescale <- function(x, invert = FALSE) {
  if (invert) {
    return(rescale(x, to = c(100, 0), from = range(x, na.rm = TRUE))) # Inverted scale
  } else {
    return(rescale(x, to = c(0, 100), from = range(x, na.rm = TRUE))) # Normal scale
  }
}

# Define offense metrics and inversion flags
offense_metrics <- c("PPP", "2Pperc", "3P%", "FT%", "FTR",
                     "TOV_perc", "STL_perc", "3PRate", "a_fgm",
                     "BLK_perc", "ORB_perc", "FL_perc", "poss")
invert_flags <- c(FALSE, FALSE, FALSE, FALSE, FALSE,
                  TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE) # TRUE for inverted metrics

# Filter out postseason games & keep only home games
gamelog_2016_home <- gamelog_2016 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_away <- gamelog_2016 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_conf_home <- gamelog_2016 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_conf_away <- gamelog_2016 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_non_conf_home <- gamelog_2016 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_non_conf_away <- gamelog_2016 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()

# Merge back with original dataset to preserve all rows
gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

colnames(gamelog_2016)

gamelog_2016_opponent_stats <- gamelog_2016 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2016_opponent_stats <- gamelog_2016_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


invert_flags_def <- c(FALSE, FALSE, FALSE, FALSE, FALSE,
                      TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, F) # TRUE for inverted metrics

# Filter out postseason games & keep only home games
gamelog_2016_home_def <- gamelog_2016 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_away_def <- gamelog_2016 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_conf_home_def <- gamelog_2016 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_conf_away_def <- gamelog_2016 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_non_conf_home_def <- gamelog_2016 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2016_non_conf_away_def <- gamelog_2016 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2016 <- left_join(gamelog_2016, gamelog_2016_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

colnames(gamelog_2016)

gamelog_2016_offense_stats <- gamelog_2016 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2016_offense_stats <- gamelog_2016_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2017
####
####



# Filter out postseason games & keep only home games
gamelog_2017_home <- gamelog_2017 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_away <- gamelog_2017 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_conf_home <- gamelog_2017 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_conf_away <- gamelog_2017 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_non_conf_home <- gamelog_2017 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_non_conf_away <- gamelog_2017 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

colnames(gamelog_2017)

gamelog_2017_opponent_stats <- gamelog_2017 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2017_opponent_stats <- gamelog_2017_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2017_home_def <- gamelog_2017 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_away_def <- gamelog_2017 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_conf_home_def <- gamelog_2017 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_conf_away_def <- gamelog_2017 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_non_conf_home_def <- gamelog_2017 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2017_non_conf_away_def <- gamelog_2017 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2017 <- left_join(gamelog_2017, gamelog_2017_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2017_offense_stats <- gamelog_2017 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2017_offense_stats <- gamelog_2017_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2018
####
####



# Filter out postseason games & keep only home games
gamelog_2018_home <- gamelog_2018 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_away <- gamelog_2018 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_conf_home <- gamelog_2018 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_conf_away <- gamelog_2018 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_non_conf_home <- gamelog_2018 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_non_conf_away <- gamelog_2018 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))
# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2018_opponent_stats <- gamelog_2018 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2018_opponent_stats <- gamelog_2018_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2018_home_def <- gamelog_2018 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_away_def <- gamelog_2018 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_conf_home_def <- gamelog_2018 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_conf_away_def <- gamelog_2018 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_non_conf_home_def <- gamelog_2018 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2018_non_conf_away_def <- gamelog_2018 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()

# Merge back with original dataset to preserve all rows
gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2018 <- left_join(gamelog_2018, gamelog_2018_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2018_offense_stats <- gamelog_2018 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2018_offense_stats <- gamelog_2018_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2019
####
####



# Filter out postseason games & keep only home games
gamelog_2019_home <- gamelog_2019 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_away <- gamelog_2019 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_conf_home <- gamelog_2019 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_conf_away <- gamelog_2019 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_non_conf_home <- gamelog_2019 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_non_conf_away <- gamelog_2019 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2019_opponent_stats <- gamelog_2019 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2019_opponent_stats <- gamelog_2019_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2019_home_def <- gamelog_2019 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_away_def <- gamelog_2019 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_conf_home_def <- gamelog_2019 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_conf_away_def <- gamelog_2019 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_non_conf_home_def <- gamelog_2019 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2019_non_conf_away_def <- gamelog_2019 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2019 <- left_join(gamelog_2019, gamelog_2019_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2019_offense_stats <- gamelog_2019 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2019_offense_stats <- gamelog_2019_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2020
####
####



# Filter out postseason games & keep only home games
gamelog_2020_home <- gamelog_2020 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_away <- gamelog_2020 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_conf_home <- gamelog_2020 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_conf_away <- gamelog_2020 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_non_conf_home <- gamelog_2020 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_non_conf_away <- gamelog_2020 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2020_opponent_stats <- gamelog_2020 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2020_opponent_stats <- gamelog_2020_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2020_home_def <- gamelog_2020 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_away_def <- gamelog_2020 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_conf_home_def <- gamelog_2020 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_conf_away_def <- gamelog_2020 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_non_conf_home_def <- gamelog_2020 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2020_non_conf_away_def <- gamelog_2020 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2020 <- left_join(gamelog_2020, gamelog_2020_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2020_offense_stats <- gamelog_2020 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2020_offense_stats <- gamelog_2020_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2021
####
####



# Filter out postseason games & keep only home games
gamelog_2021_home <- gamelog_2021 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_away <- gamelog_2021 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_conf_home <- gamelog_2021 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_conf_away <- gamelog_2021 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_non_conf_home <- gamelog_2021 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_non_conf_away <- gamelog_2021 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2021_opponent_stats <- gamelog_2021 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2021_opponent_stats <- gamelog_2021_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2021_home_def <- gamelog_2021 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_away_def <- gamelog_2021 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_conf_home_def <- gamelog_2021 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_conf_away_def <- gamelog_2021 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_non_conf_home_def <- gamelog_2021 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2021_non_conf_away_def <- gamelog_2021 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2021 <- left_join(gamelog_2021, gamelog_2021_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2021_offense_stats <- gamelog_2021 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2021_offense_stats <- gamelog_2021_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2022
####
####



# Filter out postseason games & keep only home games
gamelog_2022_home <- gamelog_2022 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_away <- gamelog_2022 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_conf_home <- gamelog_2022 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_conf_away <- gamelog_2022 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_non_conf_home <- gamelog_2022 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_non_conf_away <- gamelog_2022 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2022_opponent_stats <- gamelog_2022 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2022_opponent_stats <- gamelog_2022_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2022_home_def <- gamelog_2022 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_away_def <- gamelog_2022 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_conf_home_def <- gamelog_2022 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_conf_away_def <- gamelog_2022 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_non_conf_home_def <- gamelog_2022 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2022_non_conf_away_def <- gamelog_2022 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2022 <- left_join(gamelog_2022, gamelog_2022_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2022_offense_stats <- gamelog_2022 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2022_offense_stats <- gamelog_2022_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2023
####
####



# Filter out postseason games & keep only home games
gamelog_2023_home <- gamelog_2023 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_away <- gamelog_2023 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_conf_home <- gamelog_2023 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_conf_away <- gamelog_2023 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_non_conf_home <- gamelog_2023 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_non_conf_away <- gamelog_2023 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2023_opponent_stats <- gamelog_2023 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2023_opponent_stats <- gamelog_2023_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2023_home_def <- gamelog_2023 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_away_def <- gamelog_2023 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_conf_home_def <- gamelog_2023 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_conf_away_def <- gamelog_2023 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_non_conf_home_def <- gamelog_2023 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2023_non_conf_away_def <- gamelog_2023 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2023 <- left_join(gamelog_2023, gamelog_2023_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2023_offense_stats <- gamelog_2023 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2023_offense_stats <- gamelog_2023_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2024
####
####



# Filter out postseason games & keep only home games
gamelog_2024_home <- gamelog_2024 %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_away <- gamelog_2024 %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_conf_home <- gamelog_2024 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_conf_away <- gamelog_2024 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_non_conf_home <- gamelog_2024 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_non_conf_away <- gamelog_2024 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags[which(offense_metrics == cur_column())]),
                .names = "non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2024_opponent_stats <- gamelog_2024 %>% group_by(Opp, OppConference) %>%
  summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2024_opponent_stats <- gamelog_2024_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


# Filter out postseason games & keep only home games
gamelog_2024_home_def <- gamelog_2024 %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_away_def <- gamelog_2024 %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_conf_home_def <- gamelog_2024 %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_conf_away_def <- gamelog_2024 %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_conf_away_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_non_conf_home_def <- gamelog_2024 %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_home_perc_{.col}")) %>%
  ungroup()

# Filter out postseason games & keep only home games
gamelog_2024_non_conf_away_def <- gamelog_2024 %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  mutate(across(all_of(offense_metrics),
                ~ percentile_rescale(.x, invert = invert_flags_def[which(offense_metrics == cur_column())]),
                .names = "def_non_conf_away_perc_{.col}")) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2024 <- left_join(gamelog_2024, gamelog_2024_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2024_offense_stats <- gamelog_2024 %>% group_by(Team, Conference) %>%
  summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2024_offense_stats <- gamelog_2024_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )


####
####
#### 2025
####
####


gamelog_2025_home <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H") %>%
  group_by(Team) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("home_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_away <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H") %>%
  group_by(Team) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("away_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_conf_home <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("conf_home_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_conf_away <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("conf_away_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_non_conf_home <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("non_conf_home_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_non_conf_away <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Team) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("non_conf_away_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_home %>%
                            select(G, Team, Opp, starts_with("home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_away %>%
                            select(G, Team, Opp, starts_with("away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_conf_home %>%
                            select(G, Team, Opp, starts_with("conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_conf_away %>%
                            select(G, Team, Opp, starts_with("conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_non_conf_home %>%
                            select(G, Team, Opp, starts_with("non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_non_conf_away %>%
                            select(G, Team, Opp, starts_with("non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2025_opponent_stats <- gamelog_2025 %>% group_by(Opp, OppConference) %>%
  dplyr::summarise(across(70:148, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))

gamelog_2025_opponent_stats <- gamelog_2025_opponent_stats %>%
  mutate(
    # --- Home / Away ---
    mean_home_perc_PPP = coalesce(mean_home_perc_PPP, mean_away_perc_PPP),
    mean_away_perc_PPP = coalesce(mean_away_perc_PPP, mean_home_perc_PPP),

    mean_home_perc_2Pperc = coalesce(mean_home_perc_2Pperc, mean_away_perc_2Pperc),
    mean_away_perc_2Pperc = coalesce(mean_away_perc_2Pperc, mean_home_perc_2Pperc),

    `mean_home_perc_3P%` = coalesce(`mean_home_perc_3P%`, `mean_away_perc_3P%`),
    `mean_away_perc_3P%` = coalesce(`mean_away_perc_3P%`, `mean_home_perc_3P%`),

    `mean_home_perc_FT%` = coalesce(`mean_home_perc_FT%`, `mean_away_perc_FT%`),
    `mean_away_perc_FT%` = coalesce(`mean_away_perc_FT%`, `mean_home_perc_FT%`),

    mean_home_perc_FTR = coalesce(mean_home_perc_FTR, mean_away_perc_FTR),
    mean_away_perc_FTR = coalesce(mean_away_perc_FTR, mean_home_perc_FTR),

    mean_home_perc_TOV_perc = coalesce(mean_home_perc_TOV_perc, mean_away_perc_TOV_perc),
    mean_away_perc_TOV_perc = coalesce(mean_away_perc_TOV_perc, mean_home_perc_TOV_perc),

    mean_home_perc_STL_perc = coalesce(mean_home_perc_STL_perc, mean_away_perc_STL_perc),
    mean_away_perc_STL_perc = coalesce(mean_away_perc_STL_perc, mean_home_perc_STL_perc),

    mean_home_perc_3PRate = coalesce(mean_home_perc_3PRate, mean_away_perc_3PRate),
    mean_away_perc_3PRate = coalesce(mean_away_perc_3PRate, mean_home_perc_3PRate),

    mean_home_perc_a_fgm = coalesce(mean_home_perc_a_fgm, mean_away_perc_a_fgm),
    mean_away_perc_a_fgm = coalesce(mean_away_perc_a_fgm, mean_home_perc_a_fgm),

    mean_home_perc_BLK_perc = coalesce(mean_home_perc_BLK_perc, mean_away_perc_BLK_perc),
    mean_away_perc_BLK_perc = coalesce(mean_away_perc_BLK_perc, mean_home_perc_BLK_perc),

    mean_home_perc_ORB_perc = coalesce(mean_home_perc_ORB_perc, mean_away_perc_ORB_perc),
    mean_away_perc_ORB_perc = coalesce(mean_away_perc_ORB_perc, mean_home_perc_ORB_perc),

    mean_home_perc_FL_perc = coalesce(mean_home_perc_FL_perc, mean_away_perc_FL_perc),
    mean_away_perc_FL_perc = coalesce(mean_away_perc_FL_perc, mean_home_perc_FL_perc),

    mean_home_perc_poss = coalesce(mean_home_perc_poss, mean_away_perc_poss),
    mean_away_perc_poss = coalesce(mean_away_perc_poss, mean_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_conf_home_perc_PPP = coalesce(mean_conf_home_perc_PPP, mean_conf_away_perc_PPP),
    mean_conf_away_perc_PPP = coalesce(mean_conf_away_perc_PPP, mean_conf_home_perc_PPP),

    mean_conf_home_perc_2Pperc = coalesce(mean_conf_home_perc_2Pperc, mean_conf_away_perc_2Pperc),
    mean_conf_away_perc_2Pperc = coalesce(mean_conf_away_perc_2Pperc, mean_conf_home_perc_2Pperc),

    `mean_conf_home_perc_3P%` = coalesce(`mean_conf_home_perc_3P%`, `mean_conf_away_perc_3P%`),
    `mean_conf_away_perc_3P%` = coalesce(`mean_conf_away_perc_3P%`, `mean_conf_home_perc_3P%`),

    `mean_conf_home_perc_FT%` = coalesce(`mean_conf_home_perc_FT%`, `mean_conf_away_perc_FT%`),
    `mean_conf_away_perc_FT%` = coalesce(`mean_conf_away_perc_FT%`, `mean_conf_home_perc_FT%`),

    mean_conf_home_perc_FTR = coalesce(mean_conf_home_perc_FTR, mean_conf_away_perc_FTR),
    mean_conf_away_perc_FTR = coalesce(mean_conf_away_perc_FTR, mean_conf_home_perc_FTR),

    mean_conf_home_perc_TOV_perc = coalesce(mean_conf_home_perc_TOV_perc, mean_conf_away_perc_TOV_perc),
    mean_conf_away_perc_TOV_perc = coalesce(mean_conf_away_perc_TOV_perc, mean_conf_home_perc_TOV_perc),

    mean_conf_home_perc_STL_perc = coalesce(mean_conf_home_perc_STL_perc, mean_conf_away_perc_STL_perc),
    mean_conf_away_perc_STL_perc = coalesce(mean_conf_away_perc_STL_perc, mean_conf_home_perc_STL_perc),

    mean_conf_home_perc_3PRate = coalesce(mean_conf_home_perc_3PRate, mean_conf_away_perc_3PRate),
    mean_conf_away_perc_3PRate = coalesce(mean_conf_away_perc_3PRate, mean_conf_home_perc_3PRate),

    mean_conf_home_perc_a_fgm = coalesce(mean_conf_home_perc_a_fgm, mean_conf_away_perc_a_fgm),
    mean_conf_away_perc_a_fgm = coalesce(mean_conf_away_perc_a_fgm, mean_conf_home_perc_a_fgm),

    mean_conf_home_perc_BLK_perc = coalesce(mean_conf_home_perc_BLK_perc, mean_conf_away_perc_BLK_perc),
    mean_conf_away_perc_BLK_perc = coalesce(mean_conf_away_perc_BLK_perc, mean_conf_home_perc_BLK_perc),

    mean_conf_home_perc_ORB_perc = coalesce(mean_conf_home_perc_ORB_perc, mean_conf_away_perc_ORB_perc),
    mean_conf_away_perc_ORB_perc = coalesce(mean_conf_away_perc_ORB_perc, mean_conf_home_perc_ORB_perc),

    mean_conf_home_perc_FL_perc = coalesce(mean_conf_home_perc_FL_perc, mean_conf_away_perc_FL_perc),
    mean_conf_away_perc_FL_perc = coalesce(mean_conf_away_perc_FL_perc, mean_conf_home_perc_FL_perc),

    mean_conf_home_perc_poss = coalesce(mean_conf_home_perc_poss, mean_conf_away_perc_poss),
    mean_conf_away_perc_poss = coalesce(mean_conf_away_perc_poss, mean_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_non_conf_home_perc_PPP = coalesce(mean_non_conf_home_perc_PPP, mean_non_conf_away_perc_PPP),
    mean_non_conf_away_perc_PPP = coalesce(mean_non_conf_away_perc_PPP, mean_non_conf_home_perc_PPP),

    mean_non_conf_home_perc_2Pperc = coalesce(mean_non_conf_home_perc_2Pperc, mean_non_conf_away_perc_2Pperc),
    mean_non_conf_away_perc_2Pperc = coalesce(mean_non_conf_away_perc_2Pperc, mean_non_conf_home_perc_2Pperc),

    `mean_non_conf_home_perc_3P%` = coalesce(`mean_non_conf_home_perc_3P%`, `mean_non_conf_away_perc_3P%`),
    `mean_non_conf_away_perc_3P%` = coalesce(`mean_non_conf_away_perc_3P%`, `mean_non_conf_home_perc_3P%`),

    `mean_non_conf_home_perc_FT%` = coalesce(`mean_non_conf_home_perc_FT%`, `mean_non_conf_away_perc_FT%`),
    `mean_non_conf_away_perc_FT%` = coalesce(`mean_non_conf_away_perc_FT%`, `mean_non_conf_home_perc_FT%`),

    mean_non_conf_home_perc_FTR = coalesce(mean_non_conf_home_perc_FTR, mean_non_conf_away_perc_FTR),
    mean_non_conf_away_perc_FTR = coalesce(mean_non_conf_away_perc_FTR, mean_non_conf_home_perc_FTR),

    mean_non_conf_home_perc_TOV_perc = coalesce(mean_non_conf_home_perc_TOV_perc, mean_non_conf_away_perc_TOV_perc),
    mean_non_conf_away_perc_TOV_perc = coalesce(mean_non_conf_away_perc_TOV_perc, mean_non_conf_home_perc_TOV_perc),

    mean_non_conf_home_perc_STL_perc = coalesce(mean_non_conf_home_perc_STL_perc, mean_non_conf_away_perc_STL_perc),
    mean_non_conf_away_perc_STL_perc = coalesce(mean_non_conf_away_perc_STL_perc, mean_non_conf_home_perc_STL_perc),

    mean_non_conf_home_perc_3PRate = coalesce(mean_non_conf_home_perc_3PRate, mean_non_conf_away_perc_3PRate),
    mean_non_conf_away_perc_3PRate = coalesce(mean_non_conf_away_perc_3PRate, mean_non_conf_home_perc_3PRate),

    mean_non_conf_home_perc_a_fgm = coalesce(mean_non_conf_home_perc_a_fgm, mean_non_conf_away_perc_a_fgm),
    mean_non_conf_away_perc_a_fgm = coalesce(mean_non_conf_away_perc_a_fgm, mean_non_conf_home_perc_a_fgm),

    mean_non_conf_home_perc_BLK_perc = coalesce(mean_non_conf_home_perc_BLK_perc, mean_non_conf_away_perc_BLK_perc),
    mean_non_conf_away_perc_BLK_perc = coalesce(mean_non_conf_away_perc_BLK_perc, mean_non_conf_home_perc_BLK_perc),

    mean_non_conf_home_perc_ORB_perc = coalesce(mean_non_conf_home_perc_ORB_perc, mean_non_conf_away_perc_ORB_perc),
    mean_non_conf_away_perc_ORB_perc = coalesce(mean_non_conf_away_perc_ORB_perc, mean_non_conf_home_perc_ORB_perc),

    mean_non_conf_home_perc_FL_perc = coalesce(mean_non_conf_home_perc_FL_perc, mean_non_conf_away_perc_FL_perc),
    mean_non_conf_away_perc_FL_perc = coalesce(mean_non_conf_away_perc_FL_perc, mean_non_conf_home_perc_FL_perc),

    mean_non_conf_home_perc_poss = coalesce(mean_non_conf_home_perc_poss, mean_non_conf_away_perc_poss),
    mean_non_conf_away_perc_poss = coalesce(mean_non_conf_away_perc_poss, mean_non_conf_home_perc_poss)
  )


##
##
##


gamelog_2025_home_def <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H") %>%
  group_by(Opp) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("def_home_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_away_def <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H") %>%
  group_by(Opp) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("def_away_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_conf_home_def <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("def_conf_home_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_conf_away_def <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H", GameType %in% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("def_conf_away_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_non_conf_home_def <- gamelog_2025 %>% ungroup() %>%
  filter(Location == "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("def_non_conf_home_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


gamelog_2025_non_conf_away_def <- gamelog_2025 %>% ungroup() %>%
  filter(Location != "H", GameType %ni% c("CTOURN", "REG (Conf)")) %>%
  group_by(Opp) %>%
  group_modify(~ {
    df <- .x
    for (i in seq_along(offense_metrics)) {
      col_name <- offense_metrics[i]
      inv_flag <- invert_flags[i]
      new_col_name <- paste0("def_non_conf_away_perc_", col_name)
      df[[new_col_name]] <- percentile_rescale(df[[col_name]], invert = inv_flag)
    }
    df
  }) %>%
  ungroup()


# Merge back with original dataset to preserve all rows
gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_home_def %>%
                            select(G, Team, Opp, starts_with("def_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_away_def %>%
                            select(G, Team, Opp, starts_with("def_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_non_conf_home_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_home_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

gamelog_2025 <- left_join(gamelog_2025, gamelog_2025_non_conf_away_def %>%
                            select(G, Team, Opp, starts_with("def_non_conf_away_perc_")), by = c("G" = "G", "Team" = "Team", "Opp" = "Opp"))

# WITH THESE COLUMNS ABOVE - CAN ONLY USE THE OPP WITH THEM. THESE ARE FOR THE DEFENSE

gamelog_2025_offense_stats <- gamelog_2025 %>% group_by(Team, Conference) %>%
  dplyr::summarise(across(149:226, ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))


gamelog_2025_offense_stats <- gamelog_2025_offense_stats %>%
  mutate(
    # --- Home / Away ---
    mean_def_home_perc_PPP = coalesce(mean_def_home_perc_PPP, mean_def_away_perc_PPP),
    mean_def_away_perc_PPP = coalesce(mean_def_away_perc_PPP, mean_def_home_perc_PPP),

    mean_def_home_perc_2Pperc = coalesce(mean_def_home_perc_2Pperc, mean_def_away_perc_2Pperc),
    mean_def_away_perc_2Pperc = coalesce(mean_def_away_perc_2Pperc, mean_def_home_perc_2Pperc),

    `mean_def_home_perc_3P%` = coalesce(`mean_def_home_perc_3P%`, `mean_def_away_perc_3P%`),
    `mean_def_away_perc_3P%` = coalesce(`mean_def_away_perc_3P%`, `mean_def_home_perc_3P%`),

    `mean_def_home_perc_FT%` = coalesce(`mean_def_home_perc_FT%`, `mean_def_away_perc_FT%`),
    `mean_def_away_perc_FT%` = coalesce(`mean_def_away_perc_FT%`, `mean_def_home_perc_FT%`),

    mean_def_home_perc_FTR = coalesce(mean_def_home_perc_FTR, mean_def_away_perc_FTR),
    mean_def_away_perc_FTR = coalesce(mean_def_away_perc_FTR, mean_def_home_perc_FTR),

    mean_def_home_perc_TOV_perc = coalesce(mean_def_home_perc_TOV_perc, mean_def_away_perc_TOV_perc),
    mean_def_away_perc_TOV_perc = coalesce(mean_def_away_perc_TOV_perc, mean_def_home_perc_TOV_perc),

    mean_def_home_perc_STL_perc = coalesce(mean_def_home_perc_STL_perc, mean_def_away_perc_STL_perc),
    mean_def_away_perc_STL_perc = coalesce(mean_def_away_perc_STL_perc, mean_def_home_perc_STL_perc),

    mean_def_home_perc_3PRate = coalesce(mean_def_home_perc_3PRate, mean_def_away_perc_3PRate),
    mean_def_away_perc_3PRate = coalesce(mean_def_away_perc_3PRate, mean_def_home_perc_3PRate),

    mean_def_home_perc_a_fgm = coalesce(mean_def_home_perc_a_fgm, mean_def_away_perc_a_fgm),
    mean_def_away_perc_a_fgm = coalesce(mean_def_away_perc_a_fgm, mean_def_home_perc_a_fgm),

    mean_def_home_perc_BLK_perc = coalesce(mean_def_home_perc_BLK_perc, mean_def_away_perc_BLK_perc),
    mean_def_away_perc_BLK_perc = coalesce(mean_def_away_perc_BLK_perc, mean_def_home_perc_BLK_perc),

    mean_def_home_perc_ORB_perc = coalesce(mean_def_home_perc_ORB_perc, mean_def_away_perc_ORB_perc),
    mean_def_away_perc_ORB_perc = coalesce(mean_def_away_perc_ORB_perc, mean_def_home_perc_ORB_perc),

    mean_def_home_perc_FL_perc = coalesce(mean_def_home_perc_FL_perc, mean_def_away_perc_FL_perc),
    mean_def_away_perc_FL_perc = coalesce(mean_def_away_perc_FL_perc, mean_def_home_perc_FL_perc),

    mean_def_home_perc_poss = coalesce(mean_def_home_perc_poss, mean_def_away_perc_poss),
    mean_def_away_perc_poss = coalesce(mean_def_away_perc_poss, mean_def_home_perc_poss),


    # --- Conf Home / Conf Away ---
    mean_def_conf_home_perc_PPP = coalesce(mean_def_conf_home_perc_PPP, mean_def_conf_away_perc_PPP),
    mean_def_conf_away_perc_PPP = coalesce(mean_def_conf_away_perc_PPP, mean_def_conf_home_perc_PPP),

    mean_def_conf_home_perc_2Pperc = coalesce(mean_def_conf_home_perc_2Pperc, mean_def_conf_away_perc_2Pperc),
    mean_def_conf_away_perc_2Pperc = coalesce(mean_def_conf_away_perc_2Pperc, mean_def_conf_home_perc_2Pperc),

    `mean_def_conf_home_perc_3P%` = coalesce(`mean_def_conf_home_perc_3P%`, `mean_def_conf_away_perc_3P%`),
    `mean_def_conf_away_perc_3P%` = coalesce(`mean_def_conf_away_perc_3P%`, `mean_def_conf_home_perc_3P%`),

    `mean_def_conf_home_perc_FT%` = coalesce(`mean_def_conf_home_perc_FT%`, `mean_def_conf_away_perc_FT%`),
    `mean_def_conf_away_perc_FT%` = coalesce(`mean_def_conf_away_perc_FT%`, `mean_def_conf_home_perc_FT%`),

    mean_def_conf_home_perc_FTR = coalesce(mean_def_conf_home_perc_FTR, mean_def_conf_away_perc_FTR),
    mean_def_conf_away_perc_FTR = coalesce(mean_def_conf_away_perc_FTR, mean_def_conf_home_perc_FTR),

    mean_def_conf_home_perc_TOV_perc = coalesce(mean_def_conf_home_perc_TOV_perc, mean_def_conf_away_perc_TOV_perc),
    mean_def_conf_away_perc_TOV_perc = coalesce(mean_def_conf_away_perc_TOV_perc, mean_def_conf_home_perc_TOV_perc),

    mean_def_conf_home_perc_STL_perc = coalesce(mean_def_conf_home_perc_STL_perc, mean_def_conf_away_perc_STL_perc),
    mean_def_conf_away_perc_STL_perc = coalesce(mean_def_conf_away_perc_STL_perc, mean_def_conf_home_perc_STL_perc),

    mean_def_conf_home_perc_3PRate = coalesce(mean_def_conf_home_perc_3PRate, mean_def_conf_away_perc_3PRate),
    mean_def_conf_away_perc_3PRate = coalesce(mean_def_conf_away_perc_3PRate, mean_def_conf_home_perc_3PRate),

    mean_def_conf_home_perc_a_fgm = coalesce(mean_def_conf_home_perc_a_fgm, mean_def_conf_away_perc_a_fgm),
    mean_def_conf_away_perc_a_fgm = coalesce(mean_def_conf_away_perc_a_fgm, mean_def_conf_home_perc_a_fgm),

    mean_def_conf_home_perc_BLK_perc = coalesce(mean_def_conf_home_perc_BLK_perc, mean_def_conf_away_perc_BLK_perc),
    mean_def_conf_away_perc_BLK_perc = coalesce(mean_def_conf_away_perc_BLK_perc, mean_def_conf_home_perc_BLK_perc),

    mean_def_conf_home_perc_ORB_perc = coalesce(mean_def_conf_home_perc_ORB_perc, mean_def_conf_away_perc_ORB_perc),
    mean_def_conf_away_perc_ORB_perc = coalesce(mean_def_conf_away_perc_ORB_perc, mean_def_conf_home_perc_ORB_perc),

    mean_def_conf_home_perc_FL_perc = coalesce(mean_def_conf_home_perc_FL_perc, mean_def_conf_away_perc_FL_perc),
    mean_def_conf_away_perc_FL_perc = coalesce(mean_def_conf_away_perc_FL_perc, mean_def_conf_home_perc_FL_perc),

    mean_def_conf_home_perc_poss = coalesce(mean_def_conf_home_perc_poss, mean_def_conf_away_perc_poss),
    mean_def_conf_away_perc_poss = coalesce(mean_def_conf_away_perc_poss, mean_def_conf_home_perc_poss),


    # --- Non-Conf Home / Non-Conf Away ---
    mean_def_non_conf_home_perc_PPP = coalesce(mean_def_non_conf_home_perc_PPP, mean_def_non_conf_away_perc_PPP),
    mean_def_non_conf_away_perc_PPP = coalesce(mean_def_non_conf_away_perc_PPP, mean_def_non_conf_home_perc_PPP),

    mean_def_non_conf_home_perc_2Pperc = coalesce(mean_def_non_conf_home_perc_2Pperc, mean_def_non_conf_away_perc_2Pperc),
    mean_def_non_conf_away_perc_2Pperc = coalesce(mean_def_non_conf_away_perc_2Pperc, mean_def_non_conf_home_perc_2Pperc),

    `mean_def_non_conf_home_perc_3P%` = coalesce(`mean_def_non_conf_home_perc_3P%`, `mean_def_non_conf_away_perc_3P%`),
    `mean_def_non_conf_away_perc_3P%` = coalesce(`mean_def_non_conf_away_perc_3P%`, `mean_def_non_conf_home_perc_3P%`),

    `mean_def_non_conf_home_perc_FT%` = coalesce(`mean_def_non_conf_home_perc_FT%`, `mean_def_non_conf_away_perc_FT%`),
    `mean_def_non_conf_away_perc_FT%` = coalesce(`mean_def_non_conf_away_perc_FT%`, `mean_def_non_conf_home_perc_FT%`),

    mean_def_non_conf_home_perc_FTR = coalesce(mean_def_non_conf_home_perc_FTR, mean_def_non_conf_away_perc_FTR),
    mean_def_non_conf_away_perc_FTR = coalesce(mean_def_non_conf_away_perc_FTR, mean_def_non_conf_home_perc_FTR),

    mean_def_non_conf_home_perc_TOV_perc = coalesce(mean_def_non_conf_home_perc_TOV_perc, mean_def_non_conf_away_perc_TOV_perc),
    mean_def_non_conf_away_perc_TOV_perc = coalesce(mean_def_non_conf_away_perc_TOV_perc, mean_def_non_conf_home_perc_TOV_perc),

    mean_def_non_conf_home_perc_STL_perc = coalesce(mean_def_non_conf_home_perc_STL_perc, mean_def_non_conf_away_perc_STL_perc),
    mean_def_non_conf_away_perc_STL_perc = coalesce(mean_def_non_conf_away_perc_STL_perc, mean_def_non_conf_home_perc_STL_perc),

    mean_def_non_conf_home_perc_3PRate = coalesce(mean_def_non_conf_home_perc_3PRate, mean_def_non_conf_away_perc_3PRate),
    mean_def_non_conf_away_perc_3PRate = coalesce(mean_def_non_conf_away_perc_3PRate, mean_def_non_conf_home_perc_3PRate),

    mean_def_non_conf_home_perc_a_fgm = coalesce(mean_def_non_conf_home_perc_a_fgm, mean_def_non_conf_away_perc_a_fgm),
    mean_def_non_conf_away_perc_a_fgm = coalesce(mean_def_non_conf_away_perc_a_fgm, mean_def_non_conf_home_perc_a_fgm),

    mean_def_non_conf_home_perc_BLK_perc = coalesce(mean_def_non_conf_home_perc_BLK_perc, mean_def_non_conf_away_perc_BLK_perc),
    mean_def_non_conf_away_perc_BLK_perc = coalesce(mean_def_non_conf_away_perc_BLK_perc, mean_def_non_conf_home_perc_BLK_perc),

    mean_def_non_conf_home_perc_ORB_perc = coalesce(mean_def_non_conf_home_perc_ORB_perc, mean_def_non_conf_away_perc_ORB_perc),
    mean_def_non_conf_away_perc_ORB_perc = coalesce(mean_def_non_conf_away_perc_ORB_perc, mean_def_non_conf_home_perc_ORB_perc),

    mean_def_non_conf_home_perc_FL_perc = coalesce(mean_def_non_conf_home_perc_FL_perc, mean_def_non_conf_away_perc_FL_perc),
    mean_def_non_conf_away_perc_FL_perc = coalesce(mean_def_non_conf_away_perc_FL_perc, mean_def_non_conf_home_perc_FL_perc),

    mean_def_non_conf_home_perc_poss = coalesce(mean_def_non_conf_home_perc_poss, mean_def_non_conf_away_perc_poss),
    mean_def_non_conf_away_perc_poss = coalesce(mean_def_non_conf_away_perc_poss, mean_def_non_conf_home_perc_poss)
  )
