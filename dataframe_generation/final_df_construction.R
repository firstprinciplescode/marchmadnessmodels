gamelog_2016_base <- gamelog_2016 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
  mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
         Location = ifelse(Location == "H", "H", "A"),
         OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
  mutate(
    game_id = case_when(
      Location == "H" ~ paste0(Team, "-", Date),
      Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
      Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                     paste0(Team, "-", Opp, "-", Date),
                                                     paste0(Opp, "-", Team, "-", Date)),
      TRUE ~ NA_character_
    )
  )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2016

gamelog_2016_base_inter_one <- left_join(gamelog_2016_base, offense_final_2016, by = c("Team", "Conference"))
colnames(gamelog_2016_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2016_base_inter_one)[21:137])

## DEF
# defense_final_2016


gamelog_2016_base_inter_two <- left_join(gamelog_2016_base_inter_one, defense_final_2016, by = c("Opp", "OppConference"))
colnames(gamelog_2016_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2016_base_inter_two)[c(138:254)])


## OFF
# conference_final_2016
# non_conference_final_2016


gamelog_2016_base_inter_three <- left_join(gamelog_2016_base_inter_two, conference_final_2016, by = "Conference")
colnames(gamelog_2016_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2016_base_inter_three)[c(255:293)])


gamelog_2016_base_inter_four <- left_join(gamelog_2016_base_inter_three, non_conference_final_2016, by = "Conference")

colnames(gamelog_2016_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2016_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2016_base_inter_five <- left_join(gamelog_2016_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2016_base_inter_six <- left_join(gamelog_2016_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2016_base_inter_seven <- left_join(gamelog_2016_base_inter_six, demographics_grouped_2016, by = c("Team", "Conference"))
colnames(gamelog_2016_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2016_base_inter_seven)[411:416])

gamelog_2016_base_inter_eight <- left_join(gamelog_2016_base_inter_seven, demographics_grouped_2016, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2016_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2016_base_inter_eight)[417:422])

gamelog_2016_base_inter_eight$FC_height_diff <- gamelog_2016_base_inter_eight$off_FC_height - gamelog_2016_base_inter_eight$def_FC_height
gamelog_2016_base_inter_eight$G_height_diff <- gamelog_2016_base_inter_eight$off_G_height - gamelog_2016_base_inter_eight$def_G_height

gamelog_2016_base_inter_eight$FC_weight_diff <- gamelog_2016_base_inter_eight$off_FC_weight - gamelog_2016_base_inter_eight$def_FC_weight
gamelog_2016_base_inter_eight$G_weight_diff <- gamelog_2016_base_inter_eight$off_G_weight - gamelog_2016_base_inter_eight$def_G_weight

gamelog_2016_base_inter_eight$FC_class_diff <- gamelog_2016_base_inter_eight$off_FC_class - gamelog_2016_base_inter_eight$def_FC_class
gamelog_2016_base_inter_eight$G_class_diff <- gamelog_2016_base_inter_eight$off_G_class - gamelog_2016_base_inter_eight$def_G_class


gamelog_2016_base_inter_eight$LocationInd <- ifelse(gamelog_2016_base_inter_eight$Location == "H", 1, 0)
gamelog_2016_base_inter_eight$ConferenceInd <- ifelse(gamelog_2016_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2016_base_inter_eight <- gamelog_2016_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2016_base_inter_eight <- gamelog_2016_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2016_base_inter_eight)[21:ncol(gamelog_2016_base_inter_eight)]

gamelog_2016_base_inter_eight <- gamelog_2016_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))


####
####
####


gamelog_2017_base <- gamelog_2017 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2017

gamelog_2017_base_inter_one <- left_join(gamelog_2017_base, offense_final_2017, by = c("Team", "Conference"))
colnames(gamelog_2017_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2017_base_inter_one)[21:137])

## DEF
# defense_final_2017


gamelog_2017_base_inter_two <- left_join(gamelog_2017_base_inter_one, defense_final_2017, by = c("Opp", "OppConference"))
colnames(gamelog_2017_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2017_base_inter_two)[c(138:254)])


## OFF
# conference_final_2017
# non_conference_final_2017


gamelog_2017_base_inter_three <- left_join(gamelog_2017_base_inter_two, conference_final_2017, by = "Conference")
colnames(gamelog_2017_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2017_base_inter_three)[c(255:293)])


gamelog_2017_base_inter_four <- left_join(gamelog_2017_base_inter_three, non_conference_final_2017, by = "Conference")

colnames(gamelog_2017_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2017_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2017_base_inter_five <- left_join(gamelog_2017_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2017_base_inter_six <- left_join(gamelog_2017_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2017_base_inter_seven <- left_join(gamelog_2017_base_inter_six, demographics_grouped_2017, by = c("Team", "Conference"))
colnames(gamelog_2017_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2017_base_inter_seven)[411:416])

gamelog_2017_base_inter_eight <- left_join(gamelog_2017_base_inter_seven, demographics_grouped_2017, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2017_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2017_base_inter_eight)[417:422])

gamelog_2017_base_inter_eight$FC_height_diff <- gamelog_2017_base_inter_eight$off_FC_height - gamelog_2017_base_inter_eight$def_FC_height
gamelog_2017_base_inter_eight$G_height_diff <- gamelog_2017_base_inter_eight$off_G_height - gamelog_2017_base_inter_eight$def_G_height

gamelog_2017_base_inter_eight$FC_weight_diff <- gamelog_2017_base_inter_eight$off_FC_weight - gamelog_2017_base_inter_eight$def_FC_weight
gamelog_2017_base_inter_eight$G_weight_diff <- gamelog_2017_base_inter_eight$off_G_weight - gamelog_2017_base_inter_eight$def_G_weight

gamelog_2017_base_inter_eight$FC_class_diff <- gamelog_2017_base_inter_eight$off_FC_class - gamelog_2017_base_inter_eight$def_FC_class
gamelog_2017_base_inter_eight$G_class_diff <- gamelog_2017_base_inter_eight$off_G_class - gamelog_2017_base_inter_eight$def_G_class


gamelog_2017_base_inter_eight$LocationInd <- ifelse(gamelog_2017_base_inter_eight$Location == "H", 1, 0)
gamelog_2017_base_inter_eight$ConferenceInd <- ifelse(gamelog_2017_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2017_base_inter_eight <- gamelog_2017_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2017_base_inter_eight <- gamelog_2017_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2017_base_inter_eight)[21:ncol(gamelog_2017_base_inter_eight)]

gamelog_2017_base_inter_eight <- gamelog_2017_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))


####
####
####


gamelog_2018_base <- gamelog_2018 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2018

gamelog_2018_base_inter_one <- left_join(gamelog_2018_base, offense_final_2018, by = c("Team", "Conference"))
colnames(gamelog_2018_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2018_base_inter_one)[21:137])

## DEF
# defense_final_2018


gamelog_2018_base_inter_two <- left_join(gamelog_2018_base_inter_one, defense_final_2018, by = c("Opp", "OppConference"))
colnames(gamelog_2018_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2018_base_inter_two)[c(138:254)])


## OFF
# conference_final_2018
# non_conference_final_2018


gamelog_2018_base_inter_three <- left_join(gamelog_2018_base_inter_two, conference_final_2018, by = "Conference")
colnames(gamelog_2018_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2018_base_inter_three)[c(255:293)])


gamelog_2018_base_inter_four <- left_join(gamelog_2018_base_inter_three, non_conference_final_2018, by = "Conference")

colnames(gamelog_2018_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2018_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2018_base_inter_five <- left_join(gamelog_2018_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2018_base_inter_six <- left_join(gamelog_2018_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2018_base_inter_seven <- left_join(gamelog_2018_base_inter_six, demographics_grouped_2018, by = c("Team", "Conference"))
colnames(gamelog_2018_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2018_base_inter_seven)[411:416])

gamelog_2018_base_inter_eight <- left_join(gamelog_2018_base_inter_seven, demographics_grouped_2018, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2018_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2018_base_inter_eight)[417:422])

gamelog_2018_base_inter_eight$FC_height_diff <- gamelog_2018_base_inter_eight$off_FC_height - gamelog_2018_base_inter_eight$def_FC_height
gamelog_2018_base_inter_eight$G_height_diff <- gamelog_2018_base_inter_eight$off_G_height - gamelog_2018_base_inter_eight$def_G_height

gamelog_2018_base_inter_eight$FC_weight_diff <- gamelog_2018_base_inter_eight$off_FC_weight - gamelog_2018_base_inter_eight$def_FC_weight
gamelog_2018_base_inter_eight$G_weight_diff <- gamelog_2018_base_inter_eight$off_G_weight - gamelog_2018_base_inter_eight$def_G_weight

gamelog_2018_base_inter_eight$FC_class_diff <- gamelog_2018_base_inter_eight$off_FC_class - gamelog_2018_base_inter_eight$def_FC_class
gamelog_2018_base_inter_eight$G_class_diff <- gamelog_2018_base_inter_eight$off_G_class - gamelog_2018_base_inter_eight$def_G_class


gamelog_2018_base_inter_eight$LocationInd <- ifelse(gamelog_2018_base_inter_eight$Location == "H", 1, 0)
gamelog_2018_base_inter_eight$ConferenceInd <- ifelse(gamelog_2018_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2018_base_inter_eight <- gamelog_2018_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2018_base_inter_eight <- gamelog_2018_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2018_base_inter_eight)[21:ncol(gamelog_2018_base_inter_eight)]

gamelog_2018_base_inter_eight <- gamelog_2018_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))



####
####
####


gamelog_2019_base <- gamelog_2019 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2019

gamelog_2019_base_inter_one <- left_join(gamelog_2019_base, offense_final_2019, by = c("Team", "Conference"))
colnames(gamelog_2019_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2019_base_inter_one)[21:137])

## DEF
# defense_final_2019


gamelog_2019_base_inter_two <- left_join(gamelog_2019_base_inter_one, defense_final_2019, by = c("Opp", "OppConference"))
colnames(gamelog_2019_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2019_base_inter_two)[c(138:254)])


## OFF
# conference_final_2019
# non_conference_final_2019


gamelog_2019_base_inter_three <- left_join(gamelog_2019_base_inter_two, conference_final_2019, by = "Conference")
colnames(gamelog_2019_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2019_base_inter_three)[c(255:293)])


gamelog_2019_base_inter_four <- left_join(gamelog_2019_base_inter_three, non_conference_final_2019, by = "Conference")

colnames(gamelog_2019_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2019_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2019_base_inter_five <- left_join(gamelog_2019_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2019_base_inter_six <- left_join(gamelog_2019_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2019_base_inter_seven <- left_join(gamelog_2019_base_inter_six, demographics_grouped_2019, by = c("Team", "Conference"))
colnames(gamelog_2019_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2019_base_inter_seven)[411:416])

gamelog_2019_base_inter_eight <- left_join(gamelog_2019_base_inter_seven, demographics_grouped_2019, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2019_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2019_base_inter_eight)[417:422])

gamelog_2019_base_inter_eight$FC_height_diff <- gamelog_2019_base_inter_eight$off_FC_height - gamelog_2019_base_inter_eight$def_FC_height
gamelog_2019_base_inter_eight$G_height_diff <- gamelog_2019_base_inter_eight$off_G_height - gamelog_2019_base_inter_eight$def_G_height

gamelog_2019_base_inter_eight$FC_weight_diff <- gamelog_2019_base_inter_eight$off_FC_weight - gamelog_2019_base_inter_eight$def_FC_weight
gamelog_2019_base_inter_eight$G_weight_diff <- gamelog_2019_base_inter_eight$off_G_weight - gamelog_2019_base_inter_eight$def_G_weight

gamelog_2019_base_inter_eight$FC_class_diff <- gamelog_2019_base_inter_eight$off_FC_class - gamelog_2019_base_inter_eight$def_FC_class
gamelog_2019_base_inter_eight$G_class_diff <- gamelog_2019_base_inter_eight$off_G_class - gamelog_2019_base_inter_eight$def_G_class


gamelog_2019_base_inter_eight$LocationInd <- ifelse(gamelog_2019_base_inter_eight$Location == "H", 1, 0)
gamelog_2019_base_inter_eight$ConferenceInd <- ifelse(gamelog_2019_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2019_base_inter_eight <- gamelog_2019_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2019_base_inter_eight <- gamelog_2019_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2019_base_inter_eight)[21:ncol(gamelog_2019_base_inter_eight)]

gamelog_2019_base_inter_eight <- gamelog_2019_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))


####
####
####


gamelog_2020_base <- gamelog_2020 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2020

gamelog_2020_base_inter_one <- left_join(gamelog_2020_base, offense_final_2020, by = c("Team", "Conference"))
colnames(gamelog_2020_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2020_base_inter_one)[21:137])

## DEF
# defense_final_2020


gamelog_2020_base_inter_two <- left_join(gamelog_2020_base_inter_one, defense_final_2020, by = c("Opp", "OppConference"))
colnames(gamelog_2020_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2020_base_inter_two)[c(138:254)])


## OFF
# conference_final_2020
# non_conference_final_2020


gamelog_2020_base_inter_three <- left_join(gamelog_2020_base_inter_two, conference_final_2020, by = "Conference")
colnames(gamelog_2020_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2020_base_inter_three)[c(255:293)])


gamelog_2020_base_inter_four <- left_join(gamelog_2020_base_inter_three, non_conference_final_2020, by = "Conference")

colnames(gamelog_2020_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2020_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2020_base_inter_five <- left_join(gamelog_2020_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2020_base_inter_six <- left_join(gamelog_2020_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2020_base_inter_seven <- left_join(gamelog_2020_base_inter_six, demographics_grouped_2020, by = c("Team", "Conference"))
colnames(gamelog_2020_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2020_base_inter_seven)[411:416])

gamelog_2020_base_inter_eight <- left_join(gamelog_2020_base_inter_seven, demographics_grouped_2020, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2020_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2020_base_inter_eight)[417:422])

gamelog_2020_base_inter_eight$FC_height_diff <- gamelog_2020_base_inter_eight$off_FC_height - gamelog_2020_base_inter_eight$def_FC_height
gamelog_2020_base_inter_eight$G_height_diff <- gamelog_2020_base_inter_eight$off_G_height - gamelog_2020_base_inter_eight$def_G_height

gamelog_2020_base_inter_eight$FC_weight_diff <- gamelog_2020_base_inter_eight$off_FC_weight - gamelog_2020_base_inter_eight$def_FC_weight
gamelog_2020_base_inter_eight$G_weight_diff <- gamelog_2020_base_inter_eight$off_G_weight - gamelog_2020_base_inter_eight$def_G_weight

gamelog_2020_base_inter_eight$FC_class_diff <- gamelog_2020_base_inter_eight$off_FC_class - gamelog_2020_base_inter_eight$def_FC_class
gamelog_2020_base_inter_eight$G_class_diff <- gamelog_2020_base_inter_eight$off_G_class - gamelog_2020_base_inter_eight$def_G_class


gamelog_2020_base_inter_eight$LocationInd <- ifelse(gamelog_2020_base_inter_eight$Location == "H", 1, 0)
gamelog_2020_base_inter_eight$ConferenceInd <- ifelse(gamelog_2020_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2020_base_inter_eight <- gamelog_2020_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2020_base_inter_eight <- gamelog_2020_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2020_base_inter_eight)[21:ncol(gamelog_2020_base_inter_eight)]

gamelog_2020_base_inter_eight <- gamelog_2020_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))


####
####
####


gamelog_2021_base <- gamelog_2021 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2021

gamelog_2021_base_inter_one <- left_join(gamelog_2021_base, offense_final_2021, by = c("Team", "Conference"))
colnames(gamelog_2021_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2021_base_inter_one)[21:137])

## DEF
# defense_final_2021


gamelog_2021_base_inter_two <- left_join(gamelog_2021_base_inter_one, defense_final_2021, by = c("Opp", "OppConference"))
colnames(gamelog_2021_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2021_base_inter_two)[c(138:254)])


## OFF
# conference_final_2021
# non_conference_final_2021


gamelog_2021_base_inter_three <- left_join(gamelog_2021_base_inter_two, conference_final_2021, by = "Conference")
colnames(gamelog_2021_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2021_base_inter_three)[c(255:293)])


gamelog_2021_base_inter_four <- left_join(gamelog_2021_base_inter_three, non_conference_final_2021, by = "Conference")

colnames(gamelog_2021_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2021_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2021_base_inter_five <- left_join(gamelog_2021_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2021_base_inter_six <- left_join(gamelog_2021_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2021_base_inter_seven <- left_join(gamelog_2021_base_inter_six, demographics_grouped_2021, by = c("Team", "Conference"))
colnames(gamelog_2021_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2021_base_inter_seven)[411:416])

gamelog_2021_base_inter_eight <- left_join(gamelog_2021_base_inter_seven, demographics_grouped_2021, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2021_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2021_base_inter_eight)[417:422])

gamelog_2021_base_inter_eight$FC_height_diff <- gamelog_2021_base_inter_eight$off_FC_height - gamelog_2021_base_inter_eight$def_FC_height
gamelog_2021_base_inter_eight$G_height_diff <- gamelog_2021_base_inter_eight$off_G_height - gamelog_2021_base_inter_eight$def_G_height

gamelog_2021_base_inter_eight$FC_weight_diff <- gamelog_2021_base_inter_eight$off_FC_weight - gamelog_2021_base_inter_eight$def_FC_weight
gamelog_2021_base_inter_eight$G_weight_diff <- gamelog_2021_base_inter_eight$off_G_weight - gamelog_2021_base_inter_eight$def_G_weight

gamelog_2021_base_inter_eight$FC_class_diff <- gamelog_2021_base_inter_eight$off_FC_class - gamelog_2021_base_inter_eight$def_FC_class
gamelog_2021_base_inter_eight$G_class_diff <- gamelog_2021_base_inter_eight$off_G_class - gamelog_2021_base_inter_eight$def_G_class


gamelog_2021_base_inter_eight$LocationInd <- ifelse(gamelog_2021_base_inter_eight$Location == "H", 1, 0)
gamelog_2021_base_inter_eight$ConferenceInd <- ifelse(gamelog_2021_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2021_base_inter_eight <- gamelog_2021_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2021_base_inter_eight <- gamelog_2021_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2021_base_inter_eight)[21:ncol(gamelog_2021_base_inter_eight)]

gamelog_2021_base_inter_eight <- gamelog_2021_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))


####
####
####


gamelog_2022_base <- gamelog_2022 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2022

gamelog_2022_base_inter_one <- left_join(gamelog_2022_base, offense_final_2022, by = c("Team", "Conference"))
colnames(gamelog_2022_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2022_base_inter_one)[21:137])

## DEF
# defense_final_2022


gamelog_2022_base_inter_two <- left_join(gamelog_2022_base_inter_one, defense_final_2022, by = c("Opp", "OppConference"))
colnames(gamelog_2022_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2022_base_inter_two)[c(138:254)])


## OFF
# conference_final_2022
# non_conference_final_2022


gamelog_2022_base_inter_three <- left_join(gamelog_2022_base_inter_two, conference_final_2022, by = "Conference")
colnames(gamelog_2022_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2022_base_inter_three)[c(255:293)])


gamelog_2022_base_inter_four <- left_join(gamelog_2022_base_inter_three, non_conference_final_2022, by = "Conference")

colnames(gamelog_2022_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2022_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2022_base_inter_five <- left_join(gamelog_2022_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2022_base_inter_six <- left_join(gamelog_2022_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2022_base_inter_seven <- left_join(gamelog_2022_base_inter_six, demographics_grouped_2022, by = c("Team", "Conference"))
colnames(gamelog_2022_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2022_base_inter_seven)[411:416])

gamelog_2022_base_inter_eight <- left_join(gamelog_2022_base_inter_seven, demographics_grouped_2022, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2022_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2022_base_inter_eight)[417:422])

gamelog_2022_base_inter_eight$FC_height_diff <- gamelog_2022_base_inter_eight$off_FC_height - gamelog_2022_base_inter_eight$def_FC_height
gamelog_2022_base_inter_eight$G_height_diff <- gamelog_2022_base_inter_eight$off_G_height - gamelog_2022_base_inter_eight$def_G_height

gamelog_2022_base_inter_eight$FC_weight_diff <- gamelog_2022_base_inter_eight$off_FC_weight - gamelog_2022_base_inter_eight$def_FC_weight
gamelog_2022_base_inter_eight$G_weight_diff <- gamelog_2022_base_inter_eight$off_G_weight - gamelog_2022_base_inter_eight$def_G_weight

gamelog_2022_base_inter_eight$FC_class_diff <- gamelog_2022_base_inter_eight$off_FC_class - gamelog_2022_base_inter_eight$def_FC_class
gamelog_2022_base_inter_eight$G_class_diff <- gamelog_2022_base_inter_eight$off_G_class - gamelog_2022_base_inter_eight$def_G_class


gamelog_2022_base_inter_eight$LocationInd <- ifelse(gamelog_2022_base_inter_eight$Location == "H", 1, 0)
gamelog_2022_base_inter_eight$ConferenceInd <- ifelse(gamelog_2022_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2022_base_inter_eight <- gamelog_2022_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2022_base_inter_eight <- gamelog_2022_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2022_base_inter_eight)[21:ncol(gamelog_2022_base_inter_eight)]

gamelog_2022_base_inter_eight <- gamelog_2022_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))


####
####
####


gamelog_2023_base <- gamelog_2023 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2023

gamelog_2023_base_inter_one <- left_join(gamelog_2023_base, offense_final_2023, by = c("Team", "Conference"))
colnames(gamelog_2023_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2023_base_inter_one)[21:137])

## DEF
# defense_final_2023


gamelog_2023_base_inter_two <- left_join(gamelog_2023_base_inter_one, defense_final_2023, by = c("Opp", "OppConference"))
colnames(gamelog_2023_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2023_base_inter_two)[c(138:254)])


## OFF
# conference_final_2023
# non_conference_final_2023


gamelog_2023_base_inter_three <- left_join(gamelog_2023_base_inter_two, conference_final_2023, by = "Conference")
colnames(gamelog_2023_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2023_base_inter_three)[c(255:293)])


gamelog_2023_base_inter_four <- left_join(gamelog_2023_base_inter_three, non_conference_final_2023, by = "Conference")

colnames(gamelog_2023_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2023_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2023_base_inter_five <- left_join(gamelog_2023_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2023_base_inter_six <- left_join(gamelog_2023_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2023_base_inter_seven <- left_join(gamelog_2023_base_inter_six, demographics_grouped_2023, by = c("Team", "Conference"))
colnames(gamelog_2023_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2023_base_inter_seven)[411:416])

gamelog_2023_base_inter_eight <- left_join(gamelog_2023_base_inter_seven, demographics_grouped_2023, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2023_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2023_base_inter_eight)[417:422])

gamelog_2023_base_inter_eight$FC_height_diff <- gamelog_2023_base_inter_eight$off_FC_height - gamelog_2023_base_inter_eight$def_FC_height
gamelog_2023_base_inter_eight$G_height_diff <- gamelog_2023_base_inter_eight$off_G_height - gamelog_2023_base_inter_eight$def_G_height

gamelog_2023_base_inter_eight$FC_weight_diff <- gamelog_2023_base_inter_eight$off_FC_weight - gamelog_2023_base_inter_eight$def_FC_weight
gamelog_2023_base_inter_eight$G_weight_diff <- gamelog_2023_base_inter_eight$off_G_weight - gamelog_2023_base_inter_eight$def_G_weight

gamelog_2023_base_inter_eight$FC_class_diff <- gamelog_2023_base_inter_eight$off_FC_class - gamelog_2023_base_inter_eight$def_FC_class
gamelog_2023_base_inter_eight$G_class_diff <- gamelog_2023_base_inter_eight$off_G_class - gamelog_2023_base_inter_eight$def_G_class


gamelog_2023_base_inter_eight$LocationInd <- ifelse(gamelog_2023_base_inter_eight$Location == "H", 1, 0)
gamelog_2023_base_inter_eight$ConferenceInd <- ifelse(gamelog_2023_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2023_base_inter_eight <- gamelog_2023_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2023_base_inter_eight <- gamelog_2023_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2023_base_inter_eight)[21:ncol(gamelog_2023_base_inter_eight)]

gamelog_2023_base_inter_eight <- gamelog_2023_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))


####
####
####


gamelog_2024_base <- gamelog_2024 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2024

gamelog_2024_base_inter_one <- left_join(gamelog_2024_base, offense_final_2024, by = c("Team", "Conference"))
colnames(gamelog_2024_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2024_base_inter_one)[21:137])

## DEF
# defense_final_2024


gamelog_2024_base_inter_two <- left_join(gamelog_2024_base_inter_one, defense_final_2024, by = c("Opp", "OppConference"))
colnames(gamelog_2024_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2024_base_inter_two)[c(138:254)])


## OFF
# conference_final_2024
# non_conference_final_2024


gamelog_2024_base_inter_three <- left_join(gamelog_2024_base_inter_two, conference_final_2024, by = "Conference")
colnames(gamelog_2024_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2024_base_inter_three)[c(255:293)])


gamelog_2024_base_inter_four <- left_join(gamelog_2024_base_inter_three, non_conference_final_2024, by = "Conference")

colnames(gamelog_2024_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2024_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2024_base_inter_five <- left_join(gamelog_2024_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2024_base_inter_six <- left_join(gamelog_2024_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2024_base_inter_seven <- left_join(gamelog_2024_base_inter_six, demographics_grouped_2024, by = c("Team", "Conference"))
colnames(gamelog_2024_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2024_base_inter_seven)[411:416])

gamelog_2024_base_inter_eight <- left_join(gamelog_2024_base_inter_seven, demographics_grouped_2024, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2024_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2024_base_inter_eight)[417:422])

gamelog_2024_base_inter_eight$FC_height_diff <- gamelog_2024_base_inter_eight$off_FC_height - gamelog_2024_base_inter_eight$def_FC_height
gamelog_2024_base_inter_eight$G_height_diff <- gamelog_2024_base_inter_eight$off_G_height - gamelog_2024_base_inter_eight$def_G_height

gamelog_2024_base_inter_eight$FC_weight_diff <- gamelog_2024_base_inter_eight$off_FC_weight - gamelog_2024_base_inter_eight$def_FC_weight
gamelog_2024_base_inter_eight$G_weight_diff <- gamelog_2024_base_inter_eight$off_G_weight - gamelog_2024_base_inter_eight$def_G_weight

gamelog_2024_base_inter_eight$FC_class_diff <- gamelog_2024_base_inter_eight$off_FC_class - gamelog_2024_base_inter_eight$def_FC_class
gamelog_2024_base_inter_eight$G_class_diff <- gamelog_2024_base_inter_eight$off_G_class - gamelog_2024_base_inter_eight$def_G_class


gamelog_2024_base_inter_eight$LocationInd <- ifelse(gamelog_2024_base_inter_eight$Location == "H", 1, 0)
gamelog_2024_base_inter_eight$ConferenceInd <- ifelse(gamelog_2024_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2024_base_inter_eight <- gamelog_2024_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.)))

gamelog_2024_base_inter_eight <- gamelog_2024_base_inter_eight %>% group_by(Team) %>% mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2024_base_inter_eight)[21:ncol(gamelog_2024_base_inter_eight)]

gamelog_2024_base_inter_eight <- gamelog_2024_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))



####
####
####


gamelog_2025_base <- gamelog_2025 %>% select(G, Date, Location, OppLocation, Team, Opp, GameType, Conference, OppConference, `W/L`, PPP, poss, `2Pperc`, `3P%`, STL_perc, TOV_perc, ORB_perc, FTR) %>%
    mutate(win_ind = ifelse(`W/L` == 'W', 1, 0),
           Location = ifelse(Location == "H", "H", "A"),
           OppLocation = ifelse(OppLocation == "H", "H", "A")) %>%
    mutate(
        game_id = case_when(
            Location == "H" ~ paste0(Team, "-", Date),
            Location == "A" & OppLocation == "H" ~ paste0(Opp, "-", Date),
            Location == "A" & OppLocation == "A" ~ if_else(Team < Opp,
                                                           paste0(Team, "-", Opp, "-", Date),
                                                           paste0(Opp, "-", Team, "-", Date)),
            TRUE ~ NA_character_
        )
    )


# FOR W/L - WERE GOING TO HAVE TO FIND A WAY TO JOIN THE OPPOSING DATA IN

### IN ORDER, WE WANT TO JOIN


## OFF
# offense_final_2025

gamelog_2025_base_inter_one <- left_join(gamelog_2025_base, offense_final_2025, by = c("Team", "Conference"))
colnames(gamelog_2025_base_inter_one)[21:137] <- paste0("off_", colnames(gamelog_2025_base_inter_one)[21:137])

## DEF
# defense_final_2025


gamelog_2025_base_inter_two <- left_join(gamelog_2025_base_inter_one, defense_final_2025, by = c("Opp", "OppConference"))
colnames(gamelog_2025_base_inter_two)[c(138:254)] <- paste0("def_", colnames(gamelog_2025_base_inter_two)[c(138:254)])


## OFF
# conference_final_2025
# non_conference_final_2025


gamelog_2025_base_inter_three <- left_join(gamelog_2025_base_inter_two, conference_final_2025, by = "Conference")
colnames(gamelog_2025_base_inter_three)[c(255:293)] <- paste0("conf_mean_", colnames(gamelog_2025_base_inter_three)[c(255:293)])


gamelog_2025_base_inter_four <- left_join(gamelog_2025_base_inter_three, non_conference_final_2025, by = "Conference")

colnames(gamelog_2025_base_inter_four)[c(293:332)] <- paste0("non_conf_mean_", colnames(gamelog_2025_base_inter_four)[c(293:332)])


## DEF
# conference_def_final_2025
# non_conference_def_final_2025


gamelog_2025_base_inter_five <- left_join(gamelog_2025_base_inter_four, conference_def_final_2025, by = "OppConference")
gamelog_2025_base_inter_six <- left_join(gamelog_2025_base_inter_five, non_conference_def_final_2025, by = "OppConference")


## DEMO
# demographics_grouped_2025 (THIS NEEDS TO COME IN TWICE)


gamelog_2025_base_inter_seven <- left_join(gamelog_2025_base_inter_six, demographics_grouped_2025, by = c("Team", "Conference"))
colnames(gamelog_2025_base_inter_seven)[411:416] <- paste0("off_", colnames(gamelog_2025_base_inter_seven)[411:416])

gamelog_2025_base_inter_eight <- left_join(gamelog_2025_base_inter_seven, demographics_grouped_2025, by = c("Opp" = "Team", "OppConference" = "Conference"))
colnames(gamelog_2025_base_inter_eight)[417:422] <- paste0("def_", colnames(gamelog_2025_base_inter_eight)[417:422])

gamelog_2025_base_inter_eight$Date <- as.Date(gamelog_2025_base_inter_eight$Date)

gamelog_2025_base_inter_eight$FC_height_diff <- gamelog_2025_base_inter_eight$off_FC_height - gamelog_2025_base_inter_eight$def_FC_height
gamelog_2025_base_inter_eight$G_height_diff <- gamelog_2025_base_inter_eight$off_G_height - gamelog_2025_base_inter_eight$def_G_height

gamelog_2025_base_inter_eight$FC_weight_diff <- gamelog_2025_base_inter_eight$off_FC_weight - gamelog_2025_base_inter_eight$def_FC_weight
gamelog_2025_base_inter_eight$G_weight_diff <- gamelog_2025_base_inter_eight$off_G_weight - gamelog_2025_base_inter_eight$def_G_weight

gamelog_2025_base_inter_eight$FC_class_diff <- gamelog_2025_base_inter_eight$off_FC_class - gamelog_2025_base_inter_eight$def_FC_class
gamelog_2025_base_inter_eight$G_class_diff <- gamelog_2025_base_inter_eight$off_G_class - gamelog_2025_base_inter_eight$def_G_class


gamelog_2025_base_inter_eight$LocationInd <- ifelse(gamelog_2025_base_inter_eight$Location == "H", 1, 0)
gamelog_2025_base_inter_eight$ConferenceInd <- ifelse(gamelog_2025_base_inter_eight$GameType %in% c("REG (Conf)", "CTOURN"), 1, 0)


gamelog_2025_base_inter_eight <- gamelog_2025_base_inter_eight %>%
    filter(if_all(everything(), ~ !is.na(.))) %>% arrange(Team, Date)

gamelog_2025_base_inter_eight <- gamelog_2025_base_inter_eight %>% group_by(Team) %>%
    mutate(away_b2b_ind = ifelse(lag(Location) == "A" & !is.na(lag(Date)) & (as.numeric(Date - lag(Date)) <= 2) & G != 1, 1, 0)) %>% ungroup()

cols_to_zscore <- colnames(gamelog_2025_base_inter_eight)[21:ncol(gamelog_2025_base_inter_eight)]

gamelog_2025_base_inter_eight <- gamelog_2025_base_inter_eight %>% 
  ungroup() %>% 
  mutate(across(
    all_of(cols_to_zscore),
    ~ {
      m <- mean(., na.rm = TRUE)
      s <- sd(., na.rm = TRUE)
      if (s > 0) {
        (. - m) / s
      } else {
        0
      }
    }
  ))



###
###
###

colnames(gamelog_2021_base_inter_eight) <- colnames(gamelog_2018_base_inter_eight)


final_df <- rbind(gamelog_2016_base_inter_eight, gamelog_2017_base_inter_eight, gamelog_2018_base_inter_eight, gamelog_2019_base_inter_eight, gamelog_2020_base_inter_eight, gamelog_2021_base_inter_eight, gamelog_2022_base_inter_eight, gamelog_2023_base_inter_eight, gamelog_2024_base_inter_eight, gamelog_2025_base_inter_eight)


