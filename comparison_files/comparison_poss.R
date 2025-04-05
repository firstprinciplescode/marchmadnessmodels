

gamelog_2016$season = 2016
gamelog_2017$season = 2017
gamelog_2018$season = 2018
gamelog_2019$season = 2019
gamelog_2020$season = 2020
gamelog_2021$season = 2021
gamelog_2022$season = 2022
gamelog_2023$season = 2023
gamelog_2024$season = 2024
gamelog_2025$season = 2025


gamelog_comb <- rbind(gamelog_2016, gamelog_2017, gamelog_2018, gamelog_2019, gamelog_2020, gamelog_2021, gamelog_2022, gamelog_2023, gamelog_2024, gamelog_2025)


gamelog_comb <- gamelog_comb %>%
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


poss_df <- gamelog_comb %>% 
  select(Team, Opp, season, game_id, home_perc_poss, away_perc_poss, conf_home_perc_poss, conf_away_perc_poss, non_conf_home_perc_poss, non_conf_away_perc_poss, def_home_perc_poss, def_away_perc_poss, def_conf_home_perc_poss, def_conf_away_perc_poss, def_non_conf_home_perc_poss, def_non_conf_away_perc_poss)
colnames(poss_df)[c(5:10)] <- paste0("off_", colnames(poss_df)[c(5:10)])

poss_final_df <- inner_join(final_df, poss_df, by = c("Team", "Opp", "game_id"))

colnames(poss_final_df) <- gsub("%", "", colnames(poss_final_df))


###
###
###


off_string = "Boise State"
def_string = "Nebraska"


offense_team <- which(ppp_final_df$Team == off_string & ppp_final_df$season == 2025)[1]
defense_team <- which(ppp_final_df$Opp == def_string & ppp_final_df$season == 2025)[1]

prediction_df <- cbind(ppp_final_df[offense_team,c(21:137)], ppp_final_df[defense_team,c(138:254)], ppp_final_df[offense_team,c(255:332)], ppp_final_df[defense_team,c(333:410)], ppp_final_df[offense_team,c(411:416)], ppp_final_df[defense_team,c(417:423)])

prediction_df$FC_height_diff <- prediction_df$off_FC_height - prediction_df$def_FC_height
prediction_df$G_height_diff <- prediction_df$off_G_height - prediction_df$def_G_height

prediction_df$FC_weight_diff <- prediction_df$off_FC_weight - prediction_df$def_FC_weight
prediction_df$G_weight_diff <- prediction_df$off_G_weight - prediction_df$def_G_weight

prediction_df$FC_class_diff <- prediction_df$off_FC_class - prediction_df$def_FC_class
prediction_df$G_class_diff <- prediction_df$off_G_class - prediction_df$def_G_class

prediction_df$LocationInd = -.89
prediction_df$ConferenceInd = -1.33
prediction_df$away_b2b_ind = 2.59

colnames(prediction_df) <- gsub("%", "", colnames(prediction_df))


predict(xgb_poss_route, as.matrix(prediction_df))


comparison_poss_func <- function(threshold, year = NULL, locationinput = NULL) {
  current_game_vector <- prediction_df
  reference_df <- cbind(
    poss_final_df %>% select(Team, Opp, season, Date, Location, poss, off_home_perc_poss:def_non_conf_away_perc_poss),
    poss_final_df[, which(colnames(poss_final_df) %in% importance_matrix_poss$Feature)]
  )
  
  relevant_features <- importance_matrix_poss$Feature
  current_game <- current_game_vector[relevant_features] %>% as.numeric()
  
  metadata_cols <- reference_df %>%
    select(Team, Opp, season, Date, Location, poss, off_home_perc_poss:def_non_conf_away_perc_poss)
  
  features_matrix <- reference_df %>%
    select(all_of(relevant_features)) %>%
    as.matrix()
  
  weights <- importance_matrix_poss$Gain
  diff_mat <- abs(sweep(features_matrix, 2, current_game, "-"))
  weighted_diff <- sweep(diff_mat, 2, weights, "*")
  row_scores <- rowSums(weighted_diff)
  
  results_df <- cbind(metadata_cols, weighted_diff_score = row_scores)
  
  # Optional year filter
  if (!is.null(year)) {
    results_df <- results_df %>% filter(season == year)
  }
  
  # Optional location filter
  if (!is.null(locationinput)) {
    results_df <- results_df %>% filter(Location == locationinput)
  }
  
  results_df <- results_df %>%
    filter(weighted_diff_score <= threshold) %>%
    arrange(weighted_diff_score)
  
  return(results_df)
}


output1 <- comparison_poss_func(.43, year = NULL, locationinput = "A")
output2 <- comparison_poss_func(.53, year = NULL, locationinput = "H")

nrow(output1)
nrow(output2)

output1 %>% dplyr::summarise(poss = mean(poss, na.rm = T),
                             off_away_perc_poss = mean(off_away_perc_poss, na.rm = T),
                             off_conf_away_perc_poss  = mean(off_conf_away_perc_poss , na.rm = T),
                             off_non_conf_away_perc_poss  = mean(off_non_conf_away_perc_poss , na.rm = T),
                             def_away_perc_poss = mean(def_away_perc_poss, na.rm = T),
                             def_conf_away_perc_poss  = mean(def_conf_away_perc_poss , na.rm = T),
                             def_non_conf_away_perc_poss  = mean(def_non_conf_away_perc_poss , na.rm = T))

output1 %>% filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(poss = mean(poss, na.rm = T),
                   off_away_perc_poss = mean(off_away_perc_poss, na.rm = T),
                   off_conf_away_perc_poss  = mean(off_conf_away_perc_poss , na.rm = T),
                   off_non_conf_away_perc_poss  = mean(off_non_conf_away_perc_poss , na.rm = T),
                   def_away_perc_poss = mean(def_away_perc_poss, na.rm = T),
                   def_conf_away_perc_poss  = mean(def_conf_away_perc_poss , na.rm = T),
                   def_non_conf_away_perc_poss  = mean(def_non_conf_away_perc_poss , na.rm = T),
                   n = n())



output2 %>% dplyr::summarise(poss = mean(poss, na.rm = T),
                             off_home_perc_poss = mean(off_home_perc_poss, na.rm = T),
                             off_conf_home_perc_poss  = mean(off_conf_home_perc_poss , na.rm = T),
                             off_non_conf_home_perc_poss  = mean(off_non_conf_home_perc_poss , na.rm = T),
                             def_home_perc_poss = mean(def_home_perc_poss, na.rm = T),
                             def_conf_home_perc_poss  = mean(def_conf_home_perc_poss , na.rm = T),
                             def_non_conf_home_perc_poss  = mean(def_non_conf_home_perc_poss , na.rm = T))

output2 %>% 
  filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(poss = mean(poss, na.rm = T),
                   off_home_perc_poss = mean(off_home_perc_poss, na.rm = T),
                   off_conf_home_perc_poss  = mean(off_conf_home_perc_poss , na.rm = T),
                   off_non_conf_home_perc_poss  = mean(off_non_conf_home_perc_poss , na.rm = T),
                   def_home_perc_poss = mean(def_home_perc_poss, na.rm = T),
                   def_conf_home_perc_poss  = mean(def_conf_home_perc_poss , na.rm = T),
                   def_non_conf_home_perc_poss  = mean(def_non_conf_home_perc_poss , na.rm = T),
                   n = n())
