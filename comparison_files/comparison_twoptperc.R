

twoptperc_df <- gamelog_comb %>% 
  select(Team, Opp, season, game_id, home_perc_2Pperc, away_perc_2Pperc, conf_home_perc_2Pperc, conf_away_perc_2Pperc, non_conf_home_perc_2Pperc, non_conf_away_perc_2Pperc, def_home_perc_2Pperc, def_away_perc_2Pperc, def_conf_home_perc_2Pperc, def_conf_away_perc_2Pperc, def_non_conf_home_perc_2Pperc, def_non_conf_away_perc_2Pperc)
colnames(twoptperc_df)[c(5:10)] <- paste0("off_", colnames(twoptperc_df)[c(5:10)])

twoptperc_df <- inner_join(final_df, twoptperc_df, by = c("Team", "Opp", "game_id"))

colnames(twoptperc_df) <- gsub("%", "", colnames(twoptperc_df))


###
###
###


off_string = "Nebraska"
def_string = "Boise State"


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
prediction_df$away_b2b_ind = -.386

colnames(prediction_df) <- gsub("%", "", colnames(prediction_df))


predict(xgb_twoptperc_route, as.matrix(prediction_df))


comparison_twoptperc_func <- function(func_pred_df, threshold, year = NULL, locationinput = NULL) {
  current_game_vector <- func_pred_df
  reference_df <- cbind(
    twoptperc_df %>% select(Team, Opp, season, Date, Location, `2Pperc`, off_home_perc_2Pperc:def_non_conf_away_perc_2Pperc),
    twoptperc_df[, which(colnames(twoptperc_df) %in% importance_matrix_twoptperc$Feature)]
  )
  
  relevant_features <- importance_matrix_twoptperc$Feature
  current_game <- current_game_vector[relevant_features] %>% as.numeric()
  
  metadata_cols <- reference_df %>%
    select(Team, Opp, season, Date, Location, `2Pperc`, off_home_perc_2Pperc:def_non_conf_away_perc_2Pperc)
  
  features_matrix <- reference_df %>%
    select(all_of(relevant_features)) %>%
    as.matrix()
  
  weights <- importance_matrix_twoptperc$Gain
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


output1_twopt <- comparison_twoptperc_func(.46, year = NULL, locationinput = "A")
output2_twopt <- comparison_twoptperc_func(.62, year = NULL, locationinput = "H")

output1_twopt %>% dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                                   off_away_perc_2Pperc = mean(off_away_perc_2Pperc, na.rm = T),
                                   off_conf_away_perc_2Pperc = mean(off_conf_away_perc_2Pperc, na.rm = T),
                                   off_non_conf_away_perc_2Pperc = mean(off_non_conf_away_perc_2Pperc, na.rm = T),
                                   def_away_perc_2Pperc = mean(def_away_perc_2Pperc, na.rm = T),
                                   def_conf_away_perc_2Pperc  = mean(def_conf_away_perc_2Pperc , na.rm = T),
                                   def_non_conf_away_perc_2Pperc  = mean(def_non_conf_away_perc_2Pperc , na.rm = T))

output1_twopt %>% filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                   off_away_perc_2Pperc = mean(off_away_perc_2Pperc, na.rm = T),
                   off_conf_away_perc_2Pperc = mean(off_conf_away_perc_2Pperc, na.rm = T),
                   off_non_conf_away_perc_2Pperc = mean(off_non_conf_away_perc_2Pperc, na.rm = T),
                   def_away_perc_2Pperc = mean(def_away_perc_2Pperc, na.rm = T),
                   def_conf_away_perc_2Pperc  = mean(def_conf_away_perc_2Pperc , na.rm = T),
                   def_non_conf_away_perc_2Pperc  = mean(def_non_conf_away_perc_2Pperc , na.rm = T),
                   n = n())



output2_twopt %>% dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                                   off_home_perc_2Pperc = mean(off_home_perc_2Pperc, na.rm = T),
                                   off_conf_home_perc_2Pperc = mean(off_conf_home_perc_2Pperc, na.rm = T),
                                   off_non_conf_home_perc_2Pperc = mean(off_non_conf_home_perc_2Pperc, na.rm = T),
                                   def_home_perc_2Pperc = mean(def_home_perc_2Pperc, na.rm = T),
                                   def_conf_home_perc_2Pperc  = mean(def_conf_home_perc_2Pperc , na.rm = T),
                                   def_non_conf_home_perc_2Pperc  = mean(def_non_conf_home_perc_2Pperc , na.rm = T))

output2_twopt %>% 
  filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                   off_home_perc_2Pperc = mean(off_home_perc_2Pperc, na.rm = T),
                   off_conf_home_perc_2Pperc = mean(off_conf_home_perc_2Pperc, na.rm = T),
                   off_non_conf_home_perc_2Pperc = mean(off_non_conf_home_perc_2Pperc, na.rm = T),
                   def_home_perc_2Pperc = mean(def_home_perc_2Pperc, na.rm = T),
                   def_conf_home_perc_2Pperc  = mean(def_conf_home_perc_2Pperc , na.rm = T),
                   def_non_conf_home_perc_2Pperc  = mean(def_non_conf_home_perc_2Pperc , na.rm = T),
                   n = n())
