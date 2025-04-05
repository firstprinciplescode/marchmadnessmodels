

threeptperc_df <- gamelog_comb %>% 
  select(Team, Opp, season, game_id, `home_perc_3P%`, `away_perc_3P%`, `conf_home_perc_3P%`, `conf_away_perc_3P%`, `non_conf_home_perc_3P%`, `non_conf_away_perc_3P%`, `def_home_perc_3P%`, `def_away_perc_3P%`, `def_conf_home_perc_3P%`, `def_conf_away_perc_3P%`, `def_non_conf_home_perc_3P%`, `def_non_conf_away_perc_3P%`)
colnames(threeptperc_df)[c(5:10)] <- paste0("off_", colnames(threeptperc_df)[c(5:10)])

threeptperc_df <- inner_join(final_df, threeptperc_df, by = c("Team", "Opp", "game_id"))

colnames(threeptperc_df) <- gsub("%", "", colnames(threeptperc_df))


###
###
###


off_string = "Nebraska"
def_string = "Boise State"

offense_team <- which(threeptperc_df$Team == off_string & threeptperc_df$season == 2025)[1]
defense_team <- which(threeptperc_df$Opp == def_string & threeptperc_df$season == 2025)[1]

prediction_df <- cbind(threeptperc_df[offense_team,c(21:137)], threeptperc_df[defense_team,c(138:254)], threeptperc_df[offense_team,c(255:332)], threeptperc_df[defense_team,c(333:410)], threeptperc_df[offense_team,c(411:416)], threeptperc_df[defense_team,c(417:423)])

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


predict(xgb_threeptperc_route, as.matrix(prediction_df))



comparison_threeptperc_func <- function(func_pred_df, threshold, year = NULL, locationinput = NULL) {
  current_game_vector <- func_pred_df
  reference_df <- cbind(
    threeptperc_df %>% select(Team, Opp, season, Date, Location, `3P`, `off_home_perc_3P`:`def_non_conf_away_perc_3P`),
    threeptperc_df[, which(colnames(threeptperc_df) %in% importance_matrix_threeptperc$Feature)]
  )
  
  relevant_features <- importance_matrix_threeptperc$Feature
  current_game <- current_game_vector[relevant_features] %>% as.numeric()
  
  metadata_cols <- reference_df %>%
    select(Team, Opp, season, Date, Location, `3P`, `off_home_perc_3P`:`def_non_conf_away_perc_3P`)
  
  features_matrix <- reference_df %>%
    select(all_of(relevant_features)) %>%
    as.matrix()
  
  weights <- importance_matrix_threeptperc$Gain
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


output1_threept <- comparison_threeptperc_func(prediction_df, .41, year = NULL, locationinput = "A")
output2_threept <- comparison_threeptperc_func(prediction_df_2, .77, year = NULL, locationinput = "H")

output1_threept %>% dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                                     off_away_perc_3P = mean(off_away_perc_3P, na.rm = T),
                                     off_conf_away_perc_3P = mean(off_conf_away_perc_3P, na.rm = T),
                                     off_non_conf_away_perc_3P = mean(off_non_conf_away_perc_3P, na.rm = T),
                                     def_away_perc_3P = mean(def_away_perc_3P, na.rm = T),
                                     def_conf_away_perc_3P  = mean(def_conf_away_perc_3P , na.rm = T),
                                     def_non_conf_away_perc_3P  = mean(def_non_conf_away_perc_3P , na.rm = T))

output1_threept %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                                     off_away_perc_3P = mean(off_away_perc_3P, na.rm = T),
                                     off_conf_away_perc_3P = mean(off_conf_away_perc_3P, na.rm = T),
                                     off_non_conf_away_perc_3P = mean(off_non_conf_away_perc_3P, na.rm = T),
                                     def_away_perc_3P = mean(def_away_perc_3P, na.rm = T),
                                     def_conf_away_perc_3P  = mean(def_conf_away_perc_3P , na.rm = T),
                                     def_non_conf_away_perc_3P  = mean(def_non_conf_away_perc_3P , na.rm = T),
                                     n = n())


output2_threept %>% dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                                     off_home_perc_3P = mean(off_home_perc_3P, na.rm = T),
                                     off_conf_home_perc_3P = mean(off_conf_home_perc_3P, na.rm = T),
                                     off_non_conf_home_perc_3P = mean(off_non_conf_home_perc_3P, na.rm = T),
                                     def_home_perc_3P = mean(def_home_perc_3P, na.rm = T),
                                     def_conf_home_perc_3P  = mean(def_conf_home_perc_3P , na.rm = T),
                                     def_non_conf_home_perc_3P  = mean(def_non_conf_home_perc_3P , na.rm = T))

output2_threept %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                   off_home_perc_3P = mean(off_home_perc_3P, na.rm = T),
                   off_conf_home_perc_3P = mean(off_conf_home_perc_3P, na.rm = T),
                   off_non_conf_home_perc_3P = mean(off_non_conf_home_perc_3P, na.rm = T),
                   def_home_perc_3P = mean(def_home_perc_3P, na.rm = T),
                   def_conf_home_perc_3P  = mean(def_conf_home_perc_3P , na.rm = T),
                   def_non_conf_home_perc_3P  = mean(def_non_conf_home_perc_3P , na.rm = T),
                   n = n())
