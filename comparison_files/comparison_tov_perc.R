

TOV_perc_df <- gamelog_comb %>% 
  select(Team, Opp, season, game_id, home_perc_TOV_perc, away_perc_TOV_perc, conf_home_perc_TOV_perc, conf_away_perc_TOV_perc, non_conf_home_perc_TOV_perc, non_conf_away_perc_TOV_perc, def_home_perc_TOV_perc, def_away_perc_TOV_perc, def_conf_home_perc_TOV_perc, def_conf_away_perc_TOV_perc, def_non_conf_home_perc_TOV_perc, def_non_conf_away_perc_TOV_perc)
colnames(TOV_perc_df)[c(5:10)] <- paste0("off_", colnames(TOV_perc_df)[c(5:10)])

TOV_perc_df <- inner_join(final_df, TOV_perc_df, by = c("Team", "Opp", "game_id"))

colnames(TOV_perc_df) <- gsub("%", "", colnames(TOV_perc_df))


###
###
###


off_string = "Chattanooga"
def_string = "UC Irvine"

offense_team <- which(TOV_perc_df$Team == off_string & TOV_perc_df$season == 2025)[1]
defense_team <- which(TOV_perc_df$Opp == def_string & TOV_perc_df$season == 2025)[1]

prediction_df <- cbind(TOV_perc_df[offense_team,c(21:137)], TOV_perc_df[defense_team,c(138:254)], TOV_perc_df[offense_team,c(255:332)], TOV_perc_df[defense_team,c(333:410)], TOV_perc_df[offense_team,c(411:416)], TOV_perc_df[defense_team,c(417:423)])

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


predict(xgb_TOV_perc_route, as.matrix(prediction_df))


comparison_tov_perc_func <- function(threshold, year = NULL, locationinput = NULL) {
  current_game_vector <- prediction_df
  reference_df <- cbind(
    TOV_perc_df %>% select(Team, Opp, season, Date, Location, TOV_perc, off_home_perc_TOV_perc:def_non_conf_away_perc_TOV_perc),
    TOV_perc_df[, which(colnames(TOV_perc_df) %in% importance_matrix_TOV_perc$Feature)]
  )
  
  relevant_features <- importance_matrix_TOV_perc$Feature
  current_game <- current_game_vector[relevant_features] %>% as.numeric()
  
  metadata_cols <- reference_df %>%
    select(Team, Opp, season, Date, Location, TOV_perc, off_home_perc_TOV_perc:def_non_conf_away_perc_TOV_perc)
  
  features_matrix <- reference_df %>%
    select(all_of(relevant_features)) %>%
    as.matrix()
  
  weights <- importance_matrix_TOV_perc$Gain
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


output1_tov_perc <- comparison_tov_perc_func(.32, year = NULL, locationinput = "A")
output2_tov_perc <- comparison_tov_perc_func(.49, year = NULL, locationinput = "H")

nrow(output1_tov_perc)
nrow(output2_tov_perc)


output1_tov_perc %>% dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                                      off_away_perc_TOV_perc = mean(off_away_perc_TOV_perc, na.rm = T),
                                      off_conf_away_perc_TOV_perc = mean(off_conf_away_perc_TOV_perc, na.rm = T),
                                      off_non_conf_away_perc_TOV_perc = mean(off_non_conf_away_perc_TOV_perc, na.rm = T),
                                      def_away_perc_TOV_perc = mean(def_away_perc_TOV_perc, na.rm = T),
                                      def_conf_away_perc_TOV_perc  = mean(def_conf_away_perc_TOV_perc , na.rm = T),
                                      def_non_conf_away_perc_TOV_perc  = mean(def_non_conf_away_perc_TOV_perc , na.rm = T))

output1_tov_perc %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                   off_away_perc_TOV_perc = mean(off_away_perc_TOV_perc, na.rm = T),
                   off_conf_away_perc_TOV_perc = mean(off_conf_away_perc_TOV_perc, na.rm = T),
                   off_non_conf_away_perc_TOV_perc = mean(off_non_conf_away_perc_TOV_perc, na.rm = T),
                   def_away_perc_TOV_perc = mean(def_away_perc_TOV_perc, na.rm = T),
                   def_conf_away_perc_TOV_perc  = mean(def_conf_away_perc_TOV_perc , na.rm = T),
                   def_non_conf_away_perc_TOV_perc  = mean(def_non_conf_away_perc_TOV_perc , na.rm = T),
                   n = n())


output2_tov_perc %>% dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                                     off_home_perc_TOV_perc = mean(off_home_perc_TOV_perc, na.rm = T),
                                     off_conf_home_perc_TOV_perc = mean(off_conf_home_perc_TOV_perc, na.rm = T),
                                     off_non_conf_home_perc_TOV_perc = mean(off_non_conf_home_perc_TOV_perc, na.rm = T),
                                     def_home_perc_TOV_perc = mean(def_home_perc_TOV_perc, na.rm = T),
                                     def_conf_home_perc_TOV_perc  = mean(def_conf_home_perc_TOV_perc , na.rm = T),
                                     def_non_conf_home_perc_TOV_perc  = mean(def_non_conf_home_perc_TOV_perc , na.rm = T))

output2_tov_perc %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                   off_home_perc_TOV_perc = mean(off_home_perc_TOV_perc, na.rm = T),
                   off_conf_home_perc_TOV_perc = mean(off_conf_home_perc_TOV_perc, na.rm = T),
                   off_non_conf_home_perc_TOV_perc = mean(off_non_conf_home_perc_TOV_perc, na.rm = T),
                   def_home_perc_TOV_perc = mean(def_home_perc_TOV_perc, na.rm = T),
                   def_conf_home_perc_TOV_perc  = mean(def_conf_home_perc_TOV_perc , na.rm = T),
                   def_non_conf_home_perc_TOV_perc  = mean(def_non_conf_home_perc_TOV_perc , na.rm = T),
                   n = n())
