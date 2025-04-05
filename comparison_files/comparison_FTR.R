

FTR_df <- gamelog_comb %>% 
  select(Team, Opp, season, game_id, home_perc_FTR, away_perc_FTR, conf_home_perc_FTR, conf_away_perc_FTR, non_conf_home_perc_FTR, non_conf_away_perc_FTR, def_home_perc_FTR, def_away_perc_FTR, def_conf_home_perc_FTR, def_conf_away_perc_FTR, def_non_conf_home_perc_FTR, def_non_conf_away_perc_FTR)
colnames(FTR_df)[c(5:10)] <- paste0("off_", colnames(FTR_df)[c(5:10)])

FTR_df <- inner_join(final_df, FTR_df, by = c("Team", "Opp", "game_id"))

colnames(FTR_df) <- gsub("%", "", colnames(FTR_df))


###
###
###


off_string = "Chattanooga"
def_string = "UC Irvine"

offense_team <- which(FTR_df$Team == off_string & FTR_df$season == 2025)[1]
defense_team <- which(FTR_df$Opp == def_string & FTR_df$season == 2025)[1]

prediction_df <- cbind(FTR_df[offense_team,c(21:137)], FTR_df[defense_team,c(138:254)], FTR_df[offense_team,c(255:332)], FTR_df[defense_team,c(333:410)], FTR_df[offense_team,c(411:416)], FTR_df[defense_team,c(417:423)])

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


predict(xgb_FTR_route, as.matrix(prediction_df))


comparison_FTR_func <- function(func_pred_df, threshold, year = NULL, locationinput = NULL) {
  current_game_vector <- func_pred_df
  reference_df <- cbind(
    FTR_df %>% select(Team, Opp, season, Date, Location, FTR, off_home_perc_FTR:def_non_conf_away_perc_FTR),
    FTR_df[, which(colnames(FTR_df) %in% importance_matrix_FTR$Feature)]
  )
  
  relevant_features <- importance_matrix_FTR$Feature
  current_game <- current_game_vector[relevant_features] %>% as.numeric()
  
  metadata_cols <- reference_df %>%
    select(Team, Opp, season, Date, Location, FTR, off_home_perc_FTR:def_non_conf_away_perc_FTR)
  
  features_matrix <- reference_df %>%
    select(all_of(relevant_features)) %>%
    as.matrix()
  
  weights <- importance_matrix_FTR$Gain
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


output1_FTR <- comparison_FTR_func(prediction_df, .45, year = NULL, locationinput = "A")
output2_FTR <- comparison_FTR_func(prediction_df, .66, year = NULL, locationinput = "H")

output1_FTR %>% dplyr::summarise(FTR = mean(FTR, na.rm = T),
                                      off_away_perc_FTR = mean(off_away_perc_FTR, na.rm = T),
                                      off_conf_away_perc_FTR = mean(off_conf_away_perc_FTR, na.rm = T),
                                      off_non_conf_away_perc_FTR = mean(off_non_conf_away_perc_FTR, na.rm = T),
                                      def_away_perc_FTR = mean(def_away_perc_FTR, na.rm = T),
                                      def_conf_away_perc_FTR  = mean(def_conf_away_perc_FTR , na.rm = T),
                                      def_non_conf_away_perc_FTR  = mean(def_non_conf_away_perc_FTR , na.rm = T))

output1_FTR %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(FTR = mean(FTR, na.rm = T),
                   off_away_perc_FTR = mean(off_away_perc_FTR, na.rm = T),
                   off_conf_away_perc_FTR = mean(off_conf_away_perc_FTR, na.rm = T),
                   off_non_conf_away_perc_FTR = mean(off_non_conf_away_perc_FTR, na.rm = T),
                   def_away_perc_FTR = mean(def_away_perc_FTR, na.rm = T),
                   def_conf_away_perc_FTR  = mean(def_conf_away_perc_FTR , na.rm = T),
                   def_non_conf_away_perc_FTR  = mean(def_non_conf_away_perc_FTR , na.rm = T),
                   n = n())


output2_FTR %>% dplyr::summarise(FTR = mean(FTR, na.rm = T),
                                      off_home_perc_FTR = mean(off_home_perc_FTR, na.rm = T),
                                      off_conf_home_perc_FTR = mean(off_conf_home_perc_FTR, na.rm = T),
                                      off_non_conf_home_perc_FTR = mean(off_non_conf_home_perc_FTR, na.rm = T),
                                      def_home_perc_FTR = mean(def_home_perc_FTR, na.rm = T),
                                      def_conf_home_perc_FTR  = mean(def_conf_home_perc_FTR , na.rm = T),
                                      def_non_conf_home_perc_FTR  = mean(def_non_conf_home_perc_FTR , na.rm = T))

output2_FTR %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(FTR = mean(FTR, na.rm = T),
                   off_home_perc_FTR = mean(off_home_perc_FTR, na.rm = T),
                   off_conf_home_perc_FTR = mean(off_conf_home_perc_FTR, na.rm = T),
                   off_non_conf_home_perc_FTR = mean(off_non_conf_home_perc_FTR, na.rm = T),
                   def_home_perc_FTR = mean(def_home_perc_FTR, na.rm = T),
                   def_conf_home_perc_FTR  = mean(def_conf_home_perc_FTR , na.rm = T),
                   def_non_conf_home_perc_FTR  = mean(def_non_conf_home_perc_FTR , na.rm = T),
                   n = n())
