

team_comparison_ORB_perc_off <- function(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) {
  # Create prediction_df dynamically inside function
  offense_team_idx <- which(ORB_perc_df$Team == team_offense & ORB_perc_df$season == ssn)[1]
  defense_team_idx <- which(ORB_perc_df$Opp == opponent_defense & ORB_perc_df$season == ssn)[1]
  
  prediction_df_internal <- cbind(
    ORB_perc_df[offense_team_idx, c(21:137)],
    ORB_perc_df[defense_team_idx, c(138:254)],
    ORB_perc_df[offense_team_idx, c(255:332)],
    ORB_perc_df[defense_team_idx, c(333:410)],
    ORB_perc_df[offense_team_idx, c(411:416)],
    ORB_perc_df[defense_team_idx, c(417:422)]
  )
  
  prediction_df_internal$FC_height_diff <- prediction_df_internal$off_FC_height - prediction_df_internal$def_FC_height
  prediction_df_internal$G_height_diff <- prediction_df_internal$off_G_height - prediction_df_internal$def_G_height
  
  prediction_df_internal$FC_weight_diff <- prediction_df_internal$off_FC_weight - prediction_df_internal$def_FC_weight
  prediction_df_internal$G_weight_diff <- prediction_df_internal$off_G_weight - prediction_df_internal$def_G_weight
  
  prediction_df_internal$FC_class_diff <- prediction_df_internal$off_FC_class - prediction_df_internal$def_FC_class
  prediction_df_internal$G_class_diff <- prediction_df_internal$off_G_class - prediction_df_internal$def_G_class
  
  prediction_df_internal$LocationInd <- ifelse(locind == "A", -.89, 1.11)
  prediction_df_internal$ConferenceInd <- ifelse(confind == "N", -1.3375509, .747)
  prediction_df_internal$away_b2b_ind <- ifelse(awayb2bind == "N", -0.386, 2.59)
  
  off_prediction_df_internal <- prediction_df_internal[c(1:117, 235:312, 391:396, 403:411)]
  def_prediction_df_internal <- prediction_df_internal[c(118:234, 313:390, 397:411)]
  
  off_importance_matrix_ORB_perc_internal <- importance_matrix_ORB_perc[
    importance_matrix_ORB_perc$Feature %in% colnames(off_prediction_df_internal), ]
  
  def_importance_matrix_ORB_perc_internal <- importance_matrix_ORB_perc[
    importance_matrix_ORB_perc$Feature %in% colnames(def_prediction_df_internal), ]
  
  def_importance_matrix_ORB_perc_internal$Gain <- def_importance_matrix_ORB_perc_internal$Gain / sum(def_importance_matrix_ORB_perc_internal$Gain)
  rel_features_comp_def_ORB_perc <- def_importance_matrix_ORB_perc_internal$Feature
  
  # Chattanooga’s past opponent defenses from that season
  defense_profiles <- cbind(
    ORB_perc_df %>%
      filter(Team == team_offense, season == ssn) %>%
      select(Opp, Date, Location, ORB_perc, off_home_perc_ORB_perc:def_non_conf_away_perc_ORB_perc),
    ORB_perc_df %>%
      filter(Team == team_offense, season == ssn) %>%
      select(which(colnames(ORB_perc_df) %in% def_importance_matrix_ORB_perc_internal$Feature))
  )
  
  # Opponent defense profile for comparison
  opp_def_profile <- cbind(
    ORB_perc_df %>%
      filter(Opp == opponent_defense, season == ssn) %>%
      select(which(colnames(ORB_perc_df) %in% def_importance_matrix_ORB_perc_internal$Feature)) %>%
      select(1:201) %>%
      distinct(),
    prediction_df_internal[, c(403:411)]
  ) %>%
    select(rel_features_comp_def_ORB_perc) %>%
    as.numeric()
  
  defense_feature_matrix <- defense_profiles %>%
    select(all_of(rel_features_comp_def_ORB_perc)) %>%
    as.matrix()
  
  diff_matrix <- abs(sweep(defense_feature_matrix, 2, opp_def_profile, "-"))
  weights <- def_importance_matrix_ORB_perc_internal$Gain
  weighted_diff_matrix <- sweep(diff_matrix, 2, weights, "*")
  
  diff_scores <- rowSums(weighted_diff_matrix)
  
  result_df <- cbind(
    defense_profiles %>%
      select(Opp, Date, Location, ORB_perc, off_home_perc_ORB_perc:def_non_conf_away_perc_ORB_perc),
    weighted_diff_score = diff_scores
  ) %>%
    arrange(weighted_diff_score)
  
  return(result_df)
}

predict_weighted_ORB_perc_off <- function(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ORB_perc_off(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) %>%
    mutate(weight = pmax(1 - weighted_diff_score, 0)) %>%
    filter(weight > 0) %>%
    arrange(weighted_diff_score) %>%
    mutate(cum_weight = cumsum(weight))
  
  cutoff_row <- which(comp_table$cum_weight >= 2.5)[1]
  if (is.na(cutoff_row)) {
    cutoff_row <- nrow(comp_table)
  }
  
  comp_table_cut <- comp_table[1:cutoff_row, ]
  weighted_ORB_perc <- sum(comp_table_cut$ORB_perc * comp_table_cut$weight) / sum(comp_table_cut$weight)
  
  return(weighted_ORB_perc)
}

grouped_ORB_perc_summary_off <- function(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ORB_perc_off(team_offense, opponent_defense, ssn, locind, confind, awayb2bind)
  
  comp_table <- comp_table %>%
    mutate(weighted_diff_bucket = case_when(
      weighted_diff_score <= 0.5 ~ "1 - ≤0.5",
      weighted_diff_score <= 0.75 ~ "2 - 0.5–0.75",
      weighted_diff_score <= 1 ~ "3 - 0.75–1.0",
      TRUE ~ "4 - >1.0"
    ))
  
  bucket_summary <- comp_table %>%
    group_by(weighted_diff_bucket) %>%
    dplyr::summarise(
      mean_ORB_perc = mean(ORB_perc , na.rm = TRUE),
      mean_off_home_perc_ORB_perc = mean(off_home_perc_ORB_perc, na.rm = TRUE),
      mean_off_away_perc_ORB_perc = mean(off_away_perc_ORB_perc, na.rm = TRUE),
      mean_off_conf_home_perc_ORB_perc = mean(off_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_off_conf_away_perc_ORB_perc = mean(off_conf_away_perc_ORB_perc, na.rm = TRUE),
      mean_off_non_conf_home_perc_ORB_perc = mean(off_non_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_off_non_conf_away_perc_ORB_perc = mean(off_non_conf_away_perc_ORB_perc, na.rm = TRUE),
      mean_def_home_perc_ORB_perc = mean(def_home_perc_ORB_perc, na.rm = TRUE),
      mean_def_away_perc_ORB_perc = mean(def_away_perc_ORB_perc, na.rm = TRUE),
      mean_def_conf_home_perc_ORB_perc = mean(def_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_def_conf_away_perc_ORB_perc = mean(def_conf_away_perc_ORB_perc, na.rm = TRUE),
      mean_def_non_conf_home_perc_ORB_perc = mean(def_non_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_def_non_conf_away_perc_ORB_perc = mean(def_non_conf_away_perc_ORB_perc, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(weighted_diff_bucket)
  
  return(bucket_summary)
}



team_comparison_ORB_perc_off(off_string, def_string, 2025, "A", "N", 'Y')

team_comparison_ORB_perc_off(def_string, off_string, 2025, "A", "N", 'Y')


predict_weighted_ORB_perc_off(off_string, def_string, 2025, "A", "N", 'Y')

predict_weighted_ORB_perc_off(def_string, off_string, 2025, "A", "N", 'Y')


grouped_ORB_perc_summary_off(off_string, def_string, 2025, "A", "N", 'Y')

grouped_ORB_perc_summary_off(def_string, off_string, 2025, "A", "N", 'Y')




team_comparison_ORB_perc_def <- function(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) {
  # Create prediction_df dynamically inside function
  offense_team_idx <- which(ORB_perc_df$Team == opponent_offense & ORB_perc_df$season == ssn)[1]
  defense_team_idx <- which(ORB_perc_df$Opp == team_defense & ORB_perc_df$season == ssn)[1]
  
  prediction_df_internal <- cbind(
    ORB_perc_df[offense_team_idx, c(21:137)],
    ORB_perc_df[defense_team_idx, c(138:254)],
    ORB_perc_df[offense_team_idx, c(255:332)],
    ORB_perc_df[defense_team_idx, c(333:410)],
    ORB_perc_df[offense_team_idx, c(411:416)],
    ORB_perc_df[defense_team_idx, c(417:422)]
  )
  
  prediction_df_internal$FC_height_diff <- prediction_df_internal$off_FC_height - prediction_df_internal$def_FC_height
  prediction_df_internal$G_height_diff <- prediction_df_internal$off_G_height - prediction_df_internal$def_G_height
  
  prediction_df_internal$FC_weight_diff <- prediction_df_internal$off_FC_weight - prediction_df_internal$def_FC_weight
  prediction_df_internal$G_weight_diff <- prediction_df_internal$off_G_weight - prediction_df_internal$def_G_weight
  
  prediction_df_internal$FC_class_diff <- prediction_df_internal$off_FC_class - prediction_df_internal$def_FC_class
  prediction_df_internal$G_class_diff <- prediction_df_internal$off_G_class - prediction_df_internal$def_G_class
  
  prediction_df_internal$LocationInd <- ifelse(locind == "A", -.89, 1.11)
  prediction_df_internal$ConferenceInd <- ifelse(confind == "N", -1.3375509, .747)
  prediction_df_internal$away_b2b_ind <- ifelse(awayb2bind == "N", -0.386, 2.59)
  
  off_prediction_df_internal <- prediction_df_internal[c(1:117, 235:312, 391:396, 403:411)]
  def_prediction_df_internal <- prediction_df_internal[c(118:234, 313:390, 397:411)]
  
  off_importance_matrix_ORB_perc_internal <- importance_matrix_ORB_perc[
    importance_matrix_ORB_perc$Feature %in% colnames(off_prediction_df_internal), ]
  
  def_importance_matrix_ORB_perc_internal <- importance_matrix_ORB_perc[
    importance_matrix_ORB_perc$Feature %in% colnames(def_prediction_df_internal), ]
  
  def_importance_matrix_ORB_perc_internal$Gain <- def_importance_matrix_ORB_perc_internal$Gain / sum(def_importance_matrix_ORB_perc_internal$Gain)
  rel_features_comp_def_ORB_perc <- def_importance_matrix_ORB_perc_internal$Feature
  
  off_importance_matrix_ORB_perc_internal$Gain <- off_importance_matrix_ORB_perc_internal$Gain / sum(off_importance_matrix_ORB_perc_internal$Gain)
  rel_features_comp_off_ORB_perc <- off_importance_matrix_ORB_perc_internal$Feature
  
  # Chattanooga’s past opponent defenses from that season
  offense_profiles <- cbind(
    ORB_perc_df %>%
      filter(Opp == team_defense, season == ssn) %>%
      select(Team, Date, Location, ORB_perc, off_home_perc_ORB_perc:def_non_conf_away_perc_ORB_perc),
    ORB_perc_df %>%
      filter(Opp == team_defense, season == ssn) %>%
      select(which(colnames(ORB_perc_df) %in% off_importance_matrix_ORB_perc_internal$Feature))
  )
  
  # Opponent defense profile for comparison
  opp_off_profile <- cbind(
    ORB_perc_df %>%
      filter(Team == opponent_offense, season == ssn) %>%
      select(which(colnames(ORB_perc_df) %in% off_importance_matrix_ORB_perc_internal$Feature)) %>%
      select(1:201) %>%
      distinct(),
    prediction_df_internal[, c(403:411)]
  ) %>%
    select(rel_features_comp_off_ORB_perc) %>%
    as.numeric()
  
  offense_feature_matrix <- offense_profiles %>%
    select(all_of(rel_features_comp_off_ORB_perc)) %>%
    as.matrix()
  
  diff_matrix <- abs(sweep(offense_feature_matrix, 2, opp_off_profile, "-"))
  weights <- off_importance_matrix_ORB_perc_internal$Gain
  weighted_diff_matrix <- sweep(diff_matrix, 2, weights, "*")
  
  diff_scores <- rowSums(weighted_diff_matrix)
  
  result_df <- cbind(
    offense_profiles %>%
      select(Team, Date, Location, ORB_perc, off_home_perc_ORB_perc:def_non_conf_away_perc_ORB_perc),
    weighted_diff_score = diff_scores
  ) %>%
    arrange(weighted_diff_score)
  
  return(result_df)
}

predict_weighted_ORB_perc_def <- function(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ORB_perc_def(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) %>%
    mutate(weight = pmax(1 - weighted_diff_score, 0)) %>%
    filter(weight > 0) %>%
    arrange(weighted_diff_score) %>%
    mutate(cum_weight = cumsum(weight))
  
  cutoff_row <- which(comp_table$cum_weight >= 2.5)[1]
  if (is.na(cutoff_row)) {
    cutoff_row <- nrow(comp_table)
  }
  
  comp_table_cut <- comp_table[1:cutoff_row, ]
  weighted_ORB_perc <- sum(comp_table_cut$ORB_perc * comp_table_cut$weight) / sum(comp_table_cut$weight)
  
  return(weighted_ORB_perc)
}

grouped_ORB_perc_summary_def <- function(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ORB_perc_def(team_defense, opponent_offense, ssn, locind, confind, awayb2bind)
  
  comp_table <- comp_table %>%
    mutate(weighted_diff_bucket = case_when(
      weighted_diff_score <= 0.5 ~ "1 - ≤0.5",
      weighted_diff_score <= 0.75 ~ "2 - 0.5–0.75",
      weighted_diff_score <= 1 ~ "3 - 0.75–1.0",
      TRUE ~ "4 - >1.0"
    ))
  
  bucket_summary <- comp_table %>%
    group_by(weighted_diff_bucket) %>%
    dplyr::summarise(
      mean_ORB_perc = mean(ORB_perc, na.rm = TRUE),
      mean_off_home_perc_ORB_perc = mean(off_home_perc_ORB_perc, na.rm = TRUE),
      mean_off_away_perc_ORB_perc = mean(off_away_perc_ORB_perc, na.rm = TRUE),
      mean_off_conf_home_perc_ORB_perc = mean(off_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_off_conf_away_perc_ORB_perc = mean(off_conf_away_perc_ORB_perc, na.rm = TRUE),
      mean_off_non_conf_home_perc_ORB_perc = mean(off_non_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_off_non_conf_away_perc_ORB_perc = mean(off_non_conf_away_perc_ORB_perc, na.rm = TRUE),
      mean_def_home_perc_ORB_perc = mean(def_home_perc_ORB_perc, na.rm = TRUE),
      mean_def_away_perc_ORB_perc = mean(def_away_perc_ORB_perc, na.rm = TRUE),
      mean_def_conf_home_perc_ORB_perc = mean(def_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_def_conf_away_perc_ORB_perc = mean(def_conf_away_perc_ORB_perc, na.rm = TRUE),
      mean_def_non_conf_home_perc_ORB_perc = mean(def_non_conf_home_perc_ORB_perc, na.rm = TRUE),
      mean_def_non_conf_away_perc_ORB_perc = mean(def_non_conf_away_perc_ORB_perc, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(weighted_diff_bucket)
  
  return(bucket_summary)
}


team_comparison_ORB_perc_def(off_string, def_string, 2025, "A", "N", 'Y')

team_comparison_ORB_perc_def(def_string, off_string, 2025, "A", "N", 'Y')


predict_weighted_ORB_perc_def(off_string, def_string, 2025, "A", "N", 'Y')

predict_weighted_ORB_perc_def(def_string, off_string, 2025, "A", "N", 'Y')


grouped_ORB_perc_summary_def(off_string, def_string, 2025, "A", "N", 'Y')

grouped_ORB_perc_summary_def(def_string, off_string, 2025, "A", "N", 'Y')
