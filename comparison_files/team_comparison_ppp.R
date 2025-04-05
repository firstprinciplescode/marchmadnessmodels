### TURNS OUT, I MAY HAVE TO SEPARATE THEM AFTER ALL
### THAT WAS ALWAYS THE BASIS OF MY PREDICTIONS - WEIGHING PERFORMANCE DURING THE YEAR OF THE TEAMS, NOT FINDING SIMILAR GAMES THROUGHOUT ALL SEASONS
### WE'RE GOING TO HAVE TO START LIKE IT WAS IN THE BEGINNING.


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



####
####
####


team_comparison_ppp_off <- function(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) {
  # Create prediction_df dynamically inside function
  offense_team_idx <- which(ppp_final_df$Team == team_offense & ppp_final_df$season == ssn)[1]
  defense_team_idx <- which(ppp_final_df$Opp == opponent_defense & ppp_final_df$season == ssn)[1]

  prediction_df_internal <- cbind(
    ppp_final_df[offense_team_idx, c(21:137)],
    ppp_final_df[defense_team_idx, c(138:254)],
    ppp_final_df[offense_team_idx, c(255:332)],
    ppp_final_df[defense_team_idx, c(333:410)],
    ppp_final_df[offense_team_idx, c(411:416)],
    ppp_final_df[defense_team_idx, c(417:422)]
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

  off_importance_matrix_ppp_internal <- importance_matrix_press_off[
    importance_matrix_press_off$Feature %in% colnames(off_prediction_df_internal), ]

  def_importance_matrix_ppp_internal <- importance_matrix_press_off[
    importance_matrix_press_off$Feature %in% colnames(def_prediction_df_internal), ]

  def_importance_matrix_ppp_internal$Gain <- def_importance_matrix_ppp_internal$Gain / sum(def_importance_matrix_ppp_internal$Gain)
  rel_features_comp_def_ppp <- def_importance_matrix_ppp_internal$Feature

  # Chattanooga’s past opponent defenses from that season
  defense_profiles <- cbind(
    ppp_final_df %>%
      filter(Team == team_offense, season == ssn) %>%
      select(Opp, Date, Location, PPP, off_home_perc_PPP:def_non_conf_away_perc_PPP),
    ppp_final_df %>%
      filter(Team == team_offense, season == ssn) %>%
      select(which(colnames(ppp_final_df) %in% def_importance_matrix_ppp_internal$Feature))
  )

  # Opponent defense profile for comparison
  opp_def_profile <- cbind(
    ppp_final_df %>%
      filter(Opp == opponent_defense, season == ssn) %>%
      select(which(colnames(ppp_final_df) %in% def_importance_matrix_ppp_internal$Feature)) %>%
      select(1:201) %>%
      distinct(),
    prediction_df_internal[, c(403:411)]
  ) %>%
    select(rel_features_comp_def_ppp) %>%
    as.numeric()

  defense_feature_matrix <- defense_profiles %>%
    select(all_of(rel_features_comp_def_ppp)) %>%
    as.matrix()

  diff_matrix <- abs(sweep(defense_feature_matrix, 2, opp_def_profile, "-"))
  weights <- def_importance_matrix_ppp_internal$Gain
  weighted_diff_matrix <- sweep(diff_matrix, 2, weights, "*")

  diff_scores <- rowSums(weighted_diff_matrix)

  result_df <- cbind(
    defense_profiles %>%
      select(Opp, Date, Location, PPP, off_home_perc_PPP:def_non_conf_away_perc_PPP),
    weighted_diff_score = diff_scores
  ) %>%
    arrange(weighted_diff_score)

  return(result_df)
}

predict_weighted_ppp_off <- function(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ppp_off(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) %>%
    mutate(weight = pmax(1 - weighted_diff_score, 0)) %>%
    filter(weight > 0) %>%
    arrange(weighted_diff_score) %>%
    mutate(cum_weight = cumsum(weight))

  cutoff_row <- which(comp_table$cum_weight >= 2.5)[1]
  if (is.na(cutoff_row)) {
    cutoff_row <- nrow(comp_table)
  }

  comp_table_cut <- comp_table[1:cutoff_row, ]
  weighted_ppp <- sum(comp_table_cut$PPP * comp_table_cut$weight) / sum(comp_table_cut$weight)

  return(weighted_ppp)
}

grouped_ppp_summary_off <- function(team_offense, opponent_defense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ppp_off(team_offense, opponent_defense, ssn, locind, confind, awayb2bind)

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
      mean_PPP = mean(PPP, na.rm = TRUE),
      mean_off_home_perc_PPP = mean(off_home_perc_PPP, na.rm = TRUE),
      mean_off_away_perc_PPP = mean(off_away_perc_PPP, na.rm = TRUE),
      mean_off_conf_home_perc_PPP = mean(off_conf_home_perc_PPP, na.rm = TRUE),
      mean_off_conf_away_perc_PPP = mean(off_conf_away_perc_PPP, na.rm = TRUE),
      mean_off_non_conf_home_perc_PPP = mean(off_non_conf_home_perc_PPP, na.rm = TRUE),
      mean_off_non_conf_away_perc_PPP = mean(off_non_conf_away_perc_PPP, na.rm = TRUE),
      mean_def_home_perc_PPP = mean(def_home_perc_PPP, na.rm = TRUE),
      mean_def_away_perc_PPP = mean(def_away_perc_PPP, na.rm = TRUE),
      mean_def_conf_home_perc_PPP = mean(def_conf_home_perc_PPP, na.rm = TRUE),
      mean_def_conf_away_perc_PPP = mean(def_conf_away_perc_PPP, na.rm = TRUE),
      mean_def_non_conf_home_perc_PPP = mean(def_non_conf_home_perc_PPP, na.rm = TRUE),
      mean_def_non_conf_away_perc_PPP = mean(def_non_conf_away_perc_PPP, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(weighted_diff_bucket)

  return(bucket_summary)
}




team_comparison_ppp_off(off_string, def_string, 2025, "A", "N", "N")

predict_weighted_ppp_off(off_string, def_string, 2025, "A", "N", "N")

grouped_ppp_summary_off(off_string, def_string, 2025, "A", "N", "N")



team_comparison_ppp_off(def_string, off_string, 2025, "A", "N", "N")

predict_weighted_ppp_off(def_string, off_string, 2025, "A", "N", "N")

grouped_ppp_summary_off(def_string, off_string, 2025, "A", "N", "N")




### IF WE'RE WORRIED ABOUT THE OFFENSE, WE LOOK AT THE DEFENSE, VICE VERSA
### WE'RE GOING TO HAVE TO DO THE SAME FUNCTION BUT REVERSE FOR DEF
### AS FOR RESULT ... WE SHOULD SPLIT UP BY CATEGORY
### .5 AND UNDER, .501 - .75, .75 - 1, 1+
### ALSO, SUBTRACT THE WEIGHTED_DIFF_SCORE FROM 1 --> GET THEIR WEIGHTS (UP TO A SUM OF 2.5)
### I ONLY CARE ENOUGH TO DO THE SCORE FOR PPP.
###


team_comparison_ppp_def <- function(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) {
  # Create prediction_df dynamically inside function
  offense_team_idx <- which(ppp_final_df$Team == opponent_offense & ppp_final_df$season == ssn)[1]
  defense_team_idx <- which(ppp_final_df$Opp == team_defense & ppp_final_df$season == ssn)[1]

  prediction_df_internal <- cbind(
      ppp_final_df[offense_team_idx, c(21:137)],
      ppp_final_df[defense_team_idx, c(138:254)],
      ppp_final_df[offense_team_idx, c(255:332)],
      ppp_final_df[defense_team_idx, c(333:410)],
      ppp_final_df[offense_team_idx, c(411:416)],
      ppp_final_df[defense_team_idx, c(417:422)]
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

  off_importance_matrix_ppp_internal <- importance_matrix_press_off[
    importance_matrix_press_off$Feature %in% colnames(off_prediction_df_internal), ]

  def_importance_matrix_ppp_internal <- importance_matrix_press_off[
    importance_matrix_press_off$Feature %in% colnames(def_prediction_df_internal), ]

  def_importance_matrix_ppp_internal$Gain <- def_importance_matrix_ppp_internal$Gain / sum(def_importance_matrix_ppp_internal$Gain)
  rel_features_comp_def_ppp <- def_importance_matrix_ppp_internal$Feature

  off_importance_matrix_ppp_internal$Gain <- off_importance_matrix_ppp_internal$Gain / sum(off_importance_matrix_ppp_internal$Gain)
  rel_features_comp_off_ppp <- off_importance_matrix_ppp_internal$Feature

  # Chattanooga’s past opponent defenses from that season
  offense_profiles <- cbind(
    ppp_final_df %>%
      filter(Opp == team_defense, season == ssn) %>%
      select(Team, Date, Location, PPP, off_home_perc_PPP:def_non_conf_away_perc_PPP),
    ppp_final_df %>%
      filter(Opp == team_defense, season == ssn) %>%
      select(which(colnames(ppp_final_df) %in% off_importance_matrix_ppp_internal$Feature))
  )

  # Opponent defense profile for comparison
  opp_off_profile <- cbind(
    ppp_final_df %>%
      filter(Team == opponent_offense, season == ssn) %>%
      select(which(colnames(ppp_final_df) %in% off_importance_matrix_ppp_internal$Feature)) %>%
      select(1:201) %>%
      distinct(),
    prediction_df_internal[, c(403:411)]
  ) %>%
    select(rel_features_comp_off_ppp) %>%
    as.numeric()

  offense_feature_matrix <- offense_profiles %>%
    select(all_of(rel_features_comp_off_ppp)) %>%
    as.matrix()

  diff_matrix <- abs(sweep(offense_feature_matrix, 2, opp_off_profile, "-"))
  weights <- off_importance_matrix_ppp_internal$Gain
  weighted_diff_matrix <- sweep(diff_matrix, 2, weights, "*")

  diff_scores <- rowSums(weighted_diff_matrix)

  result_df <- cbind(
    offense_profiles %>%
      select(Team, Date, Location, PPP, off_home_perc_PPP:def_non_conf_away_perc_PPP),
    weighted_diff_score = diff_scores
  ) %>%
    arrange(weighted_diff_score)

  return(result_df)
}

predict_weighted_ppp_def <- function(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ppp_def(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) %>%
    mutate(weight = pmax(1 - weighted_diff_score, 0)) %>%
    filter(weight > 0) %>%
    arrange(weighted_diff_score) %>%
    mutate(cum_weight = cumsum(weight))

  cutoff_row <- which(comp_table$cum_weight >= 2.5)[1]
  if (is.na(cutoff_row)) {
    cutoff_row <- nrow(comp_table)
  }

  comp_table_cut <- comp_table[1:cutoff_row, ]
  weighted_ppp <- sum(comp_table_cut$PPP * comp_table_cut$weight) / sum(comp_table_cut$weight)

  return(weighted_ppp)
}

grouped_ppp_summary_def <- function(team_defense, opponent_offense, ssn, locind, confind, awayb2bind) {
  comp_table <- team_comparison_ppp_def(team_defense, opponent_offense, ssn, locind, confind, awayb2bind)

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
      mean_PPP = mean(PPP, na.rm = TRUE),
      mean_off_home_perc_PPP = mean(off_home_perc_PPP, na.rm = TRUE),
      mean_off_away_perc_PPP = mean(off_away_perc_PPP, na.rm = TRUE),
      mean_off_conf_home_perc_PPP = mean(off_conf_home_perc_PPP, na.rm = TRUE),
      mean_off_conf_away_perc_PPP = mean(off_conf_away_perc_PPP, na.rm = TRUE),
      mean_off_non_conf_home_perc_PPP = mean(off_non_conf_home_perc_PPP, na.rm = TRUE),
      mean_off_non_conf_away_perc_PPP = mean(off_non_conf_away_perc_PPP, na.rm = TRUE),
      mean_def_home_perc_PPP = mean(def_home_perc_PPP, na.rm = TRUE),
      mean_def_away_perc_PPP = mean(def_away_perc_PPP, na.rm = TRUE),
      mean_def_conf_home_perc_PPP = mean(def_conf_home_perc_PPP, na.rm = TRUE),
      mean_def_conf_away_perc_PPP = mean(def_conf_away_perc_PPP, na.rm = TRUE),
      mean_def_non_conf_home_perc_PPP = mean(def_non_conf_home_perc_PPP, na.rm = TRUE),
      mean_def_non_conf_away_perc_PPP = mean(def_non_conf_away_perc_PPP, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(weighted_diff_bucket)

  return(bucket_summary)
}



team_comparison_ppp_def(off_string, def_string, 2025, "A", "N", "N")

team_comparison_ppp_def(def_string, off_string, 2025, "A", "N", "N")


predict_weighted_ppp_def(off_string, def_string, 2025, "A", "N", "N")

predict_weighted_ppp_def(def_string, off_string, 2025, "A", "N", "N")


grouped_ppp_summary_def(off_string, def_string, 2025, "A", "N", "N")

grouped_ppp_summary_def(def_string, off_string, 2025, "A", "N", "N")
