
off_string = "Houston"
def_string = "Florida"


offense_team <- which(ppp_final_df$Team == off_string & ppp_final_df$season == 2025)[1]
defense_team <- which(ppp_final_df$Opp == def_string & ppp_final_df$season == 2025)[1]

prediction_df <- cbind(ppp_final_df[offense_team,c(21:137)], ppp_final_df[defense_team,c(138:254)], ppp_final_df[offense_team,c(255:332)], ppp_final_df[defense_team,c(333:410)], ppp_final_df[offense_team,c(411:416)], ppp_final_df[defense_team,c(417:423)])

prediction_df$FC_height_diff <- prediction_df$off_FC_height - prediction_df$def_FC_height
prediction_df$G_height_diff <- prediction_df$off_G_height - prediction_df$def_G_height

prediction_df$FC_weight_diff <- prediction_df$off_FC_weight - prediction_df$def_FC_weight
prediction_df$G_weight_diff <- prediction_df$off_G_weight - prediction_df$def_G_weight

prediction_df$FC_class_diff <- prediction_df$off_FC_class - prediction_df$def_FC_class
prediction_df$G_class_diff <- prediction_df$off_G_class - prediction_df$def_G_class

prediction_df$LocationInd = -.891
prediction_df$ConferenceInd = -1.301
prediction_df$away_b2b_ind = 2.503

colnames(prediction_df) <- gsub("%", "", colnames(prediction_df))


####
####
####


offense_team_2 <- which(ppp_final_df$Team == def_string & ppp_final_df$season == 2025)[1]
defense_team_2 <- which(ppp_final_df$Opp == off_string & ppp_final_df$season == 2025)[1]

prediction_df_2 <- cbind(ppp_final_df[offense_team_2,c(21:137)], ppp_final_df[defense_team_2,c(138:254)], ppp_final_df[offense_team_2,c(255:332)], ppp_final_df[defense_team_2,c(333:410)], ppp_final_df[offense_team_2,c(411:416)], ppp_final_df[defense_team_2,c(417:423)])

prediction_df_2$FC_height_diff <- prediction_df_2$off_FC_height - prediction_df_2$def_FC_height
prediction_df_2$G_height_diff <- prediction_df_2$off_G_height - prediction_df_2$def_G_height

prediction_df_2$FC_weight_diff <- prediction_df_2$off_FC_weight - prediction_df_2$def_FC_weight
prediction_df_2$G_weight_diff <- prediction_df_2$off_G_weight - prediction_df_2$def_G_weight

prediction_df_2$FC_class_diff <- prediction_df_2$off_FC_class - prediction_df_2$def_FC_class
prediction_df_2$G_class_diff <- prediction_df_2$off_G_class - prediction_df_2$def_G_class

prediction_df_2$LocationInd = -.891
prediction_df_2$ConferenceInd = -1.301
prediction_df_2$away_b2b_ind = 2.503

colnames(prediction_df_2) <- gsub("%", "", colnames(prediction_df_2))


####
####
####


team_one_vec <- c(predict(xgb_press_off_route, as.matrix(prediction_df)), predict(xgb_twoptperc_route, as.matrix(prediction_df)), predict(xgb_threeptperc_route, as.matrix(prediction_df)), predict(xgb_STL_perc_route, as.matrix(prediction_df)), predict(xgb_TOV_perc_route, as.matrix(prediction_df)) - predict(xgb_STL_perc_route, as.matrix(prediction_df)), predict(xgb_ORB_perc_route, as.matrix(prediction_df)), predict(xgb_FTR_route, as.matrix(prediction_df)), predict(xgb_poss_route, as.matrix(prediction_df)))
names(team_one_vec) <- c("PPP", "2P", "3P", "STL", "DTO", "ORB", "FTR", "POSS")


####
####
####


team_two_vec <- c(predict(xgb_press_off_route, as.matrix(prediction_df_2)), predict(xgb_twoptperc_route, as.matrix(prediction_df_2)), predict(xgb_threeptperc_route, as.matrix(prediction_df_2)), predict(xgb_STL_perc_route, as.matrix(prediction_df_2)), predict(xgb_TOV_perc_route, as.matrix(prediction_df_2)) - predict(xgb_STL_perc_route, as.matrix(prediction_df_2)), predict(xgb_ORB_perc_route, as.matrix(prediction_df_2)), predict(xgb_FTR_route, as.matrix(prediction_df_2)), predict(xgb_poss_route, as.matrix(prediction_df_2)))
names(team_two_vec) <- c("PPP", "2P", "3P", "STL", "DTO", "ORB", "FTR", "POSS")

reg_vec <- as.data.frame(rbind(team_one_vec, team_two_vec))
rownames(reg_vec) <- c(off_string, def_string)


####
####
####



comp_team_one_vec <- as.data.frame(rbind(c(predict_weighted_ppp_off(off_string, def_string, 2025, "A", "N", "N"), predict_weighted_twoptperc_off(off_string, def_string, 2025, "A", "N", "N"), predict_weighted_threeptperc_off(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_STL_perc_off(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_TOV_perc_off(off_string, def_string, 2025, "A", "N", 'N') - predict_weighted_STL_perc_off(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_ORB_perc_off(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_FTR_off(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_poss_off(off_string, def_string, 2025, "A", "N", 'N')), 
c(predict_weighted_ppp_def(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_twopt_def(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_threept_def(def_string, off_string, 2025, "A", "N", 'N'), predict_weighted_STL_perc_def(def_string, off_string, 2025, "A", "N", 'N'), predict_weighted_TOV_perc_def(def_string, off_string, 2025, "A", "N", 'N') - predict_weighted_STL_perc_def(def_string, off_string, 2025, "A", "N", 'N'), predict_weighted_ORB_perc_def(def_string, off_string, 2025, "A", "N", 'N'), predict_weighted_FTR_def(def_string, off_string, 2025, "A", "N", 'N'), predict_weighted_poss_def(def_string, off_string, 2025, "A", "N", 'N'))))
colnames(comp_team_one_vec) <- c("PPP", "2P", "3P", "STL", "DTO", "ORB", "FTR", "POSS")
rownames(comp_team_one_vec) <- c("OFF", "DEF")


####
####
####



comp_team_two_vec <- as.data.frame(rbind(c(predict_weighted_ppp_off(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_twoptperc_off(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_threeptperc_off(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_STL_perc_off(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_TOV_perc_off(def_string, off_string, 2025, "A", "N", "N") - predict_weighted_STL_perc_off(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_ORB_perc_off(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_FTR_off(def_string, off_string, 2025, "A", "N", "N"), predict_weighted_poss_off(def_string, off_string, 2025, "A", "N", "N")),
c(predict_weighted_ppp_def(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_twopt_def(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_threept_def(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_STL_perc_def(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_TOV_perc_def(off_string, def_string, 2025, "A", "N", 'N') - predict_weighted_STL_perc_def(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_ORB_perc_def(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_FTR_def(off_string, def_string, 2025, "A", "N", 'N'), predict_weighted_poss_def(off_string, def_string, 2025, "A", "N", 'N'))))
colnames(comp_team_two_vec) <- c("PPP", "2P", "3P", "STL", "DTO", "ORB", "FTR", "POSS")
rownames(comp_team_two_vec) <- c("OFF", "DEF")



####
####
####


output1_ppp <- comparison_game_func(prediction_df, .53, year = NULL, locationinput = "A")
output2_ppp <- comparison_game_func(prediction_df, .75, year = NULL, locationinput = "H")

output1_ppp %>% filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(PPP = mean(PPP, na.rm = T),
                   off_away_perc_PPP = mean(off_away_perc_PPP, na.rm = T),
                   off_conf_away_perc_PPP  = mean(off_conf_away_perc_PPP , na.rm = T),
                   off_non_conf_away_perc_PPP  = mean(off_non_conf_away_perc_PPP , na.rm = T),
                   def_away_perc_PPP = mean(def_away_perc_PPP, na.rm = T),
                   def_conf_away_perc_PPP  = mean(def_conf_away_perc_PPP , na.rm = T),
                   def_non_conf_away_perc_PPP  = mean(def_non_conf_away_perc_PPP , na.rm = T),
                   n = n())

output2_ppp %>% filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(PPP = mean(PPP, na.rm = T),
                   off_home_perc_PPP = mean(off_home_perc_PPP, na.rm = T),
                   off_conf_home_perc_PPP  = mean(off_conf_home_perc_PPP , na.rm = T),
                   off_non_conf_home_perc_PPP  = mean(off_non_conf_home_perc_PPP , na.rm = T),
                   def_home_perc_PPP = mean(def_home_perc_PPP, na.rm = T),
                   def_conf_home_perc_PPP  = mean(def_conf_home_perc_PPP , na.rm = T),
                   def_non_conf_home_perc_PPP  = mean(def_non_conf_home_perc_PPP , na.rm = T),
                   n = n())


team_one_ppp <- cbind(output1_ppp %>% dplyr::summarise(PPP = mean(PPP, na.rm = T),
                             off_away_perc_PPP = mean(off_away_perc_PPP, na.rm = T),
                             off_conf_away_perc_PPP  = mean(off_conf_away_perc_PPP , na.rm = T),
                             off_non_conf_away_perc_PPP  = mean(off_non_conf_away_perc_PPP , na.rm = T),
                             def_away_perc_PPP = mean(def_away_perc_PPP, na.rm = T),
                             def_conf_away_perc_PPP  = mean(def_conf_away_perc_PPP , na.rm = T),
                             def_non_conf_away_perc_PPP  = mean(def_non_conf_away_perc_PPP , na.rm = T)), 
output2_ppp %>% dplyr::summarise(PPP = mean(PPP, na.rm = T),
                             off_home_perc_PPP = mean(off_home_perc_PPP, na.rm = T),
                             off_conf_home_perc_PPP  = mean(off_conf_home_perc_PPP , na.rm = T),
                             off_non_conf_home_perc_PPP  = mean(off_non_conf_home_perc_PPP , na.rm = T),
                             def_home_perc_PPP = mean(def_home_perc_PPP, na.rm = T),
                             def_conf_home_perc_PPP  = mean(def_conf_home_perc_PPP , na.rm = T),
                             def_non_conf_home_perc_PPP  = mean(def_non_conf_home_perc_PPP , na.rm = T)))




output1_ppp_2 <- comparison_game_func(prediction_df_2, .53, year = NULL, locationinput = "A")
output2_ppp_2 <- comparison_game_func(prediction_df_2, .76, year = NULL, locationinput = "H")

output1_ppp_2 %>% filter((Team != def_string | Opp != off_string) & season != 2025) %>% 
  dplyr::summarise(PPP = mean(PPP, na.rm = T),
                   off_away_perc_PPP = mean(off_away_perc_PPP, na.rm = T),
                   off_conf_away_perc_PPP  = mean(off_conf_away_perc_PPP , na.rm = T),
                   off_non_conf_away_perc_PPP  = mean(off_non_conf_away_perc_PPP , na.rm = T),
                   def_away_perc_PPP = mean(def_away_perc_PPP, na.rm = T),
                   def_conf_away_perc_PPP  = mean(def_conf_away_perc_PPP , na.rm = T),
                   def_non_conf_away_perc_PPP  = mean(def_non_conf_away_perc_PPP , na.rm = T),
                   n = n())

output2_ppp_2 %>% filter((Team != def_string | Opp != off_string) & season != 2025) %>% 
  dplyr::summarise(PPP = mean(PPP, na.rm = T),
                   off_home_perc_PPP = mean(off_home_perc_PPP, na.rm = T),
                   off_conf_home_perc_PPP  = mean(off_conf_home_perc_PPP , na.rm = T),
                   off_non_conf_home_perc_PPP  = mean(off_non_conf_home_perc_PPP , na.rm = T),
                   def_home_perc_PPP = mean(def_home_perc_PPP, na.rm = T),
                   def_conf_home_perc_PPP  = mean(def_conf_home_perc_PPP , na.rm = T),
                   def_non_conf_home_perc_PPP  = mean(def_non_conf_home_perc_PPP , na.rm = T),
                   n = n())


team_two_ppp <- cbind(output1_ppp_2 %>% dplyr::summarise(PPP = mean(PPP, na.rm = T),
                                 off_away_perc_PPP = mean(off_away_perc_PPP, na.rm = T),
                                 off_conf_away_perc_PPP  = mean(off_conf_away_perc_PPP , na.rm = T),
                                 off_non_conf_away_perc_PPP  = mean(off_non_conf_away_perc_PPP , na.rm = T),
                                 def_away_perc_PPP = mean(def_away_perc_PPP, na.rm = T),
                                 def_conf_away_perc_PPP  = mean(def_conf_away_perc_PPP , na.rm = T),
                                 def_non_conf_away_perc_PPP  = mean(def_non_conf_away_perc_PPP , na.rm = T)), 
output2_ppp_2 %>% dplyr::summarise(PPP = mean(PPP, na.rm = T),
                                 off_home_perc_PPP = mean(off_home_perc_PPP, na.rm = T),
                                 off_conf_home_perc_PPP  = mean(off_conf_home_perc_PPP , na.rm = T),
                                 off_non_conf_home_perc_PPP  = mean(off_non_conf_home_perc_PPP , na.rm = T),
                                 def_home_perc_PPP = mean(def_home_perc_PPP, na.rm = T),
                                 def_conf_home_perc_PPP  = mean(def_conf_home_perc_PPP , na.rm = T),
                                 def_non_conf_home_perc_PPP  = mean(def_non_conf_home_perc_PPP , na.rm = T)))
ppp_result_df <- as.data.frame(rbind(team_one_ppp, team_two_ppp))
rownames(ppp_result_df) <- c(off_string, def_string)



####
####
####


output1_poss <- comparison_poss_func(prediction_df, .4, year = NULL, locationinput = "A")
output2_poss <- comparison_poss_func(prediction_df, .51, year = NULL, locationinput = "H")

output1_poss %>% filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(poss = mean(poss, na.rm = T),
                   off_away_perc_poss = mean(off_away_perc_poss, na.rm = T),
                   off_conf_away_perc_poss  = mean(off_conf_away_perc_poss , na.rm = T),
                   off_non_conf_away_perc_poss  = mean(off_non_conf_away_perc_poss , na.rm = T),
                   def_away_perc_poss = mean(def_away_perc_poss, na.rm = T),
                   def_conf_away_perc_poss  = mean(def_conf_away_perc_poss , na.rm = T),
                   def_non_conf_away_perc_poss  = mean(def_non_conf_away_perc_poss , na.rm = T),
                   n = n())

output2_poss %>% 
  filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(poss = mean(poss, na.rm = T),
                   off_home_perc_poss = mean(off_home_perc_poss, na.rm = T),
                   off_conf_home_perc_poss  = mean(off_conf_home_perc_poss , na.rm = T),
                   off_non_conf_home_perc_poss  = mean(off_non_conf_home_perc_poss , na.rm = T),
                   def_home_perc_poss = mean(def_home_perc_poss, na.rm = T),
                   def_conf_home_perc_poss  = mean(def_conf_home_perc_poss , na.rm = T),
                   def_non_conf_home_perc_poss  = mean(def_non_conf_home_perc_poss , na.rm = T),
                   n = n())


team_one_poss <- cbind(output1_poss %>% dplyr::summarise(poss = mean(poss, na.rm = T),
                             off_away_perc_poss = mean(off_away_perc_poss, na.rm = T),
                             off_conf_away_perc_poss  = mean(off_conf_away_perc_poss , na.rm = T),
                             off_non_conf_away_perc_poss  = mean(off_non_conf_away_perc_poss , na.rm = T),
                             def_away_perc_poss = mean(def_away_perc_poss, na.rm = T),
                             def_conf_away_perc_poss  = mean(def_conf_away_perc_poss , na.rm = T),
                             def_non_conf_away_perc_poss  = mean(def_non_conf_away_perc_poss , na.rm = T)), 
output2_poss %>% dplyr::summarise(poss = mean(poss, na.rm = T),
                             off_home_perc_poss = mean(off_home_perc_poss, na.rm = T),
                             off_conf_home_perc_poss  = mean(off_conf_home_perc_poss , na.rm = T),
                             off_non_conf_home_perc_poss  = mean(off_non_conf_home_perc_poss , na.rm = T),
                             def_home_perc_poss = mean(def_home_perc_poss, na.rm = T),
                             def_conf_home_perc_poss  = mean(def_conf_home_perc_poss , na.rm = T),
                             def_non_conf_home_perc_poss  = mean(def_non_conf_home_perc_poss , na.rm = T)))



output1_poss_2 <- comparison_poss_func(prediction_df_2, .39, year = NULL, locationinput = "A")
output2_poss_2 <- comparison_poss_func(prediction_df_2, .49, year = NULL, locationinput = "H")

output1_poss_2 %>% filter((Team != def_string | Opp != off_string) & season != 2025) %>% 
  dplyr::summarise(poss = mean(poss, na.rm = T),
                   off_away_perc_poss = mean(off_away_perc_poss, na.rm = T),
                   off_conf_away_perc_poss  = mean(off_conf_away_perc_poss , na.rm = T),
                   off_non_conf_away_perc_poss  = mean(off_non_conf_away_perc_poss , na.rm = T),
                   def_away_perc_poss = mean(def_away_perc_poss, na.rm = T),
                   def_conf_away_perc_poss  = mean(def_conf_away_perc_poss , na.rm = T),
                   def_non_conf_away_perc_poss  = mean(def_non_conf_away_perc_poss , na.rm = T),
                   n = n())

output2_poss_2 %>% 
  filter((Team != def_string | Opp != off_string) & season != 2025) %>% 
  dplyr::summarise(poss = mean(poss, na.rm = T),
                   off_home_perc_poss = mean(off_home_perc_poss, na.rm = T),
                   off_conf_home_perc_poss  = mean(off_conf_home_perc_poss , na.rm = T),
                   off_non_conf_home_perc_poss  = mean(off_non_conf_home_perc_poss , na.rm = T),
                   def_home_perc_poss = mean(def_home_perc_poss, na.rm = T),
                   def_conf_home_perc_poss  = mean(def_conf_home_perc_poss , na.rm = T),
                   def_non_conf_home_perc_poss  = mean(def_non_conf_home_perc_poss , na.rm = T),
                   n = n())


team_two_poss <- cbind(output1_poss_2 %>% dplyr::summarise(poss = mean(poss, na.rm = T),
                                  off_away_perc_poss = mean(off_away_perc_poss, na.rm = T),
                                  off_conf_away_perc_poss  = mean(off_conf_away_perc_poss , na.rm = T),
                                  off_non_conf_away_perc_poss  = mean(off_non_conf_away_perc_poss , na.rm = T),
                                  def_away_perc_poss = mean(def_away_perc_poss, na.rm = T),
                                  def_conf_away_perc_poss  = mean(def_conf_away_perc_poss , na.rm = T),
                                  def_non_conf_away_perc_poss  = mean(def_non_conf_away_perc_poss , na.rm = T)), output2_poss_2 %>% dplyr::summarise(poss = mean(poss, na.rm = T),
                                  off_home_perc_poss = mean(off_home_perc_poss, na.rm = T),
                                  off_conf_home_perc_poss  = mean(off_conf_home_perc_poss , na.rm = T),
                                  off_non_conf_home_perc_poss  = mean(off_non_conf_home_perc_poss , na.rm = T),
                                  def_home_perc_poss = mean(def_home_perc_poss, na.rm = T),
                                  def_conf_home_perc_poss  = mean(def_conf_home_perc_poss , na.rm = T),
                                  def_non_conf_home_perc_poss  = mean(def_non_conf_home_perc_poss , na.rm = T)))

poss_result_df <- as.data.frame(rbind(team_one_poss, team_two_poss))
rownames(poss_result_df) <- c(off_string, def_string)



####
####


output1_twopt <- comparison_twoptperc_func(prediction_df, .52, year = NULL, locationinput = "A")
output2_twopt <- comparison_twoptperc_func(prediction_df, .68, year = NULL, locationinput = "H")


output1_twopt %>% filter((Team != off_string | Opp != def_string) & season != 2025) %>% 
  dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                   off_away_perc_2Pperc = mean(off_away_perc_2Pperc, na.rm = T),
                   off_conf_away_perc_2Pperc = mean(off_conf_away_perc_2Pperc, na.rm = T),
                   off_non_conf_away_perc_2Pperc = mean(off_non_conf_away_perc_2Pperc, na.rm = T),
                   def_away_perc_2Pperc = mean(def_away_perc_2Pperc, na.rm = T),
                   def_conf_away_perc_2Pperc  = mean(def_conf_away_perc_2Pperc , na.rm = T),
                   def_non_conf_away_perc_2Pperc  = mean(def_non_conf_away_perc_2Pperc , na.rm = T),
                   n = n())

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


team_one_2P <- cbind(output1_twopt %>% dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                                   off_away_perc_2Pperc = mean(off_away_perc_2Pperc, na.rm = T),
                                   off_conf_away_perc_2Pperc = mean(off_conf_away_perc_2Pperc, na.rm = T),
                                   off_non_conf_away_perc_2Pperc = mean(off_non_conf_away_perc_2Pperc, na.rm = T),
                                   def_away_perc_2Pperc = mean(def_away_perc_2Pperc, na.rm = T),
                                   def_conf_away_perc_2Pperc  = mean(def_conf_away_perc_2Pperc , na.rm = T),
                                   def_non_conf_away_perc_2Pperc  = mean(def_non_conf_away_perc_2Pperc , na.rm = T)), 
output2_twopt %>% dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                                   off_home_perc_2Pperc = mean(off_home_perc_2Pperc, na.rm = T),
                                   off_conf_home_perc_2Pperc = mean(off_conf_home_perc_2Pperc, na.rm = T),
                                   off_non_conf_home_perc_2Pperc = mean(off_non_conf_home_perc_2Pperc, na.rm = T),
                                   def_home_perc_2Pperc = mean(def_home_perc_2Pperc, na.rm = T),
                                   def_conf_home_perc_2Pperc  = mean(def_conf_home_perc_2Pperc , na.rm = T),
                                   def_non_conf_home_perc_2Pperc  = mean(def_non_conf_home_perc_2Pperc , na.rm = T)))



output1_twopt_2 <- comparison_twoptperc_func(prediction_df_2, .5, year = NULL, locationinput = "A")
output2_twopt_2 <- comparison_twoptperc_func(prediction_df_2, .67, year = NULL, locationinput = "H")


output1_twopt_2 %>% filter((Team != def_string | Opp != off_string) & season != 2025) %>% 
  dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                   off_away_perc_2Pperc = mean(off_away_perc_2Pperc, na.rm = T),
                   off_conf_away_perc_2Pperc = mean(off_conf_away_perc_2Pperc, na.rm = T),
                   off_non_conf_away_perc_2Pperc = mean(off_non_conf_away_perc_2Pperc, na.rm = T),
                   def_away_perc_2Pperc = mean(def_away_perc_2Pperc, na.rm = T),
                   def_conf_away_perc_2Pperc  = mean(def_conf_away_perc_2Pperc , na.rm = T),
                   def_non_conf_away_perc_2Pperc  = mean(def_non_conf_away_perc_2Pperc , na.rm = T),
                   n = n())

output2_twopt_2 %>% 
  filter((Team != def_string | Opp != off_string) & season != 2025) %>% 
  dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                   off_home_perc_2Pperc = mean(off_home_perc_2Pperc, na.rm = T),
                   off_conf_home_perc_2Pperc = mean(off_conf_home_perc_2Pperc, na.rm = T),
                   off_non_conf_home_perc_2Pperc = mean(off_non_conf_home_perc_2Pperc, na.rm = T),
                   def_home_perc_2Pperc = mean(def_home_perc_2Pperc, na.rm = T),
                   def_conf_home_perc_2Pperc  = mean(def_conf_home_perc_2Pperc , na.rm = T),
                   def_non_conf_home_perc_2Pperc  = mean(def_non_conf_home_perc_2Pperc , na.rm = T),
                   n = n())


team_two_2P <- cbind(output1_twopt_2 %>% dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                                   off_away_perc_2Pperc = mean(off_away_perc_2Pperc, na.rm = T),
                                   off_conf_away_perc_2Pperc = mean(off_conf_away_perc_2Pperc, na.rm = T),
                                   off_non_conf_away_perc_2Pperc = mean(off_non_conf_away_perc_2Pperc, na.rm = T),
                                   def_away_perc_2Pperc = mean(def_away_perc_2Pperc, na.rm = T),
                                   def_conf_away_perc_2Pperc  = mean(def_conf_away_perc_2Pperc , na.rm = T),
                                   def_non_conf_away_perc_2Pperc  = mean(def_non_conf_away_perc_2Pperc , na.rm = T)), 
                     output2_twopt_2 %>% dplyr::summarise(`2Pperc` = mean(`2Pperc`, na.rm = T),
                                   off_home_perc_2Pperc = mean(off_home_perc_2Pperc, na.rm = T),
                                   off_conf_home_perc_2Pperc = mean(off_conf_home_perc_2Pperc, na.rm = T),
                                   off_non_conf_home_perc_2Pperc = mean(off_non_conf_home_perc_2Pperc, na.rm = T),
                                   def_home_perc_2Pperc = mean(def_home_perc_2Pperc, na.rm = T),
                                   def_conf_home_perc_2Pperc  = mean(def_conf_home_perc_2Pperc , na.rm = T),
                                   def_non_conf_home_perc_2Pperc  = mean(def_non_conf_home_perc_2Pperc , na.rm = T)))

`2P_result_df` <- as.data.frame(rbind(team_one_2P, team_two_2P))
rownames(`2P_result_df`) <- c(off_string, def_string)



####
####
####


output1_threept <- comparison_threeptperc_func(prediction_df, .55, year = NULL, locationinput = "A")
output2_threept <- comparison_threeptperc_func(prediction_df, .9, year = NULL, locationinput = "H")

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


team_one_3P <- cbind(output1_threept %>% dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                                     off_away_perc_3P = mean(off_away_perc_3P, na.rm = T),
                                     off_conf_away_perc_3P = mean(off_conf_away_perc_3P, na.rm = T),
                                     off_non_conf_away_perc_3P = mean(off_non_conf_away_perc_3P, na.rm = T),
                                     def_away_perc_3P = mean(def_away_perc_3P, na.rm = T),
                                     def_conf_away_perc_3P  = mean(def_conf_away_perc_3P , na.rm = T),
                                     def_non_conf_away_perc_3P  = mean(def_non_conf_away_perc_3P , na.rm = T)),
output2_threept %>% dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                                     off_home_perc_3P = mean(off_home_perc_3P, na.rm = T),
                                     off_conf_home_perc_3P = mean(off_conf_home_perc_3P, na.rm = T),
                                     off_non_conf_home_perc_3P = mean(off_non_conf_home_perc_3P, na.rm = T),
                                     def_home_perc_3P = mean(def_home_perc_3P, na.rm = T),
                                     def_conf_home_perc_3P  = mean(def_conf_home_perc_3P , na.rm = T),
                                     def_non_conf_home_perc_3P  = mean(def_non_conf_home_perc_3P , na.rm = T)))



output1_threept_2 <- comparison_threeptperc_func(prediction_df_2, .43, year = NULL, locationinput = "A")
output2_threept_2 <- comparison_threeptperc_func(prediction_df_2, .8, year = NULL, locationinput = "H")

output1_threept_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                   off_away_perc_3P = mean(off_away_perc_3P, na.rm = T),
                   off_conf_away_perc_3P = mean(off_conf_away_perc_3P, na.rm = T),
                   off_non_conf_away_perc_3P = mean(off_non_conf_away_perc_3P, na.rm = T),
                   def_away_perc_3P = mean(def_away_perc_3P, na.rm = T),
                   def_conf_away_perc_3P  = mean(def_conf_away_perc_3P , na.rm = T),
                   def_non_conf_away_perc_3P  = mean(def_non_conf_away_perc_3P , na.rm = T),
                   n = n())

output2_threept_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                   off_home_perc_3P = mean(off_home_perc_3P, na.rm = T),
                   off_conf_home_perc_3P = mean(off_conf_home_perc_3P, na.rm = T),
                   off_non_conf_home_perc_3P = mean(off_non_conf_home_perc_3P, na.rm = T),
                   def_home_perc_3P = mean(def_home_perc_3P, na.rm = T),
                   def_conf_home_perc_3P  = mean(def_conf_home_perc_3P , na.rm = T),
                   def_non_conf_home_perc_3P  = mean(def_non_conf_home_perc_3P , na.rm = T),
                   n = n())


team_two_3P <- cbind(output1_threept_2 %>% dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                                     off_away_perc_3P = mean(off_away_perc_3P, na.rm = T),
                                     off_conf_away_perc_3P = mean(off_conf_away_perc_3P, na.rm = T),
                                     off_non_conf_away_perc_3P = mean(off_non_conf_away_perc_3P, na.rm = T),
                                     def_away_perc_3P = mean(def_away_perc_3P, na.rm = T),
                                     def_conf_away_perc_3P  = mean(def_conf_away_perc_3P , na.rm = T),
                                     def_non_conf_away_perc_3P  = mean(def_non_conf_away_perc_3P , na.rm = T)),
output2_threept_2 %>% dplyr::summarise(threePperc = mean(`3P`, na.rm = T),
                                     off_home_perc_3P = mean(off_home_perc_3P, na.rm = T),
                                     off_conf_home_perc_3P = mean(off_conf_home_perc_3P, na.rm = T),
                                     off_non_conf_home_perc_3P = mean(off_non_conf_home_perc_3P, na.rm = T),
                                     def_home_perc_3P = mean(def_home_perc_3P, na.rm = T),
                                     def_conf_home_perc_3P  = mean(def_conf_home_perc_3P , na.rm = T),
                                     def_non_conf_home_perc_3P  = mean(def_non_conf_home_perc_3P , na.rm = T)))

`3P_result_df` <- as.data.frame(rbind(team_one_3P, team_two_3P))
rownames(`3P_result_df`) <- c(off_string, def_string)


####
####
####


output1_STL_perc <- comparison_STL_perc_func(prediction_df, .39, year = NULL, locationinput = "A")
output2_STL_perc <- comparison_STL_perc_func(prediction_df, .51, year = NULL, locationinput = "H")

output1_STL_perc %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                   off_away_perc_STL_perc = mean(off_away_perc_STL_perc, na.rm = T),
                   off_conf_away_perc_STL_perc = mean(off_conf_away_perc_STL_perc, na.rm = T),
                   off_non_conf_away_perc_STL_perc = mean(off_non_conf_away_perc_STL_perc, na.rm = T),
                   def_away_perc_STL_perc = mean(def_away_perc_STL_perc, na.rm = T),
                   def_conf_away_perc_STL_perc  = mean(def_conf_away_perc_STL_perc , na.rm = T),
                   def_non_conf_away_perc_STL_perc  = mean(def_non_conf_away_perc_STL_perc , na.rm = T),
                   n = n())

output2_STL_perc %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                   off_home_perc_STL_perc = mean(off_home_perc_STL_perc, na.rm = T),
                   off_conf_home_perc_STL_perc = mean(off_conf_home_perc_STL_perc, na.rm = T),
                   off_non_conf_home_perc_STL_perc = mean(off_non_conf_home_perc_STL_perc, na.rm = T),
                   def_home_perc_STL_perc = mean(def_home_perc_STL_perc, na.rm = T),
                   def_conf_home_perc_STL_perc  = mean(def_conf_home_perc_STL_perc , na.rm = T),
                   def_non_conf_home_perc_STL_perc  = mean(def_non_conf_home_perc_STL_perc , na.rm = T),
                   n = n())

team_one_STL <- cbind(output1_STL_perc %>% dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                                      off_away_perc_STL_perc = mean(off_away_perc_STL_perc, na.rm = T),
                                      off_conf_away_perc_STL_perc = mean(off_conf_away_perc_STL_perc, na.rm = T),
                                      off_non_conf_away_perc_STL_perc = mean(off_non_conf_away_perc_STL_perc, na.rm = T),
                                      def_away_perc_STL_perc = mean(def_away_perc_STL_perc, na.rm = T),
                                      def_conf_away_perc_STL_perc  = mean(def_conf_away_perc_STL_perc , na.rm = T),
                                      def_non_conf_away_perc_STL_perc  = mean(def_non_conf_away_perc_STL_perc , na.rm = T)),
output2_STL_perc %>% dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                                      off_home_perc_STL_perc = mean(off_home_perc_STL_perc, na.rm = T),
                                      off_conf_home_perc_STL_perc = mean(off_conf_home_perc_STL_perc, na.rm = T),
                                      off_non_conf_home_perc_STL_perc = mean(off_non_conf_home_perc_STL_perc, na.rm = T),
                                      def_home_perc_STL_perc = mean(def_home_perc_STL_perc, na.rm = T),
                                      def_conf_home_perc_STL_perc  = mean(def_conf_home_perc_STL_perc , na.rm = T),
                                      def_non_conf_home_perc_STL_perc  = mean(def_non_conf_home_perc_STL_perc , na.rm = T)))



output1_STL_perc_2 <- comparison_STL_perc_func(prediction_df_2, .4, year = NULL, locationinput = "A")
output2_STL_perc_2 <- comparison_STL_perc_func(prediction_df_2, .5, year = NULL, locationinput = "H")

output1_STL_perc_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                   off_away_perc_STL_perc = mean(off_away_perc_STL_perc, na.rm = T),
                   off_conf_away_perc_STL_perc = mean(off_conf_away_perc_STL_perc, na.rm = T),
                   off_non_conf_away_perc_STL_perc = mean(off_non_conf_away_perc_STL_perc, na.rm = T),
                   def_away_perc_STL_perc = mean(def_away_perc_STL_perc, na.rm = T),
                   def_conf_away_perc_STL_perc  = mean(def_conf_away_perc_STL_perc , na.rm = T),
                   def_non_conf_away_perc_STL_perc  = mean(def_non_conf_away_perc_STL_perc , na.rm = T),
                   n = n())

output2_STL_perc_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                   off_home_perc_STL_perc = mean(off_home_perc_STL_perc, na.rm = T),
                   off_conf_home_perc_STL_perc = mean(off_conf_home_perc_STL_perc, na.rm = T),
                   off_non_conf_home_perc_STL_perc = mean(off_non_conf_home_perc_STL_perc, na.rm = T),
                   def_home_perc_STL_perc = mean(def_home_perc_STL_perc, na.rm = T),
                   def_conf_home_perc_STL_perc  = mean(def_conf_home_perc_STL_perc , na.rm = T),
                   def_non_conf_home_perc_STL_perc  = mean(def_non_conf_home_perc_STL_perc , na.rm = T),
                   n = n())

team_two_STL <- cbind(output1_STL_perc_2 %>% dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                                      off_away_perc_STL_perc = mean(off_away_perc_STL_perc, na.rm = T),
                                      off_conf_away_perc_STL_perc = mean(off_conf_away_perc_STL_perc, na.rm = T),
                                      off_non_conf_away_perc_STL_perc = mean(off_non_conf_away_perc_STL_perc, na.rm = T),
                                      def_away_perc_STL_perc = mean(def_away_perc_STL_perc, na.rm = T),
                                      def_conf_away_perc_STL_perc  = mean(def_conf_away_perc_STL_perc , na.rm = T),
                                      def_non_conf_away_perc_STL_perc  = mean(def_non_conf_away_perc_STL_perc , na.rm = T)), 
output2_STL_perc_2 %>% dplyr::summarise(STL_perc = mean(STL_perc, na.rm = T),
                                      off_home_perc_STL_perc = mean(off_home_perc_STL_perc, na.rm = T),
                                      off_conf_home_perc_STL_perc = mean(off_conf_home_perc_STL_perc, na.rm = T),
                                      off_non_conf_home_perc_STL_perc = mean(off_non_conf_home_perc_STL_perc, na.rm = T),
                                      def_home_perc_STL_perc = mean(def_home_perc_STL_perc, na.rm = T),
                                      def_conf_home_perc_STL_perc  = mean(def_conf_home_perc_STL_perc , na.rm = T),
                                      def_non_conf_home_perc_STL_perc  = mean(def_non_conf_home_perc_STL_perc , na.rm = T)))

STL_result_df <- as.data.frame(rbind(team_one_STL, team_two_STL))
rownames(STL_result_df) <- c(off_string, def_string)



####
####
####


output1_tov_perc <- comparison_tov_perc_func(prediction_df, .35, year = NULL, locationinput = "A")
output2_tov_perc <- comparison_tov_perc_func(prediction_df, .52, year = NULL, locationinput = "H")

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


team_one_TOV <- cbind(output1_tov_perc %>% dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                                      off_away_perc_TOV_perc = mean(off_away_perc_TOV_perc, na.rm = T),
                                      off_conf_away_perc_TOV_perc = mean(off_conf_away_perc_TOV_perc, na.rm = T),
                                      off_non_conf_away_perc_TOV_perc = mean(off_non_conf_away_perc_TOV_perc, na.rm = T),
                                      def_away_perc_TOV_perc = mean(def_away_perc_TOV_perc, na.rm = T),
                                      def_conf_away_perc_TOV_perc  = mean(def_conf_away_perc_TOV_perc , na.rm = T),
                                      def_non_conf_away_perc_TOV_perc  = mean(def_non_conf_away_perc_TOV_perc , na.rm = T)), 
output2_tov_perc %>% dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                                      off_home_perc_TOV_perc = mean(off_home_perc_TOV_perc, na.rm = T),
                                      off_conf_home_perc_TOV_perc = mean(off_conf_home_perc_TOV_perc, na.rm = T),
                                      off_non_conf_home_perc_TOV_perc = mean(off_non_conf_home_perc_TOV_perc, na.rm = T),
                                      def_home_perc_TOV_perc = mean(def_home_perc_TOV_perc, na.rm = T),
                                      def_conf_home_perc_TOV_perc  = mean(def_conf_home_perc_TOV_perc , na.rm = T),
                                      def_non_conf_home_perc_TOV_perc  = mean(def_non_conf_home_perc_TOV_perc , na.rm = T)))



output1_tov_perc_2 <- comparison_tov_perc_func(prediction_df_2, .34, year = NULL, locationinput = "A")
output2_tov_perc_2 <- comparison_tov_perc_func(prediction_df_2, .51, year = NULL, locationinput = "H")

output1_tov_perc_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                   off_away_perc_TOV_perc = mean(off_away_perc_TOV_perc, na.rm = T),
                   off_conf_away_perc_TOV_perc = mean(off_conf_away_perc_TOV_perc, na.rm = T),
                   off_non_conf_away_perc_TOV_perc = mean(off_non_conf_away_perc_TOV_perc, na.rm = T),
                   def_away_perc_TOV_perc = mean(def_away_perc_TOV_perc, na.rm = T),
                   def_conf_away_perc_TOV_perc  = mean(def_conf_away_perc_TOV_perc , na.rm = T),
                   def_non_conf_away_perc_TOV_perc  = mean(def_non_conf_away_perc_TOV_perc , na.rm = T),
                   n = n())

output2_tov_perc_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                   off_home_perc_TOV_perc = mean(off_home_perc_TOV_perc, na.rm = T),
                   off_conf_home_perc_TOV_perc = mean(off_conf_home_perc_TOV_perc, na.rm = T),
                   off_non_conf_home_perc_TOV_perc = mean(off_non_conf_home_perc_TOV_perc, na.rm = T),
                   def_home_perc_TOV_perc = mean(def_home_perc_TOV_perc, na.rm = T),
                   def_conf_home_perc_TOV_perc  = mean(def_conf_home_perc_TOV_perc , na.rm = T),
                   def_non_conf_home_perc_TOV_perc  = mean(def_non_conf_home_perc_TOV_perc , na.rm = T),
                   n = n())


team_two_TOV <- cbind(output1_tov_perc_2 %>% dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                                      off_away_perc_TOV_perc = mean(off_away_perc_TOV_perc, na.rm = T),
                                      off_conf_away_perc_TOV_perc = mean(off_conf_away_perc_TOV_perc, na.rm = T),
                                      off_non_conf_away_perc_TOV_perc = mean(off_non_conf_away_perc_TOV_perc, na.rm = T),
                                      def_away_perc_TOV_perc = mean(def_away_perc_TOV_perc, na.rm = T),
                                      def_conf_away_perc_TOV_perc  = mean(def_conf_away_perc_TOV_perc , na.rm = T),
                                      def_non_conf_away_perc_TOV_perc  = mean(def_non_conf_away_perc_TOV_perc , na.rm = T)), 
output2_tov_perc_2 %>% dplyr::summarise(TOV_perc = mean(TOV_perc, na.rm = T),
                                      off_home_perc_TOV_perc = mean(off_home_perc_TOV_perc, na.rm = T),
                                      off_conf_home_perc_TOV_perc = mean(off_conf_home_perc_TOV_perc, na.rm = T),
                                      off_non_conf_home_perc_TOV_perc = mean(off_non_conf_home_perc_TOV_perc, na.rm = T),
                                      def_home_perc_TOV_perc = mean(def_home_perc_TOV_perc, na.rm = T),
                                      def_conf_home_perc_TOV_perc  = mean(def_conf_home_perc_TOV_perc , na.rm = T),
                                      def_non_conf_home_perc_TOV_perc  = mean(def_non_conf_home_perc_TOV_perc , na.rm = T)))

TOV_result_df <- as.data.frame(rbind(team_one_TOV, team_two_TOV))
rownames(TOV_result_df) <- c(off_string, def_string)



####
####
####



output1_ORB_perc <- comparison_ORB_perc_func(prediction_df, .45, year = NULL, locationinput = "A")
output2_ORB_perc <- comparison_ORB_perc_func(prediction_df, .55, year = NULL, locationinput = "H")

output1_ORB_perc %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                   off_away_perc_ORB_perc = mean(off_away_perc_ORB_perc, na.rm = T),
                   off_conf_away_perc_ORB_perc = mean(off_conf_away_perc_ORB_perc, na.rm = T),
                   off_non_conf_away_perc_ORB_perc = mean(off_non_conf_away_perc_ORB_perc, na.rm = T),
                   def_away_perc_ORB_perc = mean(def_away_perc_ORB_perc, na.rm = T),
                   def_conf_away_perc_ORB_perc  = mean(def_conf_away_perc_ORB_perc , na.rm = T),
                   def_non_conf_away_perc_ORB_perc  = mean(def_non_conf_away_perc_ORB_perc , na.rm = T),
                   n = n())

output2_ORB_perc %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                   off_home_perc_ORB_perc = mean(off_home_perc_ORB_perc, na.rm = T),
                   off_conf_home_perc_ORB_perc = mean(off_conf_home_perc_ORB_perc, na.rm = T),
                   off_non_conf_home_perc_ORB_perc = mean(off_non_conf_home_perc_ORB_perc, na.rm = T),
                   def_home_perc_ORB_perc = mean(def_home_perc_ORB_perc, na.rm = T),
                   def_conf_home_perc_ORB_perc  = mean(def_conf_home_perc_ORB_perc , na.rm = T),
                   def_non_conf_home_perc_ORB_perc  = mean(def_non_conf_home_perc_ORB_perc , na.rm = T),
                   n = n())

team_one_ORB <- cbind(output1_ORB_perc %>% dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                                      off_away_perc_ORB_perc = mean(off_away_perc_ORB_perc, na.rm = T),
                                      off_conf_away_perc_ORB_perc = mean(off_conf_away_perc_ORB_perc, na.rm = T),
                                      off_non_conf_away_perc_ORB_perc = mean(off_non_conf_away_perc_ORB_perc, na.rm = T),
                                      def_away_perc_ORB_perc = mean(def_away_perc_ORB_perc, na.rm = T),
                                      def_conf_away_perc_ORB_perc  = mean(def_conf_away_perc_ORB_perc , na.rm = T),
                                      def_non_conf_away_perc_ORB_perc  = mean(def_non_conf_away_perc_ORB_perc , na.rm = T)), 
output2_ORB_perc %>% dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                                      off_home_perc_ORB_perc = mean(off_home_perc_ORB_perc, na.rm = T),
                                      off_conf_home_perc_ORB_perc = mean(off_conf_home_perc_ORB_perc, na.rm = T),
                                      off_non_conf_home_perc_ORB_perc = mean(off_non_conf_home_perc_ORB_perc, na.rm = T),
                                      def_home_perc_ORB_perc = mean(def_home_perc_ORB_perc, na.rm = T),
                                      def_conf_home_perc_ORB_perc  = mean(def_conf_home_perc_ORB_perc , na.rm = T),
                                      def_non_conf_home_perc_ORB_perc  = mean(def_non_conf_home_perc_ORB_perc , na.rm = T)))


output1_ORB_perc_2 <- comparison_ORB_perc_func(prediction_df_2, .57, year = NULL, locationinput = "A")
output2_ORB_perc_2 <- comparison_ORB_perc_func(prediction_df_2, .67, year = NULL, locationinput = "H")

output1_ORB_perc_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                   off_away_perc_ORB_perc = mean(off_away_perc_ORB_perc, na.rm = T),
                   off_conf_away_perc_ORB_perc = mean(off_conf_away_perc_ORB_perc, na.rm = T),
                   off_non_conf_away_perc_ORB_perc = mean(off_non_conf_away_perc_ORB_perc, na.rm = T),
                   def_away_perc_ORB_perc = mean(def_away_perc_ORB_perc, na.rm = T),
                   def_conf_away_perc_ORB_perc  = mean(def_conf_away_perc_ORB_perc , na.rm = T),
                   def_non_conf_away_perc_ORB_perc  = mean(def_non_conf_away_perc_ORB_perc , na.rm = T),
                   n = n())

output2_ORB_perc_2 %>%
  filter(Team != def_string & Opp != off_string) %>%
  dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                   off_home_perc_ORB_perc = mean(off_home_perc_ORB_perc, na.rm = T),
                   off_conf_home_perc_ORB_perc = mean(off_conf_home_perc_ORB_perc, na.rm = T),
                   off_non_conf_home_perc_ORB_perc = mean(off_non_conf_home_perc_ORB_perc, na.rm = T),
                   def_home_perc_ORB_perc = mean(def_home_perc_ORB_perc, na.rm = T),
                   def_conf_home_perc_ORB_perc  = mean(def_conf_home_perc_ORB_perc , na.rm = T),
                   def_non_conf_home_perc_ORB_perc  = mean(def_non_conf_home_perc_ORB_perc , na.rm = T),
                   n = n())

team_two_ORB <- cbind(output1_ORB_perc_2 %>% dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                                      off_away_perc_ORB_perc = mean(off_away_perc_ORB_perc, na.rm = T),
                                      off_conf_away_perc_ORB_perc = mean(off_conf_away_perc_ORB_perc, na.rm = T),
                                      off_non_conf_away_perc_ORB_perc = mean(off_non_conf_away_perc_ORB_perc, na.rm = T),
                                      def_away_perc_ORB_perc = mean(def_away_perc_ORB_perc, na.rm = T),
                                      def_conf_away_perc_ORB_perc  = mean(def_conf_away_perc_ORB_perc , na.rm = T),
                                      def_non_conf_away_perc_ORB_perc  = mean(def_non_conf_away_perc_ORB_perc , na.rm = T)), 
output2_ORB_perc_2 %>% dplyr::summarise(ORB_perc = mean(ORB_perc, na.rm = T),
                                      off_home_perc_ORB_perc = mean(off_home_perc_ORB_perc, na.rm = T),
                                      off_conf_home_perc_ORB_perc = mean(off_conf_home_perc_ORB_perc, na.rm = T),
                                      off_non_conf_home_perc_ORB_perc = mean(off_non_conf_home_perc_ORB_perc, na.rm = T),
                                      def_home_perc_ORB_perc = mean(def_home_perc_ORB_perc, na.rm = T),
                                      def_conf_home_perc_ORB_perc  = mean(def_conf_home_perc_ORB_perc , na.rm = T),
                                      def_non_conf_home_perc_ORB_perc  = mean(def_non_conf_home_perc_ORB_perc , na.rm = T)))

ORB_result_df <- as.data.frame(rbind(team_one_ORB, team_two_ORB))
rownames(ORB_result_df) <- c(off_string, def_string)



####
####
####



output1_FTR <- comparison_FTR_func(prediction_df, .44, year = NULL, locationinput = "A")
output2_FTR <- comparison_FTR_func(prediction_df, .64, year = NULL, locationinput = "H")

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

team_one_FTR <- cbind(output1_FTR %>% dplyr::summarise(FTR = mean(FTR, na.rm = T),
                                 off_away_perc_FTR = mean(off_away_perc_FTR, na.rm = T),
                                 off_conf_away_perc_FTR = mean(off_conf_away_perc_FTR, na.rm = T),
                                 off_non_conf_away_perc_FTR = mean(off_non_conf_away_perc_FTR, na.rm = T),
                                 def_away_perc_FTR = mean(def_away_perc_FTR, na.rm = T),
                                 def_conf_away_perc_FTR  = mean(def_conf_away_perc_FTR , na.rm = T),
                                 def_non_conf_away_perc_FTR  = mean(def_non_conf_away_perc_FTR , na.rm = T)), 
output2_FTR %>% dplyr::summarise(FTR = mean(FTR, na.rm = T),
                                 off_home_perc_FTR = mean(off_home_perc_FTR, na.rm = T),
                                 off_conf_home_perc_FTR = mean(off_conf_home_perc_FTR, na.rm = T),
                                 off_non_conf_home_perc_FTR = mean(off_non_conf_home_perc_FTR, na.rm = T),
                                 def_home_perc_FTR = mean(def_home_perc_FTR, na.rm = T),
                                 def_conf_home_perc_FTR  = mean(def_conf_home_perc_FTR , na.rm = T),
                                 def_non_conf_home_perc_FTR  = mean(def_non_conf_home_perc_FTR , na.rm = T)))


output1_FTR_2 <- comparison_FTR_func(prediction_df_2, .49, year = NULL, locationinput = "A")
output2_FTR_2 <- comparison_FTR_func(prediction_df_2, .69, year = NULL, locationinput = "H")

output1_FTR_2 %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(FTR = mean(FTR, na.rm = T),
                   off_away_perc_FTR = mean(off_away_perc_FTR, na.rm = T),
                   off_conf_away_perc_FTR = mean(off_conf_away_perc_FTR, na.rm = T),
                   off_non_conf_away_perc_FTR = mean(off_non_conf_away_perc_FTR, na.rm = T),
                   def_away_perc_FTR = mean(def_away_perc_FTR, na.rm = T),
                   def_conf_away_perc_FTR  = mean(def_conf_away_perc_FTR , na.rm = T),
                   def_non_conf_away_perc_FTR  = mean(def_non_conf_away_perc_FTR , na.rm = T),
                   n = n())

output2_FTR_2 %>%
  filter(Team != off_string & Opp != def_string) %>%
  dplyr::summarise(FTR = mean(FTR, na.rm = T),
                   off_home_perc_FTR = mean(off_home_perc_FTR, na.rm = T),
                   off_conf_home_perc_FTR = mean(off_conf_home_perc_FTR, na.rm = T),
                   off_non_conf_home_perc_FTR = mean(off_non_conf_home_perc_FTR, na.rm = T),
                   def_home_perc_FTR = mean(def_home_perc_FTR, na.rm = T),
                   def_conf_home_perc_FTR  = mean(def_conf_home_perc_FTR , na.rm = T),
                   def_non_conf_home_perc_FTR  = mean(def_non_conf_home_perc_FTR , na.rm = T),
                   n = n())

team_two_FTR <- cbind(output1_FTR_2 %>% dplyr::summarise(FTR = mean(FTR, na.rm = T),
                                 off_away_perc_FTR = mean(off_away_perc_FTR, na.rm = T),
                                 off_conf_away_perc_FTR = mean(off_conf_away_perc_FTR, na.rm = T),
                                 off_non_conf_away_perc_FTR = mean(off_non_conf_away_perc_FTR, na.rm = T),
                                 def_away_perc_FTR = mean(def_away_perc_FTR, na.rm = T),
                                 def_conf_away_perc_FTR  = mean(def_conf_away_perc_FTR , na.rm = T),
                                 def_non_conf_away_perc_FTR  = mean(def_non_conf_away_perc_FTR , na.rm = T)), 
output2_FTR_2 %>% dplyr::summarise(FTR = mean(FTR, na.rm = T),
                                 off_home_perc_FTR = mean(off_home_perc_FTR, na.rm = T),
                                 off_conf_home_perc_FTR = mean(off_conf_home_perc_FTR, na.rm = T),
                                 off_non_conf_home_perc_FTR = mean(off_non_conf_home_perc_FTR, na.rm = T),
                                 def_home_perc_FTR = mean(def_home_perc_FTR, na.rm = T),
                                 def_conf_home_perc_FTR  = mean(def_conf_home_perc_FTR , na.rm = T),
                                 def_non_conf_home_perc_FTR  = mean(def_non_conf_home_perc_FTR , na.rm = T)))

FTR_result_df <- as.data.frame(rbind(team_one_FTR, team_two_FTR))
rownames(FTR_result_df) <- c(off_string, def_string)



####
####
####



reg_vec # 2 rows

comp_team_one_vec # 2 rows

comp_team_two_vec # 2 rows

ppp_result_df # 2 rows

`2P_result_df` # 2 rows

`3P_result_df` # 2 rows

STL_result_df # 2 rows

TOV_result_df # 2 rows

FTR_result_df # 2 rows

ORB_result_df # 2 rows

poss_result_df # 2 rows

#


# Create workbook and add worksheet
wb <- createWorkbook()
sheet_name <- substr(paste0("GamePred - ", def_string, " vs ", off_string), 1, 31)
addWorksheet(wb, sheet_name)

# Put your data frames (and vectors) into a list in the desired order
df_list <- list(
  "XGBoost"           = reg_vec,
  "Houston - Comp" = comp_team_one_vec,
  "Florida - Comp" = comp_team_two_vec,
  "PPP"     = ppp_result_df,
  "2P"      = `2P_result_df`,
  "3P"      = `3P_result_df`,
  "STL"     = STL_result_df,
  "TOV"     = TOV_result_df,
  "FTR"     = FTR_result_df,
  "ORB"     = ORB_result_df,
  "POSS"    = poss_result_df
)

# Each block gets 3 rows (1 row for the name, 2 rows for data)
# and add a gap row between blocks: total 4 rows per block.
num_blocks <- length(df_list)
start_rows <- seq(1, by = 4, length.out = num_blocks)

for(i in seq_along(df_list)) {
  # Write the data frame name in the first row of the block
  writeData(wb, sheet = sheet_name,
            x = data.frame(Category = names(df_list)[i]),
            startRow = start_rows[i], colNames = FALSE, rowNames = TRUE)
  
  # Write the actual data frame, keeping row names, starting two rows below
  writeData(wb, sheet = sheet_name,
            x = as.data.frame(df_list[[i]]),
            startRow = start_rows[i] + 2, colNames = TRUE, rowNames = TRUE)
}

# Save workbook (set the path as needed)
saveWorkbook(wb, file.path("C:/Users/AndLi/Downloads/The Big March One", paste0(sheet_name, ".xlsx")), overwrite = TRUE)
