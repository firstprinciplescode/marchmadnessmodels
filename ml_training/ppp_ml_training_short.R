final_df_ppp <- final_df %>% select(PPP, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_ppp) <- gsub('%', '', colnames(final_df_ppp))


###
###
###


sample_split_press_off <- sample.split(Y = final_df_ppp$PPP, SplitRatio = 0.85)
train_set_press_off <- subset(x = final_df_ppp, sample_split_press_off == TRUE)
test_set_press_off <- subset(x = final_df_ppp, sample_split_press_off == FALSE)


X_train_press_off <- train_set_press_off %>% select(-PPP) %>% as.data.frame()
X_test_press_off <- test_set_press_off %>% select(-PPP) %>% as.data.frame()
y_train_press_off <- train_set_press_off$PPP
y_test_press_off <- test_set_press_off$PPP


dtrain_press_off = xgb.DMatrix(data = as.matrix(X_train_press_off), label = y_train_press_off)
dtest_press_off = xgb.DMatrix(data =as.matrix(X_test_press_off), label = y_test_press_off)

d_xpass_all = final_df_ppp %>% select(-PPP)
d_ypass_all = final_df_ppp$PPP
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_press_off = list(train=dtrain_press_off, test=dtest_press_off)


eta_press_off = .023 # .012
gamma_press_off = .315 # .086
max_depth_press_off = 11 # 6
min_child_weight_press_off = 1.395 # 1.98
alpha_press_off = .574 # .54
lambda_press_off = .262 # 1
colsample_bynode_press_off = .751 # .405
colsample_bylevel_press_off = .28 # .564
colsample_bytree_press_off = .253 # .722

xgb_press_off_route <- xgboost(data = dtrain_press_off,
                               label = y_train_press_off,
                               eta = eta_press_off,
                               max_depth = max_depth_press_off,
                               alpha = alpha_press_off,
                               lambda = lambda_press_off,
                               min_child_weight = min_child_weight_press_off,
                               colsample_bynode = colsample_bynode_press_off,
                               colsample_bytree = colsample_bytree_press_off,
                               colsample_bylevel = colsample_bylevel_press_off,
                               nround = 2900, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_press_off,
                               early_stopping_rounds = 50
)


test_preds = predict(xgb_press_off_route, newdata = xgb.DMatrix(X_test_press_off %>% as.matrix()))

resultant_df_press_off = cbind(test_preds, y_test_press_off) %>% as.data.frame()
colnames(resultant_df_press_off) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(.5, 1.5, by = 0.05)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_press_off$buckets <- cut(resultant_df_press_off$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_press_off = colnames(dtrain_press_off)

importance_matrix_press_off <- xgb.importance(names_press_off, model = xgb_press_off_route)
importance_matrix_press_off

predictions <- predict(xgb_press_off_route, newdata = dtest_press_off, type = "response")

rmse_value <- rmse(predictions, y_test_press_off)
rmse_value

# .1177 - use the most recent one


View(resultant_df_press_off %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_press_off)
abline(lm(y_test_press_off ~ predictions))



xgb_press_off_route <- xgboost(data = as.matrix(d_xpass_all),
                               label = d_ypass_all,
                               eta = eta_press_off,
                               max_depth = max_depth_press_off,
                               alpha = alpha_press_off,
                               lambda = lambda_press_off,
                               min_child_weight = min_child_weight_press_off,
                               colsample_bynode = colsample_bynode_press_off,
                               colsample_bytree = colsample_bytree_press_off,
                               colsample_bylevel = colsample_bylevel_press_off,
                               nround = 2900, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_press_off,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_press_off_route, newdata = dtest_press_off, type = "response")

plot(predictions, y_test_press_off)
abline(lm(y_test_press_off ~ predictions))

names_press_off = colnames(dtrain_press_off)

rmse_value <- rmse(predictions, y_test_press_off)
rmse_value # .1127


importance_matrix_press_off <- xgb.importance(names_press_off, model = xgb_press_off_route)
importance_matrix_press_off
