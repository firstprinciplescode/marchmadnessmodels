final_df_TOV_perc <- final_df %>% select(TOV_perc, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_TOV_perc) <- gsub('%', '', colnames(final_df_TOV_perc))


library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3verse)
library(paradox)
library(rgenoud)
library(ranger)

library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(factoextra)
library(NbClust)
library(xgboost)
library(caret)
library(caTools)
library(Metrics)
library(cvms)
library(ParBayesianOptimization)
library(doParallel)
library(stringr)
library(pROC)
library(olsrr)
library(stats)
library(ROCR)
library(gridExtra)
library(mgcv)


# Define task
task_TOV_perc <- TaskRegr$new(id = "TOV_perc_task", backend = final_df_TOV_perc, target = "TOV_perc")

# Define learner with parameter names set, values to be tuned
learner_TOV_perc <- lrn("regr.xgboost")

# Define parameter search space based on given parameter names
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.12),
  gamma = p_dbl(lower = 0, upper = 5),
  max_depth = p_int(lower = 2, upper = 10),
  min_child_weight = p_dbl(lower = 0, upper = 12),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 3000)
)

# Define tuning instance
instance <- TuningInstanceSingleCrit$new(
  task = task_TOV_perc,
  learner = learner_TOV_perc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_TOV_perc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Train the tuning instance
at$train(task_TOV_perc)

# View best hyperparameters
at$archive$best()


###
###
###



sample_split_TOV_perc <- sample.split(Y = final_df_TOV_perc$TOV_perc, SplitRatio = 0.85)
train_set_TOV_perc <- subset(x = final_df_TOV_perc, sample_split_TOV_perc == TRUE)
test_set_TOV_perc <- subset(x = final_df_TOV_perc, sample_split_TOV_perc == FALSE)


X_train_TOV_perc <- train_set_TOV_perc %>% select(-TOV_perc) %>% as.data.frame()
X_test_TOV_perc <- test_set_TOV_perc %>% select(-TOV_perc) %>% as.data.frame()
y_train_TOV_perc <- train_set_TOV_perc$TOV_perc
y_test_TOV_perc <- test_set_TOV_perc$TOV_perc


dtrain_TOV_perc = xgb.DMatrix(data = as.matrix(X_train_TOV_perc), label = y_train_TOV_perc)
dtest_TOV_perc = xgb.DMatrix(data =as.matrix(X_test_TOV_perc), label = y_test_TOV_perc)

d_xpass_all = final_df_TOV_perc %>% select(-TOV_perc)
d_ypass_all = final_df_TOV_perc$TOV_perc
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_TOV_perc = list(train=dtrain_TOV_perc, test=dtest_TOV_perc)


eta_TOV_perc = .069 # .012
gamma_TOV_perc = .478 # .086
max_depth_TOV_perc = 8 # 6
min_child_weight_TOV_perc = 2.218 # 1.98
alpha_TOV_perc = .158 # .54
lambda_TOV_perc = .215 # 1
colsample_bynode_TOV_perc = .792 # .405
colsample_bylevel_TOV_perc = .326 # .564
colsample_bytree_TOV_perc = .565 # .722

xgb_TOV_perc_route <- xgboost(data = dtrain_TOV_perc, 
                              label = y_train_TOV_perc, 
                              eta = eta_TOV_perc,
                              max_depth = max_depth_TOV_perc, 
                              alpha = alpha_TOV_perc,
                              lambda = lambda_TOV_perc,
                              min_child_weight = min_child_weight_TOV_perc,
                              colsample_bynode = colsample_bynode_TOV_perc,
                              colsample_bytree = colsample_bytree_TOV_perc,
                              colsample_bylevel = colsample_bylevel_TOV_perc,
                              nround = 1069, # 1774
                              objective = "reg:squarederror",
                              nthread = 2,
                              gamma = gamma_TOV_perc,
                              early_stopping_rounds = 50
)


test_preds = predict(xgb_TOV_perc_route, newdata = xgb.DMatrix(X_test_TOV_perc %>% as.matrix()))

resultant_df_TOV_perc = cbind(test_preds, y_test_TOV_perc) %>% as.data.frame()
colnames(resultant_df_TOV_perc) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(.11, .26, by = 0.01)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_TOV_perc$buckets <- cut(resultant_df_TOV_perc$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_TOV_perc = colnames(dtrain_TOV_perc)

importance_matrix_TOV_perc <- xgb.importance(names_TOV_perc, model = xgb_TOV_perc_route)
importance_matrix_TOV_perc

predictions <- predict(xgb_TOV_perc_route, newdata = dtest_TOV_perc, type = "response")

rmse_value <- rmse(predictions, y_test_TOV_perc)
rmse_value 

# .044 - use the most recent one


View(resultant_df_TOV_perc %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_TOV_perc)
abline(lm(y_test_TOV_perc ~ predictions))



xgb_TOV_perc_route <- xgboost(data = as.matrix(d_xpass_all), 
                              label = d_ypass_all, 
                              eta = eta_TOV_perc,
                              max_depth = max_depth_TOV_perc, 
                              alpha = alpha_TOV_perc,
                              lambda = lambda_TOV_perc,
                              min_child_weight = min_child_weight_TOV_perc,
                              colsample_bynode = colsample_bynode_TOV_perc,
                              colsample_bytree = colsample_bytree_TOV_perc,
                              colsample_bylevel = colsample_bylevel_TOV_perc,
                              nround = 1069, # 1774
                              objective = "reg:squarederror",
                              nthread = 2,
                              gamma = gamma_TOV_perc,
                              early_stopping_rounds = 50
)

predictions <- predict(xgb_TOV_perc_route, newdata = dtest_TOV_perc, type = "response")

plot(predictions, y_test_TOV_perc)
abline(lm(y_test_TOV_perc ~ predictions))

names_TOV_perc = colnames(dtrain_TOV_perc)

rmse_value <- rmse(predictions, y_test_TOV_perc)
rmse_value # .043


importance_matrix_TOV_perc <- xgb.importance(names_TOV_perc, model = xgb_TOV_perc_route)
importance_matrix_TOV_perc
