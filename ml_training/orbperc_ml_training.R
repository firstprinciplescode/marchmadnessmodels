final_df_ORB_perc <- final_df %>% select(ORB_perc, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_ORB_perc) <- gsub('%', '', colnames(final_df_ORB_perc))


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
task_ORB_perc <- TaskRegr$new(id = "ORB_perc_task", backend = final_df_ORB_perc, target = "ORB_perc")

# Define learner with parameter names set, values to be tuned
learner_ORB_perc <- lrn("regr.xgboost")

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
  task = task_ORB_perc,
  learner = learner_ORB_perc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_ORB_perc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Train the tuning instance
at$train(task_ORB_perc)

# View best hyperparameters
at$archive$best()


###
###
###



sample_split_ORB_perc <- sample.split(Y = final_df_ORB_perc$ORB_perc, SplitRatio = 0.85)
train_set_ORB_perc <- subset(x = final_df_ORB_perc, sample_split_ORB_perc == TRUE)
test_set_ORB_perc <- subset(x = final_df_ORB_perc, sample_split_ORB_perc == FALSE)


X_train_ORB_perc <- train_set_ORB_perc %>% select(-ORB_perc) %>% as.data.frame()
X_test_ORB_perc <- test_set_ORB_perc %>% select(-ORB_perc) %>% as.data.frame()
y_train_ORB_perc <- train_set_ORB_perc$ORB_perc
y_test_ORB_perc <- test_set_ORB_perc$ORB_perc


dtrain_ORB_perc = xgb.DMatrix(data = as.matrix(X_train_ORB_perc), label = y_train_ORB_perc)
dtest_ORB_perc = xgb.DMatrix(data =as.matrix(X_test_ORB_perc), label = y_test_ORB_perc)

d_xpass_all = final_df_ORB_perc %>% select(-ORB_perc)
d_ypass_all = final_df_ORB_perc$ORB_perc
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_ORB_perc = list(train=dtrain_ORB_perc, test=dtest_ORB_perc)


eta_ORB_perc = .081 # .012
gamma_ORB_perc = .056 # .086
max_depth_ORB_perc = 10 # 6
min_child_weight_ORB_perc = 2.364 # 1.98
alpha_ORB_perc = .74 # .54
lambda_ORB_perc = .443 # 1
colsample_bynode_ORB_perc = .362 # .405
colsample_bylevel_ORB_perc = .366 # .564
colsample_bytree_ORB_perc = .573 # .722

xgb_ORB_perc_route <- xgboost(data = dtrain_ORB_perc, 
                              label = y_train_ORB_perc, 
                              eta = eta_ORB_perc,
                              max_depth = max_depth_ORB_perc, 
                              alpha = alpha_ORB_perc,
                              lambda = lambda_ORB_perc,
                              min_child_weight = min_child_weight_ORB_perc,
                              colsample_bynode = colsample_bynode_ORB_perc,
                              colsample_bytree = colsample_bytree_ORB_perc,
                              colsample_bylevel = colsample_bylevel_ORB_perc,
                              nround = 141, # 1774
                              objective = "reg:squarederror",
                              nthread = 2,
                              gamma = gamma_ORB_perc,
                              early_stopping_rounds = 50
)


test_preds = predict(xgb_ORB_perc_route, newdata = xgb.DMatrix(X_test_ORB_perc %>% as.matrix()))

resultant_df_ORB_perc = cbind(test_preds, y_test_ORB_perc) %>% as.data.frame()
colnames(resultant_df_ORB_perc) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(.1, .5, by = 0.02)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_ORB_perc$buckets <- cut(resultant_df_ORB_perc$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_ORB_perc = colnames(dtrain_ORB_perc)

importance_matrix_ORB_perc <- xgb.importance(names_ORB_perc, model = xgb_ORB_perc_route)
importance_matrix_ORB_perc

predictions <- predict(xgb_ORB_perc_route, newdata = dtest_ORB_perc, type = "response")

rmse_value <- rmse(predictions, y_test_ORB_perc)
rmse_value 

# .078 - use the most recent one


View(resultant_df_ORB_perc %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_ORB_perc)
abline(lm(y_test_ORB_perc ~ predictions))



xgb_ORB_perc_route <- xgboost(data = as.matrix(d_xpass_all), 
                              label = d_ypass_all, 
                              eta = eta_ORB_perc,
                              max_depth = max_depth_ORB_perc, 
                              alpha = alpha_ORB_perc,
                              lambda = lambda_ORB_perc,
                              min_child_weight = min_child_weight_ORB_perc,
                              colsample_bynode = colsample_bynode_ORB_perc,
                              colsample_bytree = colsample_bytree_ORB_perc,
                              colsample_bylevel = colsample_bylevel_ORB_perc,
                              nround = 141, # 1774
                              objective = "reg:squarederror",
                              nthread = 2,
                              gamma = gamma_ORB_perc,
                              early_stopping_rounds = 50
)

predictions <- predict(xgb_ORB_perc_route, newdata = dtest_ORB_perc, type = "response")

plot(predictions, y_test_ORB_perc)
abline(lm(y_test_ORB_perc ~ predictions))

names_ORB_perc = colnames(dtrain_ORB_perc)

rmse_value <- rmse(predictions, y_test_ORB_perc)
rmse_value # .043


importance_matrix_ORB_perc <- xgb.importance(names_ORB_perc, model = xgb_ORB_perc_route)
importance_matrix_ORB_perc
