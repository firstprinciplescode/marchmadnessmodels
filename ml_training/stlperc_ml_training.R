final_df_STL_perc <- final_df %>% select(STL_perc, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_STL_perc) <- gsub('%', '', colnames(final_df_STL_perc))


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
task_STL_perc <- TaskRegr$new(id = "STL_perc_task", backend = final_df_STL_perc, target = "STL_perc")

# Define learner with parameter names set, values to be tuned
learner_STL_perc <- lrn("regr.xgboost")

# Define parameter search space based on given parameter names
param_set <- ps(
  eta = p_dbl(lower = 0.001, upper = 0.12),
  gamma = p_dbl(lower = 0, upper = 5),
  max_depth = p_int(lower = 2, upper = 10),
  min_child_weight = p_dbl(lower = 0, upper = 10),
  alpha = p_dbl(lower = 0, upper = 1),
  lambda = p_dbl(lower = 0, upper = 1),
  colsample_bynode = p_dbl(lower = 0, upper = 1),
  colsample_bylevel = p_dbl(lower = 0, upper = 1),
  colsample_bytree = p_dbl(lower = 0, upper = 1),
  nrounds = p_int(lower = 100, upper = 3000)
)

# Define tuning instance
instance <- TuningInstanceSingleCrit$new(
  task = task_STL_perc,
  learner = learner_STL_perc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_STL_perc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Train the tuning instance
at$train(task_STL_perc)

# View best hyperparameters
at$archive$best()


###
###
###



sample_split_STL_perc <- sample.split(Y = final_df_STL_perc$STL_perc, SplitRatio = 0.85)
train_set_STL_perc <- subset(x = final_df_STL_perc, sample_split_STL_perc == TRUE)
test_set_STL_perc <- subset(x = final_df_STL_perc, sample_split_STL_perc == FALSE)


X_train_STL_perc <- train_set_STL_perc %>% select(-STL_perc) %>% as.data.frame()
X_test_STL_perc <- test_set_STL_perc %>% select(-STL_perc) %>% as.data.frame()
y_train_STL_perc <- train_set_STL_perc$STL_perc
y_test_STL_perc <- test_set_STL_perc$STL_perc


dtrain_STL_perc = xgb.DMatrix(data = as.matrix(X_train_STL_perc), label = y_train_STL_perc)
dtest_STL_perc = xgb.DMatrix(data =as.matrix(X_test_STL_perc), label = y_test_STL_perc)

d_xpass_all = final_df_STL_perc %>% select(-STL_perc)
d_ypass_all = final_df_STL_perc$STL_perc
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_STL_perc = list(train=dtrain_STL_perc, test=dtest_STL_perc)


eta_STL_perc = .096 # .012
gamma_STL_perc = .27 # .086
max_depth_STL_perc = 9 # 6
min_child_weight_STL_perc = 7.646 # 1.98
alpha_STL_perc = .202 # .54
lambda_STL_perc = .482 # 1
colsample_bynode_STL_perc = .272 # .405
colsample_bylevel_STL_perc = .221 # .564
colsample_bytree_STL_perc = .672 # .722

xgb_STL_perc_route <- xgboost(data = dtrain_STL_perc, 
                                 label = y_train_STL_perc, 
                                 eta = eta_STL_perc,
                                 max_depth = max_depth_STL_perc, 
                                 alpha = alpha_STL_perc,
                                 lambda = lambda_STL_perc,
                                 min_child_weight = min_child_weight_STL_perc,
                                 colsample_bynode = colsample_bynode_STL_perc,
                                 colsample_bytree = colsample_bytree_STL_perc,
                                 colsample_bylevel = colsample_bylevel_STL_perc,
                                 nround = 2571, # 1774
                                 objective = "reg:squarederror",
                                 nthread = 2,
                                 gamma = gamma_STL_perc,
                                 early_stopping_rounds = 50
)


test_preds = predict(xgb_STL_perc_route, newdata = xgb.DMatrix(X_test_STL_perc %>% as.matrix()))

resultant_df_STL_perc = cbind(test_preds, y_test_STL_perc) %>% as.data.frame()
colnames(resultant_df_STL_perc) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(.05, .15, by = 0.01)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_STL_perc$buckets <- cut(resultant_df_STL_perc$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_STL_perc = colnames(dtrain_STL_perc)

importance_matrix_STL_perc <- xgb.importance(names_STL_perc, model = xgb_STL_perc_route)
importance_matrix_STL_perc

predictions <- predict(xgb_STL_perc_route, newdata = dtest_STL_perc, type = "response")

rmse_value <- rmse(predictions, y_test_STL_perc)
rmse_value 

# .0803 - use the most recent one


View(resultant_df_STL_perc %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_STL_perc)
abline(lm(y_test_STL_perc ~ predictions))



xgb_STL_perc_route <- xgboost(data = as.matrix(d_xpass_all), 
                                 label = d_ypass_all, 
                                 eta = eta_STL_perc,
                                 max_depth = max_depth_STL_perc, 
                                 alpha = alpha_STL_perc,
                                 lambda = lambda_STL_perc,
                                 min_child_weight = min_child_weight_STL_perc,
                                 colsample_bynode = colsample_bynode_STL_perc,
                                 colsample_bytree = colsample_bytree_STL_perc,
                                 colsample_bylevel = colsample_bylevel_STL_perc,
                                 nround = 2571, # 1774
                                 objective = "reg:squarederror",
                                 nthread = 2,
                                 gamma = gamma_STL_perc,
                                 early_stopping_rounds = 50
)

predictions <- predict(xgb_STL_perc_route, newdata = dtest_STL_perc, type = "response")

plot(predictions, y_test_STL_perc)
abline(lm(y_test_STL_perc ~ predictions))

names_STL_perc = colnames(dtrain_STL_perc)

rmse_value <- rmse(predictions, y_test_STL_perc)
rmse_value # .1157


importance_matrix_STL_perc <- xgb.importance(names_STL_perc, model = xgb_STL_perc_route)
importance_matrix_STL_perc
