final_df_twoptperc <- final_df %>% select(`2Pperc`, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_twoptperc) <- gsub('%', '', colnames(final_df_twoptperc))
colnames(final_df_twoptperc)[1] <- "twoptperc"


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
task_twoptperc <- TaskRegr$new(id = "twoptperc_task", backend = final_df_twoptperc, target = "twoptperc")

# Define learner with parameter names set, values to be tuned
learner_twoptperc <- lrn("regr.xgboost")

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
  nrounds = p_int(lower = 100, upper = 2000)
)

# Define tuning instance
instance <- TuningInstanceSingleCrit$new(
  task = task_twoptperc,
  learner = learner_twoptperc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_twoptperc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Train the tuning instance
at$train(task_twoptperc)

# View best hyperparameters
at$archive$best()


###
###
###


sample_split_twoptperc <- sample.split(Y = final_df_twoptperc$twoptperc, SplitRatio = 0.85)
train_set_twoptperc <- subset(x = final_df_twoptperc, sample_split_twoptperc == TRUE)
test_set_twoptperc <- subset(x = final_df_twoptperc, sample_split_twoptperc == FALSE)


X_train_twoptperc <- train_set_twoptperc %>% select(-twoptperc) %>% as.data.frame()
X_test_twoptperc <- test_set_twoptperc %>% select(-twoptperc) %>% as.data.frame()
y_train_twoptperc <- train_set_twoptperc$twoptperc
y_test_twoptperc <- test_set_twoptperc$twoptperc


dtrain_twoptperc = xgb.DMatrix(data = as.matrix(X_train_twoptperc), label = y_train_twoptperc)
dtest_twoptperc = xgb.DMatrix(data =as.matrix(X_test_twoptperc), label = y_test_twoptperc)

d_xpass_all = final_df_twoptperc %>% select(-twoptperc)
d_ypass_all = final_df_twoptperc$twoptperc
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_twoptperc = list(train=dtrain_twoptperc, test=dtest_twoptperc)


eta_twoptperc = .086 # .012
gamma_twoptperc = .117 # .086
max_depth_twoptperc = 4 # 6
min_child_weight_twoptperc = 4.79 # 1.98
alpha_twoptperc = .694 # .54
lambda_twoptperc = .593 # 1
colsample_bynode_twoptperc = .691 # .405
colsample_bylevel_twoptperc = .841 # .564
colsample_bytree_twoptperc = .152 # .722

xgb_twoptperc_route <- xgboost(data = dtrain_twoptperc, 
                               label = y_train_twoptperc, 
                               eta = eta_twoptperc,
                               max_depth = max_depth_twoptperc, 
                               alpha = alpha_twoptperc,
                               lambda = lambda_twoptperc,
                               min_child_weight = min_child_weight_twoptperc,
                               colsample_bynode = colsample_bynode_twoptperc,
                               colsample_bytree = colsample_bytree_twoptperc,
                               colsample_bylevel = colsample_bylevel_twoptperc,
                               nround = 1892, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_twoptperc,
                               early_stopping_rounds = 50
)


test_preds = predict(xgb_twoptperc_route, newdata = xgb.DMatrix(X_test_twoptperc %>% as.matrix()))

resultant_df_twoptperc = cbind(test_preds, y_test_twoptperc) %>% as.data.frame()
colnames(resultant_df_twoptperc) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(.3, .76, by = 0.02)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_twoptperc$buckets <- cut(resultant_df_twoptperc$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_twoptperc = colnames(dtrain_twoptperc)

importance_matrix_twoptperc <- xgb.importance(names_twoptperc, model = xgb_twoptperc_route)
importance_matrix_twoptperc

predictions <- predict(xgb_twoptperc_route, newdata = dtest_twoptperc, type = "response")

rmse_value <- rmse(predictions, y_test_twoptperc)
rmse_value 

# .08 - use the most recent one


View(resultant_df_twoptperc %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_twoptperc)
abline(lm(y_test_twoptperc ~ predictions))



xgb_twoptperc_route <- xgboost(data = as.matrix(d_xpass_all), 
                               label = d_ypass_all, 
                               eta = eta_twoptperc,
                               max_depth = max_depth_twoptperc, 
                               alpha = alpha_twoptperc,
                               lambda = lambda_twoptperc,
                               min_child_weight = min_child_weight_twoptperc,
                               colsample_bynode = colsample_bynode_twoptperc,
                               colsample_bytree = colsample_bytree_twoptperc,
                               colsample_bylevel = colsample_bylevel_twoptperc,
                               nround = 1892, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_twoptperc,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_twoptperc_route, newdata = dtest_twoptperc, type = "response")

plot(predictions, y_test_twoptperc)
abline(lm(y_test_twoptperc ~ predictions))

names_twoptperc = colnames(dtrain_twoptperc)

rmse_value <- rmse(predictions, y_test_twoptperc)
rmse_value # .079


importance_matrix_twoptperc <- xgb.importance(names_twoptperc, model = xgb_twoptperc_route)
importance_matrix_twoptperc
