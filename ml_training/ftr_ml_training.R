final_df_FTR <- final_df %>% select(FTR, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_FTR) <- gsub('%', '', colnames(final_df_FTR))


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
task_FTR <- TaskRegr$new(id = "FTR_task", backend = final_df_FTR, target = "FTR")

# Define learner with parameter names set, values to be tuned
learner_FTR <- lrn("regr.xgboost")

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
  task = task_FTR,
  learner = learner_FTR,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_FTR,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Train the tuning instance
at$train(task_FTR)

# View best hyperparameters
at$archive$best()


###
###
###



sample_split_FTR <- sample.split(Y = final_df_FTR$FTR, SplitRatio = 0.85)
train_set_FTR <- subset(x = final_df_FTR, sample_split_FTR == TRUE)
test_set_FTR <- subset(x = final_df_FTR, sample_split_FTR == FALSE)


X_train_FTR <- train_set_FTR %>% select(-FTR) %>% as.data.frame()
X_test_FTR <- test_set_FTR %>% select(-FTR) %>% as.data.frame()
y_train_FTR <- train_set_FTR$FTR
y_test_FTR <- test_set_FTR$FTR


dtrain_FTR = xgb.DMatrix(data = as.matrix(X_train_FTR), label = y_train_FTR)
dtest_FTR = xgb.DMatrix(data =as.matrix(X_test_FTR), label = y_test_FTR)

d_xpass_all = final_df_FTR %>% select(-FTR)
d_ypass_all = final_df_FTR$FTR
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_FTR = list(train=dtrain_FTR, test=dtest_FTR)


eta_FTR = .08 # .012
gamma_FTR = .818 # .086
max_depth_FTR = 10 # 6
min_child_weight_FTR = 3.585 # 1.98
alpha_FTR = .75 # .54
lambda_FTR = .27 # 1
colsample_bynode_FTR = .909 # .405
colsample_bylevel_FTR = .418 # .564
colsample_bytree_FTR = .415 # .722

xgb_FTR_route <- xgboost(data = dtrain_FTR, 
                              label = y_train_FTR, 
                              eta = eta_FTR,
                              max_depth = max_depth_FTR, 
                              alpha = alpha_FTR,
                              lambda = lambda_FTR,
                              min_child_weight = min_child_weight_FTR,
                              colsample_bynode = colsample_bynode_FTR,
                              colsample_bytree = colsample_bytree_FTR,
                              colsample_bylevel = colsample_bylevel_FTR,
                              nround = 2348, # 1774
                              objective = "reg:squarederror",
                              nthread = 2,
                              gamma = gamma_FTR,
                              early_stopping_rounds = 50
)


test_preds = predict(xgb_FTR_route, newdata = xgb.DMatrix(X_test_FTR %>% as.matrix()))

resultant_df_FTR = cbind(test_preds, y_test_FTR) %>% as.data.frame()
colnames(resultant_df_FTR) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(.11, .26, by = 0.01)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_FTR$buckets <- cut(resultant_df_FTR$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_FTR = colnames(dtrain_FTR)

importance_matrix_FTR <- xgb.importance(names_FTR, model = xgb_FTR_route)
importance_matrix_FTR

predictions <- predict(xgb_FTR_route, newdata = dtest_FTR, type = "response")

rmse_value <- rmse(predictions, y_test_FTR)
rmse_value 

# .044 - use the most recent one


View(resultant_df_FTR %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_FTR)
abline(lm(y_test_FTR ~ predictions))



xgb_FTR_route <- xgboost(data = as.matrix(d_xpass_all), 
                              label = d_ypass_all, 
                              eta = eta_FTR,
                              max_depth = max_depth_FTR, 
                              alpha = alpha_FTR,
                              lambda = lambda_FTR,
                              min_child_weight = min_child_weight_FTR,
                              colsample_bynode = colsample_bynode_FTR,
                              colsample_bytree = colsample_bytree_FTR,
                              colsample_bylevel = colsample_bylevel_FTR,
                              nround = 1069, # 1774
                              objective = "reg:squarederror",
                              nthread = 2,
                              gamma = gamma_FTR,
                              early_stopping_rounds = 50
)

predictions <- predict(xgb_FTR_route, newdata = dtest_FTR, type = "response")

plot(predictions, y_test_FTR)
abline(lm(y_test_FTR ~ predictions))

names_FTR = colnames(dtrain_FTR)

rmse_value <- rmse(predictions, y_test_FTR)
rmse_value # .043


importance_matrix_FTR <- xgb.importance(names_FTR, model = xgb_FTR_route)
importance_matrix_FTR
