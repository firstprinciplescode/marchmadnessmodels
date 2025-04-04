final_df_poss <- final_df %>% select(poss, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_poss) <- gsub('%', '', colnames(final_df_poss))

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
task_poss <- TaskRegr$new(id = "poss_task", backend = final_df_poss, target = "poss")

# Define learner with parameter names set, values to be tuned
learner_poss <- lrn("regr.xgboost")

# Define parameter search space based on given parameter names
param_set <- ps(
  eta = p_dbl(lower = 0.01, upper = 0.1),
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
  task = task_poss,
  learner = learner_poss,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 50)
)

# Run Bayesian optimization
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_poss,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 50)
)

# Train the tuning instance
at$train(task_poss)

# View best hyperparameters
at$archive$best()


###
###
###


sample_split_poss <- sample.split(Y = final_df_poss$poss, SplitRatio = 0.85)
train_set_poss <- subset(x = final_df_poss, sample_split_poss == TRUE)
test_set_poss <- subset(x = final_df_poss, sample_split_poss == FALSE)


X_train_poss <- train_set_poss %>% select(-poss) %>% as.data.frame()
X_test_poss <- test_set_poss %>% select(-poss) %>% as.data.frame()
y_train_poss <- train_set_poss$poss
y_test_poss <- test_set_poss$poss


dtrain_poss = xgb.DMatrix(data = as.matrix(X_train_poss), label = y_train_poss)
dtest_poss = xgb.DMatrix(data =as.matrix(X_test_poss), label = y_test_poss)

d_xpass_all = final_df_poss %>% select(-poss)
d_ypass_all = final_df_poss$poss
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_poss = list(train=dtrain_poss, test=dtest_poss)


eta_poss = .034 # .012
gamma_poss = 4.666 # .086
max_depth_poss = 7 # 6
min_child_weight_poss = 6 # 1.98
alpha_poss = .22 # .54
lambda_poss = 1 # 1
colsample_bynode_poss = .709 # .405
colsample_bylevel_poss = .783 # .564
colsample_bytree_poss = .841 # .722

xgb_poss_route <- xgboost(data = dtrain_poss, 
                               label = y_train_poss, 
                               eta = eta_poss,
                               max_depth = max_depth_poss, 
                               alpha = alpha_poss,
                               lambda = lambda_poss,
                               min_child_weight = min_child_weight_poss,
                               colsample_bynode = colsample_bynode_poss,
                               colsample_bytree = colsample_bytree_poss,
                               colsample_bylevel = colsample_bylevel_poss,
                               nround = 327, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_poss,
                               early_stopping_rounds = 50
)


test_preds = predict(xgb_poss_route, newdata = xgb.DMatrix(X_test_poss %>% as.matrix()))

resultant_df_poss = cbind(test_preds, y_test_poss) %>% as.data.frame()
colnames(resultant_df_poss) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(55, 90, by = 1)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_poss$buckets <- cut(resultant_df_poss$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_poss = colnames(dtrain_poss)

importance_matrix_poss <- xgb.importance(names_poss, model = xgb_poss_route)
importance_matrix_poss

predictions <- predict(xgb_poss_route, newdata = dtest_poss, type = "response")

rmse_value <- rmse(predictions, y_test_poss)
rmse_value 

# 4.69 - use the most recent one


View(resultant_df_poss %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_poss)
abline(lm(y_test_poss ~ predictions))



xgb_poss_route <- xgboost(data = as.matrix(d_xpass_all), 
                               label = d_ypass_all, 
                               eta = eta_poss,
                               max_depth = max_depth_poss, 
                               alpha = alpha_poss,
                               lambda = lambda_poss,
                               min_child_weight = min_child_weight_poss,
                               colsample_bynode = colsample_bynode_poss,
                               colsample_bytree = colsample_bytree_poss,
                               colsample_bylevel = colsample_bylevel_poss,
                               nround = 327, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_poss,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_poss_route, newdata = dtest_poss, type = "response")

plot(predictions, y_test_poss)
abline(lm(y_test_poss ~ predictions))

names_poss = colnames(dtrain_poss)

rmse_value <- rmse(predictions, y_test_poss)
rmse_value # 4.27


importance_matrix_poss <- xgb.importance(names_poss, model = xgb_poss_route)
importance_matrix_poss
