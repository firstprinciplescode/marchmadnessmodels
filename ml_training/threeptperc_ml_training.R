final_df_threeptperc <- final_df %>% select(`3P%`, off_mean_def_home_perc_PPP:away_b2b_ind)

colnames(final_df_threeptperc) <- gsub('%', '', colnames(final_df_threeptperc))
colnames(final_df_threeptperc)[1] <- "threeptperc"


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
task_threeptperc <- TaskRegr$new(id = "threeptperc_task", backend = final_df_threeptperc, target = "threeptperc")

# Define learner with parameter names set, values to be tuned
learner_threeptperc <- lrn("regr.xgboost")

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
  task = task_threeptperc,
  learner = learner_threeptperc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Run Bayesian optimization
at <- auto_tuner(
  tuner = tnr("mbo"),
  learner = learner_threeptperc,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 40)
)

# Train the tuning instance
at$train(task_threeptperc)

# View best hyperparameters
at$archive$best()


###
###
###



sample_split_threeptperc <- sample.split(Y = final_df_threeptperc$threeptperc, SplitRatio = 0.85)
train_set_threeptperc <- subset(x = final_df_threeptperc, sample_split_threeptperc == TRUE)
test_set_threeptperc <- subset(x = final_df_threeptperc, sample_split_threeptperc == FALSE)


X_train_threeptperc <- train_set_threeptperc %>% select(-threeptperc) %>% as.data.frame()
X_test_threeptperc <- test_set_threeptperc %>% select(-threeptperc) %>% as.data.frame()
y_train_threeptperc <- train_set_threeptperc$threeptperc
y_test_threeptperc <- test_set_threeptperc$threeptperc


dtrain_threeptperc = xgb.DMatrix(data = as.matrix(X_train_threeptperc), label = y_train_threeptperc)
dtest_threeptperc = xgb.DMatrix(data =as.matrix(X_test_threeptperc), label = y_test_threeptperc)

d_xpass_all = final_df_threeptperc %>% select(-threeptperc)
d_ypass_all = final_df_threeptperc$threeptperc
d_all_pass_all = xgb.DMatrix(data = as.matrix(d_xpass_all), label = d_ypass_all)

watchlist_threeptperc = list(train=dtrain_threeptperc, test=dtest_threeptperc)


eta_threeptperc = .105 # .012
gamma_threeptperc = .254 # .086
max_depth_threeptperc = 9 # 6
min_child_weight_threeptperc = 4.899 # 1.98
alpha_threeptperc = .985 # .54
lambda_threeptperc = .274 # 1
colsample_bynode_threeptperc = .249 # .405
colsample_bylevel_threeptperc = .536 # .564
colsample_bytree_threeptperc = .879 # .722

xgb_threeptperc_route <- xgboost(data = dtrain_threeptperc, 
                               label = y_train_threeptperc, 
                               eta = eta_threeptperc,
                               max_depth = max_depth_threeptperc, 
                               alpha = alpha_threeptperc,
                               lambda = lambda_threeptperc,
                               min_child_weight = min_child_weight_threeptperc,
                               colsample_bynode = colsample_bynode_threeptperc,
                               colsample_bytree = colsample_bytree_threeptperc,
                               colsample_bylevel = colsample_bylevel_threeptperc,
                               nround = 2038, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_threeptperc,
                               early_stopping_rounds = 50
)


test_preds = predict(xgb_threeptperc_route, newdata = xgb.DMatrix(X_test_threeptperc %>% as.matrix()))

resultant_df_threeptperc = cbind(test_preds, y_test_threeptperc) %>% as.data.frame()
colnames(resultant_df_threeptperc) = c("Preds", "Vals")

summary(test_preds)

time_to_throw_breaks <- seq(.18, .5, by = 0.02)

time_to_throw_labels <- sprintf("%.3f", head(time_to_throw_breaks, -1))

resultant_df_threeptperc$buckets <- cut(resultant_df_threeptperc$Preds, time_to_throw_breaks, labels = time_to_throw_labels, include.lowest = TRUE, right = FALSE)


names_threeptperc = colnames(dtrain_threeptperc)

importance_matrix_threeptperc <- xgb.importance(names_threeptperc, model = xgb_threeptperc_route)
importance_matrix_threeptperc

predictions <- predict(xgb_threeptperc_route, newdata = dtest_threeptperc, type = "response")

rmse_value <- rmse(predictions, y_test_threeptperc)
rmse_value 

# .099 - use the most recent one


View(resultant_df_threeptperc %>% group_by(buckets) %>% dplyr::summarize(n = n(), mean = mean(Vals))
)


plot(predictions, y_test_threeptperc)
abline(lm(y_test_threeptperc ~ predictions))



xgb_threeptperc_route <- xgboost(data = as.matrix(d_xpass_all), 
                               label = d_ypass_all, 
                               eta = eta_threeptperc,
                               max_depth = max_depth_threeptperc, 
                               alpha = alpha_threeptperc,
                               lambda = lambda_threeptperc,
                               min_child_weight = min_child_weight_threeptperc,
                               colsample_bynode = colsample_bynode_threeptperc,
                               colsample_bytree = colsample_bytree_threeptperc,
                               colsample_bylevel = colsample_bylevel_threeptperc,
                               nround = 2038, # 1774
                               objective = "reg:squarederror",
                               nthread = 2,
                               gamma = gamma_threeptperc,
                               early_stopping_rounds = 50
)

predictions <- predict(xgb_threeptperc_route, newdata = dtest_threeptperc, type = "response")

plot(predictions, y_test_threeptperc)
abline(lm(y_test_threeptperc ~ predictions))

names_threeptperc = colnames(dtrain_threeptperc)

rmse_value <- rmse(predictions, y_test_threeptperc)
rmse_value # .096


importance_matrix_threeptperc <- xgb.importance(names_threeptperc, model = xgb_threeptperc_route)
importance_matrix_threeptperc
