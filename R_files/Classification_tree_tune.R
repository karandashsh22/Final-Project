# CLASSIFICATION TREE
# is this a better model for our problem?
library(rpart.plot)
library(vip)
library(randomForest)
library(xgboost)
tree_soccer <- decision_tree() %>%
  set_engine("rpart")
tree_soccer_class <- tree_soccer %>%
  set_mode("classification")

# TUNING
# tune the parameter cost_complexity
tree_tune_wkflow <- workflow() %>%
  add_model(tree_soccer_class %>% set_args(cost_complexity = tune())) %>%
  add_formula(result ~ cmp+cmp_2+poss+crd_y+crd_r+ck+fls+venue)

set.seed(2000)
soccer_grid_tree <- grid_regular(cost_complexity(range = c(-3, -1)), levels = 10)
tree_tune_res <- tune_grid(
  tree_tune_wkflow, 
  resamples = soccer_folds, 
  grid = soccer_grid_tree, 
  metrics = metric_set(accuracy)
)

autoplot(tree_tune_res)

collect_metrics(tree_tune_res)

# fetch our optimal tuned model
tree_best_tuned <- select_best(tree_tune_res)
tree_best_tuned

# apply it to the training set
tree_tuned_final <- finalize_workflow(tree_tune_wkflow, tree_best_tuned)
tree_tuned_fit <- fit(tree_tuned_final, data = soccer_train)
tree_tuned_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

# use it to predict training set
tree_pre_tuned <- predict(tree_tuned_fit, new_data = soccer_train, type = "prob")
tree_pre_tuned <- bind_cols(tree_pre_tuned, soccer_train)
tree_pre_tuned

augment(tree_tuned_fit, new_data = soccer_train) %>% 
  roc_auc(result, .pred_W)

augment(tree_tuned_fit, new_data = soccer_train) %>%
  roc_curve(truth = result, estimate = .pred_W) %>%
  autoplot()

augment(tree_tuned_fit, new_data = soccer_train) %>%
  conf_mat(truth = result, estimate = .pred_class) 



