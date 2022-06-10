# BOOSTED TREE
# finally let's use the boosted tree
boost_soccer <- boost_tree(trees = 2222, tree_depth = 4) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# try it first without tuning
boost_soccer_fit <- fit(boost_soccer, 
                        result ~ ck+cmp+cmp_2+sh+so_t+poss+crd_y+crd_r+fls+venue, 
                        data = soccer_train)

vip(boost_soccer_fit)

# TUNING
# tune the parameter trees
boost_tune_wkflow <- workflow() %>%
  add_model(boost_soccer %>% set_args(trees = tune())) %>%
  add_formula(result ~ ck+cmp+cmp_2+sh+so_t+poss+crd_y+crd_r+fls+venue)

set.seed(2000)
soccer_grid_boost <- grid_regular(trees(range = c(10,2000)))
boost_tune_res <- tune_grid(
  boost_tune_wkflow, 
  resamples = soccer_folds, 
  grid = soccer_grid_boost, 
  metrics = metric_set(accuracy)
)

autoplot(boost_tune_res)

collect_metrics(boost_tune_res)

boost_best_tuned <- select_best(boost_tune_res) # find the best tuned model
boost_best_tuned

boost_tuned_final <- finalize_workflow(boost_tune_wkflow, boost_best_tuned) # fit on the training set
boost_tuned_fit <- fit(boost_tuned_final, data = soccer_train)
boost_tuned_fit %>%
  extract_fit_engine()

boost_pre_tuned <- predict(boost_tuned_fit, new_data = soccer_train, type = "prob")
boost_pre_tuned <- bind_cols(boost_pre_tuned, soccer_train) 
boost_pre_tuned

augment(boost_tuned_fit, new_data = soccer_train) %>% 
  roc_auc(result, .pred_W) 

augment(boost_tuned_fit, new_data = soccer_train) %>%
  roc_curve(truth = result, estimate = .pred_W) %>%
  autoplot()

augment(boost_tuned_fit, new_data = soccer_train) %>%
  conf_mat(truth = result, estimate = .pred_class) 





