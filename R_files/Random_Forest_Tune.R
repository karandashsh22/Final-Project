# RANDOM FOREST
# Then, let's use random forest
rand_fr_soccer <- rand_forest(mtry = 5) %>%
  set_engine("ranger", importance = 'impurity') %>%
  set_mode("classification")

# TUNING
# tune the parameters mtry, trees, and min_n
rand_fr_tune_wkflow <- workflow() %>%
  add_model(rand_fr_soccer %>% set_args(mtry = tune(), trees = tune(), min_n = tune())) %>%
  add_formula(result ~ sh+so_t+poss+crd_y+crd_r+fls+venue)

set.seed(2000)
soccer_grid_rand_fr <- grid_regular(mtry(range = c(1, 6)), 
                                    trees(range = c(10,2000)),
                                    min_n(range = c(1,6)))
rand_fr_tune_res <- tune_grid(
  rand_fr_tune_wkflow, 
  resamples = soccer_folds, 
  grid = soccer_grid_rand_fr, 
  metrics = metric_set(accuracy)
)

autoplot(rand_fr_tune_res)

collect_metrics(rand_fr_tune_res)

rand_fr_best_tuned <- select_best(rand_fr_tune_res) # find the optimal tuned model
rand_fr_best_tuned

rand_fr_tuned_final <- finalize_workflow(rand_fr_tune_wkflow, rand_fr_best_tuned)
rand_fr_tuned_fit <- fit(rand_fr_tuned_final, data = soccer_train)
rand_fr_tuned_fit %>% # fit it into the training set
  extract_fit_engine()

# use it predict the training set
rand_fr_pre_tuned <- predict(rand_fr_tuned_fit, new_data = soccer_train, type = "prob")
rand_fr_pre_tuned <- bind_cols(rand_fr_pre_tuned, soccer_train) # apply to predict the training set
rand_fr_pre_tuned

augment(rand_fr_tuned_fit, new_data = soccer_train) %>% # test its accuracy
  roc_auc(result, .pred_W)

augment(rand_fr_tuned_fit, new_data = soccer_train) %>%
  roc_curve(truth = result, estimate = .pred_W) %>%
  autoplot()

augment(rand_fr_tuned_fit, new_data = soccer_train) %>%
  conf_mat(truth = result, estimate = .pred_class) 














