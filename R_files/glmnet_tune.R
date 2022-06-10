soccer_folds <- vfold_cv(soccer_train, v = 10, strata = result)
soccer_folds

glm_soccer_fit_folded <- fit_resamples(glm_wkflow, soccer_folds)
collect_metrics(glm_soccer_fit_folded)

library(glmnet)
soccer_tune_glmnet <- 
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

glmnet_tune_wkflow <- workflow() %>% # generate tuning workflow
  add_recipe(soccer_recipe) %>% 
  add_model(soccer_tune_glmnet)

soccer_grid_glmnet <- grid_regular(penalty(range = c(-5, 5)), mixture(range = c(0, 1)), levels = 8)

glmnet_tune_res <- tune_grid(glmnet_tune_wkflow, resamples = soccer_folds, grid = soccer_grid_glmnet)

autoplot(glmnet_tune_res)

collect_metrics(glmnet_tune_res)

glmnet_best_tuned <- select_best(glmnet_tune_res, metric = "roc_auc")
glmnet_best_tuned

glmnet_tuned_final <- finalize_workflow(glmnet_tune_wkflow, glmnet_best_tuned)
glmnet_tuned_fit <- fit(glmnet_tuned_final, data = soccer_train)

glmnet_pre_tuned <- predict(glmnet_tuned_fit, new_data = soccer_train, type = "prob")
glmnet_pre_tuned <- bind_cols(glmnet_pre_tuned, soccer_train)
glmnet_pre_tuned

augment(glmnet_tuned_fit, new_data = soccer_train) %>% 
  roc_auc(result, .pred_W)

augment(glmnet_tuned_fit, new_data = soccer_train) %>%
  roc_curve(truth = result, estimate = .pred_W) %>%
  autoplot()

augment(glmnet_tuned_fit, new_data = soccer_train) %>%
  conf_mat(truth = result, estimate = .pred_class) 





