optimal_k_eval <- function(df_train, df_test){
  
  # The same model formulation is in the previous chapter
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = df_train) |> 
    recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
    recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())
  
  #Searching for optimal k
  mod_cv <- caret::train(pp,
                         data = df_train |> drop_na(),
                         method = "knn",
                         trControl = caret::trainControl(method = "cv", number = 10),
                         tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                         metric = "MAE")
  
  #Model train
  mod <- caret::train(
    pp,
    data = df_train |> drop_na(),
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = mod_cv$finalModel$k),
    metric = "RMSE")
  
  # add predictions to the data frames
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  # get metrics tables
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  return(c(rmse_test, rsq_test))
}