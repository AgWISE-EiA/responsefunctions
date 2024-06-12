

#' Title
#'
#' @param pathOut path to write out the hyper parameters, the model and the validation plot
#' @param training_data 
#' @param test_data 
#' @param response 
#' @param predictors 
#'
#' @return
#' @export
#'
#' @examples
run_RF_ML <- function(pathOut, training_data, test_data, response, predictors){
  # h2o.init()
  ## Specify the hyper-parameter grid: test some values around the out put of h2o.get_best_model(auto ML)
  hyper_params <- list(
    ntrees = seq(500, 1200, 100),
    max_depth = seq(4, 12, 2),
    mtries = c(2, 3, 4, 5, 6)
  )
  
  # Train and tune the random forest model
  grid_rf <- h2o.grid(
    algorithm = "randomForest",
    x = predictors,
    y = response,
    grid_id = "rf_grid",
    hyper_params = hyper_params,
    training_frame = training_data,
    validation_frame = test_data,
    seed = 444
  )
  
  saveRDS(grid_rf, paste(pathOut, "grid_rf_hyperParam.rds", sep=""))
  # Get the best model from the grid search
  best_model <- h2o.getModel(grid_rf@model_ids[[1]])
  
  ML_randomForest <- h2o.randomForest(x = predictors,
                                      y = response,
                                      ntrees =  best_model@parameters$ntrees,
                                      max_depth = best_model@parameters$max_depth,
                                      mtries =   best_model@parameters$mtries,
                                      training_frame = training_data,
                                      validation_frame = test_data,
                                      keep_cross_validation_predictions = TRUE,
                                      nfolds = 5,
                                      seed = 444)
  
  model_path <- h2o.saveModel(object = ML_randomForest, path = pathOut, force = TRUE)
  saved_model <- h2o.loadModel(model_path)
  
  rmse_r2_randomforest <- data.frame( mae = round(h2o.mae(ML_randomForest, train=TRUE, valid=TRUE), 0),
                                      rmse =h2o.rmse(ML_randomForest, train=TRUE, valid=TRUE)[[2]],
                                      R_sq = c(h2o.r2(ML_randomForest, train=TRUE, valid=TRUE)[[2]]))
  saveRDS(rmse_r2_randomforest, paste(pathOut, "rmse_r2_randomforest.RDS"))
  

  print("Model performance evaluation plots")
  
  print("1. residual plot)")
  h2o.residual_analysis_plot(ML_randomForest,test_data)
  
  print("2. variable importance plot)")
  h2o.varimp_plot(ML_randomForest)
  
  print("3. shap summary")
  h2o.shap_summary_plot(ML_randomForest, test_data)
  
  print("4. partial plot")
  h2o.partialPlot(object = ML_randomForest, newdata = test_data, cols = c("N_rate", "P_rate"))
  
  h2o.ice_plot(ML_randomForest,test_data,column = "N_rate")
  h2o.ice_plot(ML_randomForest,test_data,column = "P_rate")

  print("5. validating by prediction")
  rf_valid <- test_data
  rf_valid$predResponse <- h2o.predict(object = ML_randomForest, newdata = test_data)
  rf_valid <- as.data.frame(rf_valid)
  rf_valid$Response <- rf_valid[,which(names(rf_valid)==response)]
  
 gg2 <-  ggplot(rf_valid, aes(Response, predResponse)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    xlab("Measured Yield") + ylab("predicted yield")+
    ggtitle("Random forest: soilGrids, iSDA") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12))
  
 ggsave(paste(pathOut, "randomForest_Validation.pdf", sep=""), gg2)
 
 print(gg2)

 return(ML_randomForest)
                  
}




