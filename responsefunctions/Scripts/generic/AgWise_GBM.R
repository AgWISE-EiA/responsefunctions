

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
run_GBM_ML <- function(pathOut, training_data, test_data, response, predictors){
  # h2o.init()
  ## Specify the hyper-parameter grid: test some values around the out put of h2o.get_best_model(auto ML)
  hyperparams_gbm <- list(
    ntrees = seq(500, 1200, 100), ### is tested for diff nrtrees with seq(20, 200, 20), seq(200, 500, 50) and seq(500, 1000, 100)
    max_depth = seq(4, 10, 2)
  )
  # Train and tune the gradient boosting model
  grid_gbm <- h2o.grid(
    algorithm = "gbm",
    x = predictors,
    y = response,
    #y = response2,
    grid_id = "hyperparams_gbm",
    hyper_params = hyperparams_gbm,
    training_frame = training_data,
    validation_frame = test_data,
    seed = 444
  )
  saveRDS(grid_gbm, paste(pathOut, "grid_gbm.rds", sep=""))
  
  # Get the best hyper parameters
  best_hyperParm <- h2o.getModel(grid_gbm@model_ids[[1]])
  ntrees_gbm_optim <- best_hyperParm@parameters$ntrees
  max_depth_gbm_optim <- best_hyperParm@parameters$max_depth
  print(paste0("The best hyper parameter for nr trees and max depth are ", ntrees_gbm_optim, " & " , max_depth_gbm_optim) )
  
  ### fit the model with the tuned hyper parameters: 
  ML_gbm <- h2o.gbm(x = predictors,
                    y = response,
                    ntrees = ntrees_gbm_optim,
                    max_depth = max_depth_gbm_optim,
                    training_frame = training_data,
                    validation_frame = test_data,
                    keep_cross_validation_predictions = TRUE,
                    nfolds = 5,
                    seed = 444)
  
  model_path <- h2o.saveModel(object = ML_gbm, path = pathOut, force = TRUE)
  saved_model <- h2o.loadModel(model_path)
  
  print("Model performance evaluation plots")
  
  print("1. residual plot)")
  h2o.residual_analysis_plot(ML_gbm,test_data)
  
  print("2. mae, rmse and r sq.)")
  rmse_r2_gbm <- data.frame(mae = round(h2o.mae(ML_gbm, train=TRUE, valid=TRUE), 0),
                            rmse = round(h2o.rmse(ML_gbm, train=TRUE, valid=TRUE), 0),
                            R_sq = round(h2o.r2(ML_gbm, train=TRUE, valid=TRUE), 2))
  print(rmse_r2_gbm)
  
  saveRDS(rmse_r2_gbm, paste(pathOut, "rmse_r2_GB.RDS"))
  
  
  print("3. variable importance plot)")
  h2o.varimp_plot(ML_gbm)
  
  print("4. shap summary")
  h2o.shap_summary_plot(ML_gbm, test_data)
  
  print("5. partial plot")
  h2o.partialPlot(object = ML_gbm, newdata = test_data, cols = c("N_rate", "P_rate"))
  
  print("6. validating by prediction")
  GBM_valid <- test_data
  GBM_valid$predResponse <- h2o.predict(object = ML_gbm, newdata = test_data)
  GBM_valid <- as.data.frame(GBM_valid)
  GBM_valid$Response <- GBM_valid[,which(names(GBM_valid)==response)]
  
 gg1 <- ggplot(GBM_valid, aes(Response, predResponse)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    xlab("Measured Yield") + ylab("predicted yield")+
    ggtitle("Gradient Boosting: soilGrids, iSDA") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12))
 
 ggsave(paste(pathOut, "gradientBoosting_Validation.pdf", sep=""), gg1)
 
 print(gg1)
 
 h2o.varimp_plot(ML_gbm)
 h2o.shap_summary_plot(ML_gbm, test_data)
 h2o.partialPlot(object = ML_gbm, newdata = test_data, cols = c("N_rate", "P_rate"))
 h2o.ice_plot(ML_gbm,test_data,column = "N_rate")
 h2o.shap_explain_row_plot(ML_gbm,test_data,row_index = 100)
 return(ML_gbm)
                  
}




