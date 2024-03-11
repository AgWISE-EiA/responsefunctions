
#################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
#################################################################################################################
packages_required <- c("doParallel", "foreach", "h2o", "tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


#################################################################################################################
# 2. train the models  
#################################################################################################################
#################################################################################################################

country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"


#' @param nthread By default, h2o will spun all available CPU’s but you can specify a specific number of CPU’s to initialize using nthread
train_ML_H20Auto <- function(country, useCaseName, Crop, listVars=NULL, nthread){
  
  ML_inputData <- readRDS(paste("/home/jovyan/agwise/AgWise_Data/fieldData_analytics/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/", "field_geoSpatiallinked_geoSpatial_trial.RDS", sep=""))
  ML_inputData$lat <- as.numeric(ML_inputData$lat)
  ML_inputData$lon <- as.numeric(ML_inputData$lon)
  
  ML_inputData$NAME_1 <- as.factor(ML_inputData$NAME_1)
  ML_inputData$NAME_2 <- as.factor(ML_inputData$NAME_2)
  ML_inputData$season <- as.factor(ML_inputData$season)
  
  # setting up a local h2o cluster
  h2o.init()
  
  # convert our dataframe into a special object that h2o can recognize
  ML_inputData.h2o <- as.h2o(ML_inputData)
  
  
  #create a random training-test split of our data
  ML_inputData.h2o <- as.h2o(ML_inputData)
  ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.8, seed = 1234)
  training_data <- ML_inputData_split[[1]]
  test_data <- ML_inputData_split[[2]]
  
  
  ## define the target and feature variables
  if(!is.null(listVars)){
    predictors <- listVars
  }else{
    predictors <- c("lon", "lat", "N", "P", "K", "totalRF", "nrRainyDays", "Rain_month1", "Rain_month2", "Rain_month3", "Rain_month4", 
                    "Tmax_mean", "Tmax_month1", 
                    "Tmax_month2", "Tmax_month3", "Tmax_month4", "Tmin_mean", "Tmin_month1", "Tmin_month2", "Tmin_month3", "Tmin_month4", "altitude",
                    "slope", "TPI", "TRI", "c_tot_top", "c_tot_bottom", "ca_top", "ca_bottom", "clay_tot_psa_top", "clay_tot_psa_bottom", 
                    "db_od_top", "db_od_bottom", "ecec_f_top", "ecec_f_bottom", "k_top", "k_bottom", "mg_top", "mg_bottom", 
                    "n_tot_ncs_top", "n_tot_ncs_bottom", "oc_top", "oc_bottom", "p_top", "p_bottom", "ph_h2o_top", "ph_h2o_bottom", "s_top", "s_bottom",
                    "sand_tot_psa_top", "sand_tot_psa_bottom", "silt_tot_psa_top", "silt_tot_psa_bottom", "SOM_top", "SOM_bottom", "PWP_top", "PWP_bottom", 
                    "FC_top", "FC_bottom", "SWS_top", "SWS_bottom", "K_0_30", "N_0_30", "P_0_30", "Ptot_0_30")
    
  }
  
  response <- "Yield"

  
  ################# Testing different models #################
  ### 1. Auto ML ****************************************************************************************
      ## A quick and raw way to look at the way different models perform on the data:
      autoML <- h2o.automl(x = predictors, 
                           y = response,
                           training_frame = training_data,
                           validation_frame = test_data,
                           max_models = 15,
                           seed = 44)
      
      # Get the leaderboard with model performance to select the top models
      leaderboard <- autoML@leaderboard
      leaderboard <- as.data.frame(leaderboard)
      print(leaderboard)
      bestModID <- leaderboard$model_id[1]
      
      #It seems the gradient boosting machine followed by Stacked Ensemble all models are best performers. 
      ## Get more information on the best model by calling:
     bestMOd <- h2o.get_best_model(autoML)
     ntrees <- bestMOd@model$model_summary$number_of_trees
     minDepth <- bestMOd@model$model_summary$min_depth
     maxDepth <- bestMOd@model$model_summary$max_depth
     
     bestMod_info <- data.frame(modelID= bestModID, ntrees = ntrees, minDepth=minDepth, maxDepth=maxDepth)
     
  
 ### 2. fitting the best model: given the best model is unidentifiable without supervision, it is not easy to automate this step *****************************************************************************
      ## (gradient boosting machine in the example) after tuning the hyper parameters
      ## Specify the hyper-parameter grid: test some values around the out put of h2o.get_best_model(autoML)
        hyperparams_gbm <- list(
          ntrees = seq(bestModData$ntrees - 30, bestModData$ntrees + 30, 10),
          max_depth = seq(bestModData$minDepth - 3, bestModData$maxDepth +3, 2)
        )
      
        # Train and tune the gradient boosting model
        grid_gbm <- h2o.grid(
          algorithm = "gbm",
          x = predictors,
          y = response,
          grid_id = "hyperparams_gbm",
          hyper_params = hyperparams_GBM_3,
          training_frame = training_data,
          validation_frame = test_data,
          seed = 444
        )
        
        # Get the best hyper parameters
        best_hyperParm <- h2o.getModel(grid_gbm@model_ids[[1]])
        print(best_hyperParm@parameters) 
        
        ntrees_gbm_optim <- best_hyperParm@parameters$ntrees
        max_depth_gbm_optim <- best_hyperParm@parameters$max_depth
      
        ### fit the model with the tuned hyper parameters: ntrees = 40 and max_depth=8
            ML_gbm <- h2o.gbm(x = predictors,
                              y = response,
                              ntrees = ntrees_gbm_optim,
                              max_depth = max_depth_gbm_optim,
                              training_frame = training_data,
                              validation_frame = test_data,
                              seed = 444)
          
          rmse_r2_gbm <- data.frame( rmse = round(h2o.rmse(ML_gbm, train=TRUE, valid=TRUE)[[2]], 3),
                                              R_sq = round(h2o.r2(ML_gbm, train=TRUE, valid=TRUE)[[2]], 3))
          rmse_r2_gbm
          
          bestMod_tuened <- data.frame(ntrees = ntrees_gbm_optim, maxDepth=max_depth_gbm_optim, R2 = rmse_r2_gbm$R_sq, rmse = rmse_r2_gbm$rmse)
          
          
          
          GBM_valid <- test_data
          GBM_valid$predYield <- h2o.predict(object = ML_gbm, newdata = test_data)
          GBM_valid <- as.data.frame(GBM_valid)
          ggplot(GBM_valid, aes(Yield, predYield)) +
            geom_point() +
            geom_abline(slope = 1, intercept = 0) +
            xlab("Measured yield") + ylab("predicted yield")+
            ggtitle(bestMod_info$modelID) +
            xlim(0,max(ML_inputData$Yield)) + ylim(0,max(ML_inputData$Yield))+ 
            theme_minimal()
          
          
          #the variable importance plot
          par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
          h2o.varimp_plot(ML_gbm)
          
          
          # shap values = the direction of the relationship between our features and target
          # e.g., high vlaues of total rainfall has positive contribution
          h2o.shap_summary_plot(ML_gbm, test_data)
          
              
          
          
  #### 3. Random Forest *******************************************************************
          ## for the sake of comparison, we fit random forest and ANN as well 
          
          ## Specify the hyper-parameter grid
          hyper_params <- list(
            ntrees = seq(500, 1000, 100),
            max_depth = c(3, 5, 7, 10),
            mtries = c(2, 3, 4)
          )
          
          # Train and tune the random forest model
          grid <- h2o.grid(
            algorithm = "randomForest",
            x = predictors,
            y = response,
            grid_id = "rf_grid",
            hyper_params = hyper_params,
            training_frame = training_data,
            validation_frame = test_data,
            seed = 444
          )
          
          # Get the best model from the grid search
          best_model <- h2o.getModel(grid@model_ids[[1]])
          
          # View the hyperparameters of the best model
          print(best_model@parameters) ### The best model has ntrees  = 900 and maxdepth=10 and mtries = 4
          
      
      
        ML_randomForest <- h2o.randomForest(x = predictors,
                                            y = response,
                                            ntrees = 900,
                                            max_depth = 10,
                                            mtries = 4,
                                            training_frame = training_data,
                                            validation_frame = test_data,
                                            seed = 444)
        
       
        rmse_r2_randomforest <- data.frame( rmse =h2o.rmse(ML_randomForest, train=TRUE, valid=TRUE)[[2]],
                               R_sq = c(h2o.r2(ML_randomForest, train=TRUE, valid=TRUE)[[2]]))
        rmse_r2_randomforest ## a huge improvment from lm
        
        rf_valid <- test_data
        rf_valid$predYield <- h2o.predict(object = ML_randomForest, newdata = test_data)
        rf_valid <- as.data.frame(rf_valid)
        ggplot(rf_valid, aes(Yield, predYield)) +
          geom_point() +
          geom_abline(slope = 1, intercept = 0) +
          xlab("Measured yield") + ylab("predicted yield")+
          ggtitle("Random forest") +
          xlim(0,max(ML_inputData$Yield)) + ylim(0,max(ML_inputData$Yield))+ 
          theme_minimal()
        
        
        #the variable importance plot
        par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
        h2o.varimp_plot(ML_randomForest)
        
        
        # shap values = the direction of the relationship between our features and target
        # e.g., high vlaues of total rainfall has positive contribution
        h2o.shap_summary_plot(ML_randomForest, test_data)

  
  ### 4. deep learning / ANN ********************************************************
  
          # Specify the hyper-parameter grid
          hyper_params_ANN <- list(
            hidden = list(c(20, 20)),
            activation = c("Rectifier", "Tanh", "Maxout"),
            epochs = c(100, 500, 1000),
            l1 = c(0, 0.001),
            l2 = c(0, 0.001)
          )
          
          
          # Train and tune the deep learning model
          grid_ANN <- h2o.grid(
            algorithm = "deeplearning",
            x = predictors,
            y = response,
            grid_id = "dl_grid",
            hyper_params = hyper_params_ANN,
            training_frame = training_data,
            validation_frame = test_data,
            seed = 444
          )
          
          # Get the best model from the grid search
          best_HP_ANN <- h2o.getModel(grid_ANN@model_ids[[1]])
          print(best_HP_ANN@parameters)
          
          # run the model with optimized hyper-parameters
          ML_ANN <- h2o.deeplearning(x = predictors,
                                       y = response,
                                       hidden = c(20,20),
                                       epochs = 100,
                                       reproducible = TRUE,
                                       activation = "Tanh",
                                       l2 = 0.001,
                                       seed = 444,
                                       training_frame = training_data,
                                       validation_frame = test_data,
                                     )
          
          rmse_r2_ANN <- data.frame( rmse =h2o.rmse(ML_ANN, train=TRUE, valid=TRUE)[[2]],
                                              R_sq = c(h2o.r2(ML_ANN, train=TRUE, valid=TRUE)[[2]]))
          rmse_r2_ANN ## random forest is bettter 
          
          ANN_valid <- test_data
          ANN_valid$predYield <- h2o.predict(object = ML_ANN, newdata = test_data)
          ANN_valid <- as.data.frame(ANN_valid)
          ggplot(ANN_valid, aes(Yield, predYield)) +
            geom_point() +
            geom_abline(slope = 1, intercept = 0) +
            xlab("Measured yield") + ylab("predicted yield")+
            ggtitle("Artificail Nueral Network") +
            xlim(0,max(ML_inputData$Yield)) + ylim(0,max(ML_inputData$Yield))+ 
            theme_minimal()
          
          #the variable importance plot
          par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
          h2o.varimp_plot(ML_ANN)
          
          
  return(list(bestMod_info, ))
 
  
}


#Explanation Plotting Functions#
#There are a number of individual plotting functions that are used inside the explain() function. Some of these functions take a group of models as input and others just evaluate a single model at a time. The following functions take a list of models (including an AutoML object or an H2OFrame with model_id column, e.g., the Leaderboard) as input:
#When h2o.explain() is provided a single model, we get the following global explanations:
#Residual Analysis (regression only)
#Variable Importance
#Partial Dependence (PD) Plots
#Individual Conditional Expectation (ICE) Plots
# Methods for an AutoML object
h2o.varimp_heatmap(autoML)
h2o.model_correlation_heatmap(autoML,
                                test_data,
                                cluster_models = TRUE,
                                triangular = TRUE)
h2o.pd_multi_plot(autoML,
                    test_data,
                    column = "N")

  #These functions take a single H2O model as input, here we start with gbm:
  h2o.residual_analysis_plot(ML_gbm,test_data)
  h2o.varimp_plot(ML_gbm)
  h2o.shap_explain_row_plot(ML_gbm,test_data,row_index = 1)
  h2o.shap_summary_plot(ML_gbm,test_data)
  h2o.pd_plot(ML_gbm,test_data,column = "N")
  h2o.ice_plot(ML_gbm,test_data,column = "N")

  #Explanation with ML_randomForest
  
  h2o.residual_analysis_plot(ML_randomForest,test_data)
  h2o.varimp_plot(ML_randomForest)
  h2o.shap_explain_row_plot(ML_randomForest,test_data,row_index = 1)
  h2o.shap_summary_plot(ML_randomForest,test_data)
  h2o.pd_plot(ML_randomForest,test_data,column = "N")
  h2o.ice_plot(ML_randomForest,test_data,column = "N")








valData <- readRDS(paste("/home/jovyan/agwise/AgWise_Data/fieldData_analytics/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/", "field_geoSpatiallinked_geoSpatial_AOI.RDS", sep=""))

Nrate_pred <- c(seq(min(trData$N), (max(trData$N)+10), 10))
Prate_pred <- c(seq(min(trData$P), (max(trData$P)+10), 10))
Krate_pred <- c(seq(min(trData$K), (max(trData$K)+10), 10))


expand_grid(Nrate_pred, Prate_pred, Krate_pred)




## Defining a Stacked Ensemble Model library(h2o)
h2o.init()

# import the higgs_train_5k train and test datasets
train <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/testng/higgs_train_5k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/testng/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# Convert the response column in train and test datasets to a factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])


# Set number of folds for base learners
nfolds <- 3

# Train & Cross-validate a GBM model
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  ntrees = 10,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# Train & Cross-validate an RF model
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 10,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)


# Next we can train a few different ensembles using different metalearners

# Train a stacked ensemble using the default metalearner algorithm
stack <- h2o.stackedEnsemble(x = x,
                             y = y,
                             training_frame = train,
                             base_models = list(my_gbm, my_rf))
h2o.auc(h2o.performance(stack, test))
# 0.7570171

# Train a stacked ensemble using GBM as the metalearner algorithm
# The metalearner will use GBM default values
stack_gbm <- h2o.stackedEnsemble(x = x,
                                 y = y,
                                 training_frame = train,
                                 base_models = list(my_gbm, my_rf),
                                 metalearner_algorithm = "gbm")
h2o.auc(h2o.performance(stack_gbm, test))
# 0.7511055

# Train a stacked ensemble using RF as the metalearner algorithm
# The metalearner will use RF default values
stack_rf <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = list(my_gbm, my_rf),
                                metalearner_algorithm = "drf")
h2o.auc(h2o.performance(stack_rf, test))
# 0.7232461

# Train a stacked ensemble using Deep Learning as the metalearner algorithm
# The metalearner will use RF default values
stack_dl <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = list(my_gbm, my_rf),
                                metalearner_algorithm = "deeplearning")
h2o.auc(h2o.performance(stack_dl, test))
# 0.7571556

#Explanation Plotting Functions#
#There are a number of individual plotting functions that are used inside the explain() function. Some of these functions take a group of models as input and others just evaluate a single model at a time. The following functions take a list of models (including an AutoML object or an H2OFrame with model_id column, e.g., the Leaderboard) as input:
#When h2o.explain() is provided a single model, we get the following global explanations:
#Residual Analysis (regression only)
#Variable Importance
#Partial Dependence (PD) Plots
#Individual Conditional Expectation (ICE) Plots
# Methods for an AutoML object
h2o.varimp_heatmap(autoML)
h2o.model_correlation_heatmap(autoML,
                              test_data,
                              cluster_models = TRUE,
                              triangular = TRUE)
h2o.pd_multi_plot(autoML,
                  test_data,
                  column = "N")

#These functions take a single H2O model as input, here we start with gbm:
h2o.residual_analysis_plot(ML_gbm,test_data)
h2o.varimp_plot(ML_gbm)
h2o.shap_explain_row_plot(ML_gbm,test_data,row_index = 1)
h2o.shap_summary_plot(ML_gbm,test_data)
h2o.pd_plot(ML_gbm,test_data,column = "N")
h2o.ice_plot(ML_gbm,test_data,column = "N")

#Explanation with ML_randomForest

h2o.residual_analysis_plot(ML_randomForest,test_data)
h2o.varimp_plot(ML_randomForest)
h2o.shap_explain_row_plot(ML_randomForest,test_data,row_index = 1)
h2o.shap_summary_plot(ML_randomForest,test_data)
h2o.pd_plot(ML_randomForest,test_data,column = "N")
h2o.ice_plot(ML_randomForest,test_data,column = "N")








valData <- readRDS(paste("/home/jovyan/agwise/AgWise_Data/fieldData_analytics/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/", "field_geoSpatiallinked_geoSpatial_AOI.RDS", sep=""))

Nrate_pred <- c(seq(min(trData$N), (max(trData$N)+10), 10))
Prate_pred <- c(seq(min(trData$P), (max(trData$P)+10), 10))
Krate_pred <- c(seq(min(trData$K), (max(trData$K)+10), 10))


expand_grid(Nrate_pred, Prate_pred, Krate_pred)




########################################
## 1. H2O supports the following supervised algorithms:
#1- AutoML: Automatic Machine Learning
#2- Cox Proportional Hazards (CoxPH)
#3- Deep Learning (Neural Networks)
#4- Distributed Random Forest (DRF)
#5- Generalized Linear Model (GLM)
#6- Isotonic Regression
#7- ModelSelection
#8- Generalized Additive Models (GAM)
#9- ANOVA GLM
#10-Gradient Boosting Machine (GBM)
#11-Naïve Bayes Classifier
#12-RuleFit
#13-Stacked Ensembles
#14-Support Vector Machine (SVM)
#15-Distributed Uplift Random Forest (Uplift DRF)
#16-XGBoost
###let’s fit a Neural Network, using h2o.deeplearning:


## random forest hyper parameters
# The h2o.randomForest algorithm in H2O provides various hyper-parameters that you can tune to customize the behavior of the random forest model. Here are some commonly used hyperparameters for h2o.randomForest:
# ntrees: The number of trees to grow in the random forest. Typically, a higher number of trees can improve model performance but increase training time.
# max_depth: The maximum depth of each tree in the random forest. Controlling the depth helps control the complexity of the trees and can prevent overfitting.
# mtries: The number of features randomly selected at each split. It determines the number of features available for splitting at each node and affects the diversity and accuracy of the trees.



##ANN hyper parameters
# the h2o.deeplearning algorithm in H2O has several hyper-parameters that you can tune to customize the behavior of the deep learning model. Here are some commonly used hyperparameters for h2o.deeplearning:
# hidden: The number and sizes of hidden layers in the neural network. You can specify a vector of integers to define the number of neurons in each hidden layer.
# activation: The activation function to use in the hidden layers. Options include "Rectifier" (default), "Tanh", "TanhWithDropout", "RectifierWithDropout", "Maxout", "MaxoutWithDropout", "ExpRectifier", and "ExpRectifierWithDropout".
# epochs: The number of passes over the training data (epochs) during training. More epochs can lead to better model performance but may increase training time.
# l1: The L1 regularization strength. It helps to control the complexity of the model and prevent overfitting by adding an L1 penalty term to the loss function.
# l2: The L2 regularization strength. Similar to L1 regularization, it adds an L2 penalty term to the loss function to control model complexity and prevent overfitting.



