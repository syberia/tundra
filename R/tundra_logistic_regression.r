#' Tundra logistic regression wrapper
#

tundra_logistic_regression_train_fn <- function(dataframe) {

  cat("Training Logistic Regression model (unregularized)...\n")

  require(glm)

  # get the list of predictors
  indep_vars <- setdiff(colnames(dataframe), 'dep_var')
  stopifnot(length(indep_vars) > 0)    
  
  # default parameters for the glm 
  defaults <- list(family = "binomial") 

  # set glm parameters based on user input; otherwise use defaults
  lapply(names(defaults), function(name) input[[name]] <<- input[[name]] %||% defaults[[name]])

  # get training data
  model.formula <- as.formula(paste('dep_var ~',paste(indep_vars, collapse = '+'))) 
  pred.data <- model.matrix(model.formula, dataframe[, indep_vars])

  # construct glm argument list
  gml_args <- list()
  glm_args$formula <- model.formula
  glm_args$data <- pred.data
  glm_args$family <- input$family

  # train the glm
  model <- do.call(glm, glm_args)

  # output model object and other info
  output <<- list(model = model, 
                  indep_vars = indep_vars)

  # return this thing invisibly (not quite sure what "this thing" is) 
  invisible("logistic_regression")
}

tundra_logistic_regression_predict_fn <- function(dataframe, predict_args = list()) {

  cat("Predicting using Logistic Regression Model (unregularized)\n")

  require(glm)
  
  # default parameters for prediction
  defaults <- list(prediction_type = "response")

  # set prediction parameters based on user input and training phase output; otherwise use defaults 
  lapply(names(defaults), function(name) input[[name]] <<- input[[name]] %||% defaults[[name]])

  # get the prediction data
  indep_vars <- output$indep_vars # carried over from the training stage
  model.formula <- as.formula(paste('~', paste(indep_vars, collapse = '+'))) 
  pred.data <- model.matrix(model.formula, dataframe[, indep_vars])

  # construction the glm.predict argument list
  glm_pred_args <- list()
  glm_pred_args$object <- output$model
  glm_pred_args$newdata <- pred.data
  glm_pred_args$type <- input$prediction_type

  # do the prediction
  mypredictions <- do.call(predict.glm, glm_pred_args)

  # return the prediction vector
  mypredictions
}

#' @export
tundra_logistic_regression <- function(munge_procedure = list(), default_args = list(), internal = list()) {
  tundra:::tundra_container$new('logistic_regression',
                                 tundra_logistic_regression_train_fn,
                                 tundra_logistic_regression_predict_fn,
                                 munge_procedure,
                                 default_args,
                                 internal)
}
