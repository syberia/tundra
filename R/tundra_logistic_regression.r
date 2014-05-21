#' Tundra logistic regression wrapper
#

tundra_logistic_regression_train_fn <- function(dataframe) {

  cat("Training Logistic Regression model (unregularized)...\n")`

  require(glm)

  # get the list of predictors
  indep_vars <- setdiff(colnames(dataframe), 'dep_var')
  stopifnot(length(indep_vars) > 0)    
 
  # default parameters for the glm 
  defaults <- list(family = "binomial",
                   prediction_type = "response")

  # set glm parameters based on user input; otherwise use defaults
  lapply(names(defaults), function(name) input[[name]] <<- input[[name]] %||% defaults[[name]])

  # train the glm
  model <- glm(dep_var ~ ., family = inputs$family)

  # output model object and other info
  output <<- list(model = model, 
                  indep_vars = indep_vars)

  # copy prediction type to output list 
  if (!is.null(input$prediction_type))
     output$prediction_type <<- input$prediction_type
  
  # don't know what this does
  invisible("logistic_regression")
}

tundra_logistic_regression_predict_fn <- function(dataframe, predict_args = list()) {

  require(glm)
  
  # don't really understand this
  type <- if (is.null(predict_args$prediction_type)) output$prediction_type
          else predict_args$prediction_type

  # get the prediction data
  model.formula <- as.formula(paste('~', paste(output$indep_vars, collapse = '+'))) 
  pred.data <- model.matrix(model.formula, dataframe[, output$indep_vars])

  # do the prediction
  mypredictions <- predict(object = output$model,
                           newdata =  pred.data)

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
