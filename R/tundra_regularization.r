#' Tundra regularization wrapper
#

tundra_regularization_train_fn <- function(dataframe) {
  # cat("Training Regularized Logistic Regression model...\n")
  library(glmnet)
  regularization_args <- list()
  indep_vars <- setdiff(colnames(dataframe), 'dep_var')
  stopifnot(length(indep_vars) > 0)    
  
  defaults <- list(family       ="binomial", 
                   alpha        = 0.5,
                   nfolds       = 10,
                   standardize  = FALSE, 
                   type.measure = "deviance"
                   #penalty      = 1
              )
  # alpha : 
  # The elasticnet mixing parameter, with 0<=alpha<=1. The penalty is defined as
  # (1-alpha)/[2*(beta)_2_norm^2] + alpha*beta_1_norm
  # alpha = 1   <=> Lasso penalty
  # alpha = 0.5 <=> Elastic Net penalty
  # alpha = 0   <=> Ridge penalty

  lapply(names(defaults), function(name) input[[name]] <<- input[[name]] %||% defaults[[name]])

  regularization_args$x <- model.matrix(as.formula(paste('~', paste(indep_vars, collapse='+'))),
                                        data = dataframe[, indep_vars])
  regularization_args$y <- as.matrix(dataframe[, 'dep_var'])
  regularization_args$family <- input$family
  regularization_args$alpha  <- input$alpha
  regularization_args$nfolds  <- input$nfolds
  regularization_args$standardize  <- input$standardize
  regularization_args$type.measure  <- input$type.measure

  #output <<- list(model = do.call(cv.glmnet, regularization_args), indep_vars = indep_vars)
  output <<- list(model = cv.glmnet( x = regularization_args$x, 
                        y = regularization_args$y, 
                        family = regularization_args$family, 
                        alpha = regularization_args$alpha, 
                        nfolds = regularization_args$nfolds, 
                        standardize =  regularization_args$standardize, 
                        type.measure =  regularization_args$type.measure),
                  indep_vars = indep_vars)
  if (!is.null(input$prediction_type))
    output$prediction_type <<- input$prediction_type
  
  invisible("regularization")
}

tundra_regularization_predict_fn <- function(dataframe, predict_args = list()) {
  require(glmnet)
  
  type <- if (is.null(predict_args$prediction_type)) output$prediction_type
          else predict_args$prediction_type
  
  # s specified, check if cached
  perf_method <- predict_args$penalty %||% input$penalty %||% output$model$lambda.1se
  
  predict(object = output$model$glmnet.fit,
          newx = 
            model.matrix(as.formula(paste('~', paste(output$indep_vars, collapse = '+'))),
                         dataframe[, output$indep_vars]),
          s = perf_method, type = type)[, 1]
}

#' @export
tundra_regularization <- function(munge_procedure = list(), default_args = list(), internal = list()) {
  tundra:::tundra_container$new('regularization',
                       tundra_regularization_train_fn,
                       tundra_regularization_predict_fn,
                       munge_procedure,
                       default_args,
                       internal)
}
