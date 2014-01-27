#' Tundra GBM wrapper
#
create_design_matrix <- function(dataframe, indep_vars = colnames(dataframe)) {
  model.matrix(as.formula(paste('~', paste(indep_vars, collapse = '+'))), dataframe[, indep_vars])
}

tundra_regularization_train_fn <- function(dataframe) {
  cat("Training Regularized Logistic Regression model...\n")
  require(glmnet)
  
  regularization_args <- list()
  indep_vars <- setdiff(colnames(dataframe), 'dep_var')
  stopifnot(length(indep_vars) > 0)    
  
  defaults <- list(family       ="binomial", 
                   alpha        = 0,
                   nfolds       = 10,
                   standardize  = FALSE, 
                   type.measure = "deviance",
                   penalty      = 1
              )
  # alpha : 
  # The elasticnet mixing parameter, with 0<=alpha<=1. The penalty is defined as
  # (1-alpha)/[2*(beta)_2_norm^2] + alpha*beta_1_norm
  # alpha = 1   <=> Lasso penality
  # alpha = 0.5 <=> Elastic Net penality
  # alpha = 0   <=> Ridge penality

  lapply(names(defaults), function(name) input[[name]] <<- input[[name]] %||% defaults[[name]])

  regularization_args$x <- as.matrix(create_design_matrix(dataframe[, indep_vars], indep_vars))
  regularization_args$y <- as.matrix(dataframe[, 'dep_var'])
  regularization_args$family <- input$family
  regularization_args$alpha  <- input$alpha
  regularization_args$nfolds  <- input$nfolds
  regularization_args$standardize  <- input$standardize
  regularization_args$type.measure  <- input$type.measure


  # regularization_args <- append(regularization_args,
  #                        list(distribution = input$distribution
  #                        ))

  output <<- list(model = do.call(cv.glmnet, regularization_args), indep_vars = indep_vars)
  
  if (!is.null(input$prediction_type))
    output$prediction_type <<- input$prediction_type
  
  invisible("regularization")
}

tundra_regularization_predict_fn <- function(dataframe, predict_args) {
  require(glmnet)
  
  type <- if (is.null(predict_args$prediction_type)) output$prediction_type
          else predict_args$prediction_type
  
  # s specified, check if cached
  perf_method <- predict_args$penalty %||% input$penalty %||% output$model$lambda.1se
  
  predict.glmnet(object = output$model,
                 newx = as.matrix(create_design_matrix(dataframe[, output$indep_vars])),
                 output$penalty[[perf_method]], type = type)
}

#' @export
tundra_regularization <- function(munge_procedure = list(), default_args = list()) {
  tundra:::tundra_container$new('regularization',
                       tundra_regularization_train_fn,
                       tundra_regularization_predict_fn,
                       munge_procedure,
                       default_args)
}
