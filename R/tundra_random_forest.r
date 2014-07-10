#' Tundra Random Forest wrapper
#
tundra_rf_train_fn <- function(dataframe) {

  cat("Training Random Forest model...\n")
  
  require(party)
  require(survival)
  
  # defaults
  input$distribution <<- input$distribution %||% "any"    
  
  # get argument list for random forests
  rf_args <- list()
  
  if(input$distribution == "coxph") {
    indep_vars <- setdiff(colnames(dataframe), c('dep_var', 'surv_time'))
    stopifnot(length(indep_vars) > 0)
    
    rf_args$data <- within(dataframe, surv.. <- Surv(surv_time, dep_var))
    
    rf_args$formula <- as.formula(paste('surv.. ~ `',
                                        paste(setdiff(indep_vars, 'surv..'), collapse = "` + `"),
                                        '`', sep = ''))
  } else {
    
    indep_vars <- setdiff(colnames(dataframe), 'dep_var')
    stopifnot(length(indep_vars) > 0)
    
    rf_args$formula <- as.formula(paste('dep_var ~ `',
                                        paste(indep_vars, collapse = "` + `"),
                                        '`', sep = ''))
    
    rf_args$data <- dataframe
  }  
  
  rf_args$controls <- cforest_unbiased(input$trees, input$branches)
  
  #set.seed(input$seed %||% 100)
  output <<- list(model = do.call(cforest, rf_args)) 
 
  if (!is.null(input$prediction_type))
    output$prediction_type <<- input$prediction_type
 
  invisible("random_forest")
}

tundra_rf_predict_fn <- function(dataframe, predict_args = list()) {
  
  if (is.null(input$OOB) && is.null(predict_args$OOB))
    stop("No Random Forest performance method specified: must specify OOB") 
  
  require(party)
  
  type <- if (is.null(predict_args$prediction_type)) output$prediction_type
  else predict_args$prediction_type
  
  # Perf method specified, check if cached
  OOB <- if (is.null(predict_args$perf_method)) input$OOB
  else predict_args$OOB

  preds <- predict(object = output$model, newdata = dataframe, type = type, OOB = OOB)
  
  if(input$distribution == "coxph"){
    preds
   } else { 
    Reduce(rbind, preds)[,1]
  }

}

#' @export
tundra_random_forest <- function(munge_procedure = list(), default_args = list(), internal = list() ) {
  tundra:::tundra_container$new('random_forest',
                                tundra_rf_train_fn,
                                tundra_rf_predict_fn,
                                munge_procedure,
                                default_args,
                                internal )
}

