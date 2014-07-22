#' Tundra Random Forest wrapper
#
tundra_rf_train_fn <- function(dataframe) {
  # cat("Training Random Forest model...\n")
  require(party)
  
  rf_args <- list()
  indep_vars <- setdiff(colnames(dataframe), 'dep_var')
  stopifnot(length(indep_vars) > 0)
  
  rf_args[[1]] <- as.formula(paste('dep_var ~ `',
                                      paste(indep_vars, collapse = "` + `"),
                                      '`', sep = ''))  
  rf_args$data <- dataframe
  input$branches <- input$branches %||% round(sqrt(length(dataframe)))
  rf_args$controls <- cforest_unbiased(ntree = input$trees, mtry = input$branches)
  
  output <<- list(model = do.call(cforest, rf_args))
  
  if (!is.null(input$prediction_type))
    output$prediction_type <<- input$prediction_type
  invisible("random_forest")
}

tundra_rf_predict_fn <- function(dataframe, predict_args = list()) {
#  dataframe$loan_id  # trick: when one is not sure what is going on with new data pre-proccessing procedures or new models, just return loan_ids 
  if (is.null(input$OOB) && is.null(predict_args$OOB))
   stop("No Random Forest performance method specified: must specify OOB") 
  
  require(party)
  
  type <- if (is.null(predict_args$prediction_type)) output$prediction_type
  else predict_args$prediction_type
  
  # Perf method specified, check if cached
  OOB <- if (is.null(predict_args$perf_method)) input$OOB
  else predict_args$OOB
  preds <- predict(object = output$model, newdata = dataframe,
           type = type, OOB = OOB)
  predsdf <- Reduce(rbind, preds)
  if (ncol(predsdf) > 1) predsdf[, grep("1$", colnames(preds[[1]]))]
  else predsdf[, 1]
}

#' @export
tundra_random_forest <- function(munge_procedure = list(), default_args = list(), internal = list()) {
  tundra:::tundra_container$new('random_forest',
                                tundra_rf_train_fn,
                                tundra_rf_predict_fn,
                                munge_procedure,
                                default_args,
                                internal)
}

