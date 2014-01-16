#' Tundra ensemble wrapper
tundra_ensemble_train_fn <- function(dataframe) {
  stopifnot('submodels' %in% names(inputs) && 'master' %in% names(inputs))

  # TODO: Cross-validation on master learner
  output$submodels <<- lapply(inputs$submodels, function(model_parameters) {
    stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
    if (!exists(model_fn <- paste0('tundra_', model_parameters[[1]])))
      stop("Missing tundra container for keyword '", model_parameters[[1]], "'")
    model <- get(model_fn)(model_parameters$data,
      model_parameters[setdiff(which(names(model_parameters) != 'data'), 1)])
    model$train(dataframe)
    model
  })

  # Train meta-learner
  meta_dataframe <- data.frame(lapply(output$submodels, function(model) {
    model$predict(dataframe[which(colnames(dataframe)) != 'dep_var'])
  }))
  colnames(meta_dataframe) <- as.character(seq_along(meta_dataframe))
  
  # TODO: Dry this
  model_parameters <- inputs$master
  stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
  if (!exists(model_fn <- paste0('tundra_', model_parameters[[1]])))
    stop("Missing tundra container for keyword '", model_parameters[[1]], "'")
  master_model <- get(model_fn)(model_parameters$data,
    model_parameters[setdiff(which(names(model_parameters) != 'data'), 1)])
  master_model$train(meta_dataframe)

  output$master <- master_model

  invisible("ensemble")
}

tundra_ensemble_predict_fn <- function(dataframe, predicts_args) {
# TODO
}

#' @export
tundra_ensemble <- function(munge_procedure = list(), default_args = list()) {
  tundra_container$new('ensemble',
                       tundra_ensemble_train_fn,
                       tundra_ensemble_predict_fn,
                       munge_procedure,
                       default_args)
}

